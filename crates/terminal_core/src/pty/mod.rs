use std::io::{Read, Write};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use anyhow::{Context, Result, anyhow};
use crossbeam_channel::{Receiver, unbounded};
use portable_pty::{Child, CommandBuilder, MasterPty, PtySize, native_pty_system};

pub struct PtySession {
    master: Box<dyn MasterPty + Send>,
    child: Box<dyn Child + Send>,
    writer: Arc<Mutex<Box<dyn Write + Send>>>,
    receiver: Receiver<Vec<u8>>,
}

impl PtySession {
    pub fn spawn_powershell(cols: u16, rows: u16) -> Result<Self> {
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .context("failed to open PTY pair")?;

        let mut cmd = CommandBuilder::new("powershell.exe");
        cmd.arg("-NoLogo");
        cmd.arg("-NoExit");
        cmd.arg("-Command");
        cmd.arg(
            "$OutputEncoding=[System.Text.Encoding]::UTF8; \
[Console]::InputEncoding=[System.Text.Encoding]::UTF8; \
[Console]::OutputEncoding=[System.Text.Encoding]::UTF8; \
chcp.com 65001 > $null",
        );
        if let Ok(cwd) = std::env::current_dir() {
            cmd.cwd(cwd);
        }

        let child = pair
            .slave
            .spawn_command(cmd)
            .context("failed to spawn powershell in PTY")?;

        let mut reader = pair
            .master
            .try_clone_reader()
            .context("failed to clone PTY reader")?;
        let writer = pair
            .master
            .take_writer()
            .context("failed to take PTY writer")?;
        let writer = Arc::new(Mutex::new(writer));
        let reader_writer = Arc::clone(&writer);

        let (sender, receiver) = unbounded();
        thread::Builder::new()
            .name("pty-reader".to_owned())
            .spawn(move || {
                let mut buf = [0_u8; 4096];
                loop {
                    match reader.read(&mut buf) {
                        Ok(0) => break,
                        Ok(n) => {
                            respond_to_terminal_queries(&buf[..n], &reader_writer);
                            if sender.send(buf[..n].to_vec()).is_err() {
                                break;
                            }
                        }
                        Err(err) if err.kind() == std::io::ErrorKind::Interrupted => continue,
                        Err(err)
                            if matches!(
                                err.kind(),
                                std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                            ) =>
                        {
                            thread::sleep(Duration::from_millis(8));
                            continue;
                        }
                        Err(_) => break,
                    }
                }
            })
            .context("failed to spawn PTY reader thread")?;

        Ok(Self {
            master: pair.master,
            child,
            writer,
            receiver,
        })
    }

    pub fn try_read_chunk(&self) -> Option<Vec<u8>> {
        self.receiver.try_recv().ok()
    }

    pub fn write_input(&self, input: &str) -> Result<()> {
        let mut writer = self
            .writer
            .lock()
            .map_err(|_| anyhow!("failed to lock PTY writer"))?;
        writer
            .write_all(input.as_bytes())
            .context("failed writing to PTY")?;
        writer.flush().context("failed flushing PTY writer")?;
        Ok(())
    }

    pub fn resize(&self, cols: u16, rows: u16) -> Result<()> {
        self.master
            .resize(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .context("failed to resize PTY")?;
        Ok(())
    }

    pub fn process_id(&mut self) -> Option<u32> {
        self.child.process_id()
    }
}

fn respond_to_terminal_queries(chunk: &[u8], writer: &Arc<Mutex<Box<dyn Write + Send>>>) {
    let has_dsr =
        chunk.windows(4).any(|w| w == b"\x1b[6n") || chunk.windows(5).any(|w| w == b"\x1b[?6n");

    if !has_dsr {
        return;
    }

    if let Ok(mut w) = writer.lock() {
        let _ = w.write_all(b"\x1b[1;1R");
        let _ = w.flush();
    }
}

impl Drop for PtySession {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

#[cfg(test)]
mod tests {
    use super::PtySession;
    use std::time::{Duration, Instant};

    #[cfg(windows)]
    #[test]
    fn powershell_pty_roundtrip_emits_output() {
        let session = PtySession::spawn_powershell(120, 40).expect("pty spawn should succeed");
        session
            .write_input("Write-Output '__PTY_OK__'\r\n")
            .expect("pty write should succeed");

        let deadline = Instant::now() + Duration::from_secs(8);
        let mut combined = Vec::new();
        while Instant::now() < deadline {
            if let Some(chunk) = session.try_read_chunk() {
                combined.extend(chunk);
                if String::from_utf8_lossy(&combined).contains("__PTY_OK__") {
                    return;
                }
            } else {
                std::thread::sleep(Duration::from_millis(20));
            }
        }

        let output = String::from_utf8_lossy(&combined).to_string();
        panic!("did not receive expected marker from powershell PTY; output={output}");
    }

    #[cfg(windows)]
    #[test]
    fn powershell_pty_roundtrip_emits_korean_output() {
        let session = PtySession::spawn_powershell(120, 40).expect("pty spawn should succeed");
        session
            .write_input("$text='한글테스트'; Write-Output $text\r\n")
            .expect("pty write should succeed");

        let deadline = Instant::now() + Duration::from_secs(8);
        let mut combined = Vec::new();
        while Instant::now() < deadline {
            if let Some(chunk) = session.try_read_chunk() {
                combined.extend(chunk);
                if String::from_utf8_lossy(&combined).contains("한글테스트") {
                    return;
                }
            } else {
                std::thread::sleep(Duration::from_millis(20));
            }
        }

        let output = String::from_utf8_lossy(&combined).to_string();
        panic!("did not receive expected korean text from powershell PTY; output={output}");
    }
}
