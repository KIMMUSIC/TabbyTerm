#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ParseState {
    Ground,
    Escape,
    Csi,
    Osc,
    OscEscape,
}

pub struct MinimalVtParser {
    state: ParseState,
    current_line: String,
    pending_utf8: Vec<u8>,
    saw_cr: bool,
}

impl Default for MinimalVtParser {
    fn default() -> Self {
        Self {
            state: ParseState::Ground,
            current_line: String::new(),
            pending_utf8: Vec::new(),
            saw_cr: false,
        }
    }
}

impl MinimalVtParser {
    pub fn feed(&mut self, chunk: &[u8]) -> Vec<String> {
        let mut completed = Vec::new();

        for &byte in chunk {
            match self.state {
                ParseState::Ground => match byte {
                    0x1B => {
                        self.flush_pending_utf8();
                        self.state = ParseState::Escape;
                    }
                    b'\r' => {
                        self.flush_pending_utf8();
                        self.saw_cr = true;
                    }
                    b'\n' => {
                        self.flush_pending_utf8();
                        self.saw_cr = false;
                        completed.push(std::mem::take(&mut self.current_line));
                    }
                    0x08 => {
                        self.flush_pending_utf8();
                        self.saw_cr = false;
                        self.current_line.pop();
                    }
                    b'\t' => {
                        self.flush_pending_utf8();
                        if self.saw_cr {
                            self.current_line.clear();
                            self.saw_cr = false;
                        }
                        self.saw_cr = false;
                        self.current_line.push_str("    ");
                    }
                    b if !b.is_ascii_control() => {
                        if self.saw_cr {
                            // CR without LF means cursor returned to column 0.
                            // Approximate this by replacing the current line content.
                            self.current_line.clear();
                        }
                        self.saw_cr = false;
                        self.pending_utf8.push(b);
                        self.flush_pending_utf8();
                    }
                    _ => {}
                },
                ParseState::Escape => match byte {
                    b'[' => self.state = ParseState::Csi,
                    b']' => self.state = ParseState::Osc,
                    _ => self.state = ParseState::Ground,
                },
                ParseState::Csi => {
                    if (0x40..=0x7E).contains(&byte) {
                        self.state = ParseState::Ground;
                    }
                }
                ParseState::Osc => match byte {
                    0x07 => self.state = ParseState::Ground,
                    0x1B => self.state = ParseState::OscEscape,
                    _ => {}
                },
                ParseState::OscEscape => match byte {
                    b'\\' => self.state = ParseState::Ground,
                    _ => self.state = ParseState::Osc,
                },
            }
        }

        self.flush_pending_utf8();
        completed
    }

    pub fn current_line(&self) -> &str {
        &self.current_line
    }

    fn flush_pending_utf8(&mut self) {
        while !self.pending_utf8.is_empty() {
            match std::str::from_utf8(&self.pending_utf8) {
                Ok(text) => {
                    self.current_line.push_str(text);
                    self.pending_utf8.clear();
                    break;
                }
                Err(err) => {
                    let valid_up_to = err.valid_up_to();
                    if valid_up_to > 0 {
                        let valid = std::str::from_utf8(&self.pending_utf8[..valid_up_to])
                            .expect("valid prefix must decode");
                        self.current_line.push_str(valid);
                        self.pending_utf8.drain(..valid_up_to);
                        continue;
                    }

                    match err.error_len() {
                        Some(error_len) => {
                            self.current_line.push('\u{FFFD}');
                            self.pending_utf8.drain(..error_len);
                        }
                        None => break,
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::MinimalVtParser;

    #[test]
    fn strips_ansi_sequences() {
        let mut parser = MinimalVtParser::default();
        let lines = parser.feed(b"\x1b[31mhello\x1b[0m\n");
        assert_eq!(lines, vec!["hello"]);
    }

    #[test]
    fn handles_crlf_without_dropping_line_content() {
        let mut parser = MinimalVtParser::default();
        let lines = parser.feed(b"alpha\r\nbeta\r\n");
        assert_eq!(lines, vec!["alpha", "beta"]);
    }

    #[test]
    fn carriage_return_rewrites_the_same_line() {
        let mut parser = MinimalVtParser::default();
        let lines = parser.feed(b"first\rsecond\n");
        assert_eq!(lines, vec!["second"]);
    }

    #[test]
    fn parses_utf8_text() {
        let mut parser = MinimalVtParser::default();
        let lines = parser.feed("한글 출력\n".as_bytes());
        assert_eq!(lines, vec!["한글 출력"]);
    }

    #[test]
    fn keeps_utf8_sequences_split_across_chunks() {
        let mut parser = MinimalVtParser::default();
        let text = "가\n".as_bytes();
        let lines = parser.feed(&text[..1]);
        assert!(lines.is_empty());
        let lines = parser.feed(&text[1..]);
        assert_eq!(lines, vec!["가"]);
    }
}
