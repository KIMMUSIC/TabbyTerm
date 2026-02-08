pub mod ai_panel;
pub mod config;
pub mod palette;
pub mod panes;
pub mod sidebar;
pub mod tabs;
pub mod theme;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs;
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Result, anyhow};
use crossbeam_channel::{Receiver, Sender, unbounded};
use eframe::egui;
use serde::{Deserialize, Serialize};
use terminal_core::pty::PtySession;
use terminal_core::vt_parser::MinimalVtParser;
use ux_model::ai::{AiBlock, AiBlockStatus, AiTool};
use ux_model::blocks::CommandBlock;
use ux_model::session::{SessionSnapshot, SessionState, TimelineItem};

use crate::ai_panel::{AiPanelAction, AiPanelState};
use crate::config::{
    AppConfig, ResolvedAiCommand, deserialize as config_deserialize,
    serialize_pretty as config_serialize_pretty,
};
use crate::palette::PaletteAction;
use crate::palette::PaletteState;
use crate::panes::PaneGridState;
use crate::panes::PaneLayout;
use crate::sidebar::{SidebarAction, SidebarState};
use crate::tabs::TabAction;
use crate::tabs::TabState;

enum AiRunEvent {
    OutputChunk {
        tab_id: u64,
        ai_block_id: u64,
        lines: Vec<String>,
    },
    Completed {
        tab_id: u64,
        ai_block_id: u64,
        exit_code: i32,
        duration_ms: u64,
    },
    Failed {
        tab_id: u64,
        ai_block_id: u64,
        message: String,
        duration_ms: u64,
    },
}

#[derive(Debug, Clone)]
struct FileLineRef {
    path: PathBuf,
    line: u32,
    column: Option<u32>,
}

struct TabRuntime {
    pty: PtySession,
    parser: MinimalVtParser,
    session: SessionState,
    pane_grid: PaneGridState,
    input_buffer: String,
    input_history_cursor: Option<usize>,
    block_search_query: String,
    bookmarks_only: bool,
    selected_context_block_ids: BTreeSet<u64>,
    export_message: String,
    ai_status_line: String,
    running_ai_jobs: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct WorkspaceSnapshot {
    format_version: u32,
    active_tab_id: u64,
    tabs: Vec<SavedTabSnapshot>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SavedTabSnapshot {
    tab_id: u64,
    tab_label: String,
    session: SessionSnapshot,
}

#[derive(Debug, Clone)]
struct SavedSessionEntry {
    path: PathBuf,
    modified_unix_sec: u64,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum AppTabKind {
    Terminal,
    Editor,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum VimMode {
    Normal,
    Insert,
    Command,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum UiDensity {
    Compact,
    Comfortable,
    Spacious,
}

impl UiDensity {
    fn label(self) -> &'static str {
        match self {
            Self::Compact => "compact",
            Self::Comfortable => "comfortable",
            Self::Spacious => "spacious",
        }
    }
}

struct EditorTabState {
    file_path: PathBuf,
    lines: Vec<String>,
    cursor_line: usize,
    cursor_col: usize,
    mode: VimMode,
    command_buffer: String,
    status_line: String,
    pending_normal_key: Option<char>,
    dirty: bool,
}

impl TabRuntime {
    fn with_session(session: SessionState) -> Result<(Self, u32)> {
        let mut pty = PtySession::spawn_powershell(180, 48)?;
        let pid = pty.process_id().unwrap_or_default();

        Ok((
            Self {
                pty,
                parser: MinimalVtParser::default(),
                session,
                pane_grid: PaneGridState::default(),
                input_buffer: String::new(),
                input_history_cursor: None,
                block_search_query: String::new(),
                bookmarks_only: false,
                selected_context_block_ids: BTreeSet::new(),
                export_message: String::new(),
                ai_status_line: "idle".to_owned(),
                running_ai_jobs: 0,
            },
            pid,
        ))
    }

    fn new(cwd: String) -> Result<(Self, u32)> {
        Self::with_session(SessionState::new(cwd))
    }
}

impl EditorTabState {
    fn open(path: &Path) -> Result<Self> {
        let bytes = fs::read(path)?;
        let content = String::from_utf8_lossy(&bytes).to_string();
        let mut lines: Vec<String> = content
            .split('\n')
            .map(|line| line.trim_end_matches('\r').to_owned())
            .collect();
        if lines.is_empty() {
            lines.push(String::new());
        }

        Ok(Self {
            file_path: path.to_path_buf(),
            lines,
            cursor_line: 0,
            cursor_col: 0,
            mode: VimMode::Normal,
            command_buffer: String::new(),
            status_line: "NORMAL".to_owned(),
            pending_normal_key: None,
            dirty: false,
        })
    }

    fn label(&self) -> String {
        let name = self
            .file_path
            .file_name()
            .map(|v| v.to_string_lossy().to_string())
            .unwrap_or_else(|| self.file_path.display().to_string());
        if self.dirty {
            format!("{name} [+]")
        } else {
            name
        }
    }

    fn mode_label(&self) -> &'static str {
        match self.mode {
            VimMode::Normal => "NORMAL",
            VimMode::Insert => "INSERT",
            VimMode::Command => "COMMAND",
        }
    }

    fn ensure_cursor_bounds(&mut self) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        self.cursor_line = self.cursor_line.min(self.lines.len().saturating_sub(1));
        let max_col = self.lines[self.cursor_line].chars().count();
        self.cursor_col = self.cursor_col.min(max_col);
    }

    fn move_left(&mut self) {
        if self.cursor_col > 0 {
            self.cursor_col -= 1;
        }
    }

    fn move_right(&mut self) {
        let max_col = self.lines[self.cursor_line].chars().count();
        if self.cursor_col < max_col {
            self.cursor_col += 1;
        }
    }

    fn move_up(&mut self) {
        if self.cursor_line > 0 {
            self.cursor_line -= 1;
            self.ensure_cursor_bounds();
        }
    }

    fn move_down(&mut self) {
        if self.cursor_line + 1 < self.lines.len() {
            self.cursor_line += 1;
            self.ensure_cursor_bounds();
        }
    }

    fn to_byte_idx(line: &str, char_idx: usize) -> usize {
        if char_idx == 0 {
            return 0;
        }
        line.char_indices()
            .nth(char_idx)
            .map(|(idx, _)| idx)
            .unwrap_or_else(|| line.len())
    }

    fn insert_text(&mut self, text: &str) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }

        if let Some(line) = self.lines.get_mut(self.cursor_line) {
            let byte_idx = Self::to_byte_idx(line, self.cursor_col);
            line.insert_str(byte_idx, text);
            self.cursor_col += text.chars().count();
            self.dirty = true;
        }
    }

    fn insert_newline(&mut self) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }

        let current = self.lines[self.cursor_line].clone();
        let split_at = Self::to_byte_idx(&current, self.cursor_col);
        let left = current[..split_at].to_owned();
        let right = current[split_at..].to_owned();
        self.lines[self.cursor_line] = left;
        self.lines.insert(self.cursor_line + 1, right);
        self.cursor_line += 1;
        self.cursor_col = 0;
        self.dirty = true;
    }

    fn backspace(&mut self) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
            return;
        }

        if self.cursor_col > 0 {
            if let Some(line) = self.lines.get_mut(self.cursor_line) {
                let end = Self::to_byte_idx(line, self.cursor_col);
                let start = Self::to_byte_idx(line, self.cursor_col.saturating_sub(1));
                line.replace_range(start..end, "");
                self.cursor_col = self.cursor_col.saturating_sub(1);
                self.dirty = true;
            }
            return;
        }

        if self.cursor_line > 0 {
            let current = self.lines.remove(self.cursor_line);
            self.cursor_line -= 1;
            let prev_len = self.lines[self.cursor_line].chars().count();
            self.lines[self.cursor_line].push_str(&current);
            self.cursor_col = prev_len;
            self.dirty = true;
        }
    }

    fn delete_char(&mut self) {
        if self.lines.is_empty() {
            return;
        }

        let line_len = self.lines[self.cursor_line].chars().count();
        if self.cursor_col < line_len {
            if let Some(line) = self.lines.get_mut(self.cursor_line) {
                let start = Self::to_byte_idx(line, self.cursor_col);
                let end = Self::to_byte_idx(line, self.cursor_col + 1);
                line.replace_range(start..end, "");
                self.dirty = true;
            }
            return;
        }

        if self.cursor_line + 1 < self.lines.len() {
            let next = self.lines.remove(self.cursor_line + 1);
            self.lines[self.cursor_line].push_str(&next);
            self.dirty = true;
        }
    }

    fn delete_current_line(&mut self) {
        if self.lines.is_empty() {
            return;
        }
        self.lines.remove(self.cursor_line);
        if self.lines.is_empty() {
            self.lines.push(String::new());
            self.cursor_line = 0;
            self.cursor_col = 0;
        } else {
            self.cursor_line = self.cursor_line.min(self.lines.len().saturating_sub(1));
            self.cursor_col = self
                .cursor_col
                .min(self.lines[self.cursor_line].chars().count());
        }
        self.dirty = true;
    }

    fn open_newline_below(&mut self) {
        let target = (self.cursor_line + 1).min(self.lines.len());
        self.lines.insert(target, String::new());
        self.cursor_line = target;
        self.cursor_col = 0;
        self.mode = VimMode::Insert;
        self.status_line = "-- INSERT --".to_owned();
        self.dirty = true;
    }

    fn open_newline_above(&mut self) {
        let target = self.cursor_line.min(self.lines.len());
        self.lines.insert(target, String::new());
        self.cursor_line = target;
        self.cursor_col = 0;
        self.mode = VimMode::Insert;
        self.status_line = "-- INSERT --".to_owned();
        self.dirty = true;
    }

    fn write_to_disk(&mut self) -> Result<()> {
        let mut content = self.lines.join("\n");
        if !content.ends_with('\n') {
            content.push('\n');
        }
        fs::write(&self.file_path, content)?;
        self.dirty = false;
        self.status_line = format!("written {}", self.file_path.display());
        Ok(())
    }

    fn execute_command(&mut self) -> EditorCommand {
        let command = self.command_buffer.trim();
        match command {
            "w" => {
                if let Err(err) = self.write_to_disk() {
                    self.status_line = format!("write failed: {err}");
                }
            }
            "q" => {
                if self.dirty {
                    self.status_line = "unsaved changes (use :wq or :q!)".to_owned();
                } else {
                    return EditorCommand::CloseRequested;
                }
            }
            "q!" => return EditorCommand::CloseRequested,
            "wq" | "x" => match self.write_to_disk() {
                Ok(()) => return EditorCommand::CloseRequested,
                Err(err) => {
                    self.status_line = format!("write failed: {err}");
                }
            },
            "" => {}
            _ => {
                self.status_line = format!("unknown command: :{command}");
            }
        }

        self.mode = VimMode::Normal;
        self.command_buffer.clear();
        if !self.status_line.starts_with("written") && !self.status_line.starts_with("write failed")
        {
            self.status_line = "NORMAL".to_owned();
        }
        EditorCommand::None
    }

    fn set_mode_normal(&mut self) {
        self.mode = VimMode::Normal;
        self.pending_normal_key = None;
        self.command_buffer.clear();
        self.status_line = "NORMAL".to_owned();
    }

    fn set_mode_insert(&mut self) {
        self.mode = VimMode::Insert;
        self.pending_normal_key = None;
        self.command_buffer.clear();
        self.status_line = "-- INSERT --".to_owned();
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum EditorCommand {
    None,
    CloseRequested,
}

pub fn run() -> Result<()> {
    let native_options = eframe::NativeOptions {
        renderer: eframe::Renderer::Wgpu,
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1440.0, 900.0])
            .with_decorations(false),
        ..Default::default()
    };

    eframe::run_native(
        "MyTerminal-c",
        native_options,
        Box::new(|_cc| {
            let app = TerminalApp::new()?;
            Ok(Box::new(app))
        }),
    )
    .map_err(|err| anyhow!("failed to launch eframe app: {err}"))?;

    Ok(())
}

pub struct TerminalApp {
    config: AppConfig,
    config_path: PathBuf,
    config_mtime: Option<SystemTime>,
    last_config_poll: Instant,
    workspace_root: PathBuf,
    tab_runtimes: HashMap<u64, TabRuntime>,
    tab_kinds: HashMap<u64, AppTabKind>,
    editor_tabs: HashMap<u64, EditorTabState>,
    tabs: TabState,
    sidebar: SidebarState,
    palette: PaletteState,
    ai_panel: AiPanelState,
    ui_density: UiDensity,
    sidebar_visible: bool,
    ai_panel_visible: bool,
    prompt_mascot_frames: Vec<egui::TextureHandle>,
    prompt_mascot_load_attempted: bool,
    status_text: String,
    ai_event_tx: Sender<AiRunEvent>,
    ai_event_rx: Receiver<AiRunEvent>,
    session_save_path: PathBuf,
    restore_picker_open: bool,
    saved_session_entries: Vec<SavedSessionEntry>,
    last_session_save: Instant,
    theme_applied: bool,
}

impl TerminalApp {
    fn new() -> Result<Self> {
        let (ai_event_tx, ai_event_rx) = unbounded();
        let workspace_root = std::env::current_dir()?;
        let cwd = workspace_root.display().to_string();

        let config_path = default_config_path();
        let (config, config_mtime, config_note) = load_or_init_config(&config_path)?;
        let session_save_path = PathBuf::from(&config.session.session_file);
        let session = SessionState::new(cwd);

        let tabs = TabState::default();
        let initial_tab_id = tabs.active_id();
        let (runtime, pid) = TabRuntime::with_session(session)?;
        let mut tab_runtimes = HashMap::new();
        tab_runtimes.insert(initial_tab_id, runtime);
        let mut tab_kinds = HashMap::new();
        tab_kinds.insert(initial_tab_id, AppTabKind::Terminal);

        let mut app = Self {
            config,
            config_path,
            config_mtime,
            last_config_poll: Instant::now(),
            workspace_root,
            tab_runtimes,
            tab_kinds,
            editor_tabs: HashMap::new(),
            tabs,
            sidebar: SidebarState::default(),
            palette: PaletteState::default(),
            ai_panel: AiPanelState::default(),
            ui_density: UiDensity::Comfortable,
            sidebar_visible: true,
            ai_panel_visible: true,
            prompt_mascot_frames: Vec::new(),
            prompt_mascot_load_attempted: false,
            status_text: format!(
                "PTY attached (pid={pid}) | {config_note} | auto-restore off (use restore session)"
            ),
            ai_event_tx,
            ai_event_rx,
            session_save_path,
            restore_picker_open: false,
            saved_session_entries: Vec::new(),
            last_session_save: Instant::now(),
            theme_applied: false,
        };
        app.refresh_saved_session_entries();
        Ok(app)
    }

    fn active_runtime(&self) -> Option<&TabRuntime> {
        self.tab_runtimes.get(&self.tabs.active_id())
    }

    fn active_runtime_mut(&mut self) -> Option<&mut TabRuntime> {
        let tab_id = self.tabs.active_id();
        self.tab_runtimes.get_mut(&tab_id)
    }

    fn active_tab_kind(&self) -> AppTabKind {
        self.tab_kinds
            .get(&self.tabs.active_id())
            .copied()
            .unwrap_or(AppTabKind::Terminal)
    }

    fn active_editor(&self) -> Option<&EditorTabState> {
        let tab_id = self.tabs.active_id();
        self.editor_tabs.get(&tab_id)
    }

    fn ensure_tab_runtime(&mut self, tab_id: u64) {
        if self.tab_runtimes.contains_key(&tab_id) {
            return;
        }

        let cwd = self.workspace_root.display().to_string();
        match TabRuntime::new(cwd) {
            Ok((runtime, pid)) => {
                self.tab_runtimes.insert(tab_id, runtime);
                self.tab_kinds.insert(tab_id, AppTabKind::Terminal);
                self.status_text = format!("opened {} (pid={pid})", self.tabs.active_label());
            }
            Err(err) => {
                self.status_text = format!(
                    "failed to open runtime for {}: {err}",
                    self.tabs.active_label()
                );
            }
        }
    }

    fn open_new_terminal_tab(&mut self) {
        let tab_id = self.tabs.add_tab();
        self.tab_kinds.insert(tab_id, AppTabKind::Terminal);
        self.ensure_tab_runtime(tab_id);
    }

    fn set_sidebar_visible(&mut self, visible: bool) {
        if self.sidebar_visible != visible {
            self.sidebar_visible = visible;
            self.status_text = if self.sidebar_visible {
                "sidebar shown".to_owned()
            } else {
                "sidebar hidden".to_owned()
            };
        }
    }

    fn toggle_sidebar_visible(&mut self) {
        let next = !self.sidebar_visible;
        self.set_sidebar_visible(next);
    }

    fn set_ai_panel_visible(&mut self, visible: bool) {
        if self.ai_panel_visible != visible {
            self.ai_panel_visible = visible;
            self.status_text = if self.ai_panel_visible {
                "ai panel shown".to_owned()
            } else {
                "ai panel hidden".to_owned()
            };
        }
    }

    fn set_ui_density(&mut self, density: UiDensity) {
        if self.ui_density != density {
            self.ui_density = density;
            self.status_text = format!("ui density: {}", density.label());
        }
    }

    fn apply_ui_density(&self, ctx: &egui::Context) {
        let mut style = (*ctx.style()).clone();
        match self.ui_density {
            UiDensity::Compact => {
                style.spacing.item_spacing = egui::vec2(6.0, 6.0);
                style.spacing.button_padding = egui::vec2(8.0, 4.0);
                style.spacing.interact_size = egui::vec2(40.0, 22.0);
            }
            UiDensity::Comfortable => {
                style.spacing.item_spacing = egui::vec2(8.0, 8.0);
                style.spacing.button_padding = egui::vec2(10.0, 5.0);
                style.spacing.interact_size = egui::vec2(48.0, 24.0);
            }
            UiDensity::Spacious => {
                style.spacing.item_spacing = egui::vec2(10.0, 10.0);
                style.spacing.button_padding = egui::vec2(12.0, 6.0);
                style.spacing.interact_size = egui::vec2(56.0, 28.0);
            }
        }
        ctx.set_style(style);
    }

    fn stage_command_input(&mut self, command: String, source: &str) {
        let staged = if let Some(runtime) = self.active_runtime_mut() {
            runtime.input_buffer = command.clone();
            runtime.input_history_cursor = None;
            true
        } else {
            false
        };

        self.palette.set_query(command);
        if staged {
            self.status_text = format!("{source} (tab: {})", self.tabs.active_label());
        }
    }

    fn prompt_mascot_candidate_dirs(&self) -> Vec<PathBuf> {
        let root = self.workspace_root.join("assets").join("mascot");
        vec![root.join("gaming-cat"), root.join("party-parrot")]
    }

    fn parse_frame_index(path: &Path) -> u32 {
        let stem = path
            .file_stem()
            .and_then(|v| v.to_str())
            .unwrap_or_default()
            .to_owned();
        stem.rsplit('-')
            .next()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(u32::MAX)
    }

    fn load_prompt_mascot_frames(
        ctx: &egui::Context,
        dir: &Path,
    ) -> Result<Vec<egui::TextureHandle>> {
        let mut frame_paths = fs::read_dir(dir)?
            .flatten()
            .map(|entry| entry.path())
            .filter(|path| path.is_file())
            .filter(|path| {
                path.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext.eq_ignore_ascii_case("png"))
                    .unwrap_or(false)
            })
            .filter(|path| {
                path.file_name()
                    .and_then(|name| name.to_str())
                    .map(|name| !name.starts_with("._"))
                    .unwrap_or(false)
            })
            .collect::<Vec<_>>();

        frame_paths.sort_by(|a, b| {
            let ai = Self::parse_frame_index(a);
            let bi = Self::parse_frame_index(b);
            ai.cmp(&bi).then_with(|| a.cmp(b))
        });

        let mut frames = Vec::new();
        for path in frame_paths {
            let bytes = match fs::read(&path) {
                Ok(bytes) => bytes,
                Err(_) => continue,
            };
            let decoded = match image::load_from_memory(&bytes) {
                Ok(img) => img,
                Err(_) => continue,
            };
            let rgba = decoded.to_rgba8();
            let size = [rgba.width() as usize, rgba.height() as usize];
            let pixels = rgba.into_vec();
            let color_image = egui::ColorImage::from_rgba_unmultiplied(size, &pixels);
            let texture_name = format!("prompt-mascot:{}", path.display());
            let texture =
                ctx.load_texture(texture_name, color_image, egui::TextureOptions::NEAREST);
            frames.push(texture);
        }

        Ok(frames)
    }

    fn ensure_prompt_mascot_loaded(&mut self, ctx: &egui::Context) {
        if self.prompt_mascot_load_attempted || !self.prompt_mascot_frames.is_empty() {
            return;
        }
        self.prompt_mascot_load_attempted = true;

        for dir in self.prompt_mascot_candidate_dirs() {
            if !dir.exists() {
                continue;
            }
            match Self::load_prompt_mascot_frames(ctx, &dir) {
                Ok(frames) if !frames.is_empty() => {
                    let frame_count = frames.len();
                    self.prompt_mascot_frames = frames;
                    self.status_text = format!(
                        "loaded mascot animation: {frame_count} frames ({})",
                        dir.display()
                    );
                    break;
                }
                _ => {}
            }
        }
    }

    fn run_cat_frame(&self, time_sec: f64) -> &'static str {
        const FRAMES: [&str; 6] = [
            "(=^.^=)/_ ",
            "(=^.^=)_\\ ",
            "(=^.^=)/_ .",
            "(=^.^=)_\\ ..",
            "(=^.^=)/_ ...",
            "(=^.^=)_\\ ..",
        ];
        let idx = ((time_sec * 8.0) as usize) % FRAMES.len();
        FRAMES[idx]
    }

    fn open_file_in_editor_tab(&mut self, path: PathBuf) {
        let canonical = path.canonicalize().unwrap_or(path.clone());
        let normalized = normalize_opened_path(canonical);

        if let Some((tab_id, _)) = self
            .editor_tabs
            .iter()
            .find(|(_, editor)| editor.file_path == normalized)
        {
            let _ = self.tabs.set_active_by_id(*tab_id);
            self.status_text = format!("focused editor: {}", display_path(&normalized));
            return;
        }

        let editor = match EditorTabState::open(&normalized) {
            Ok(editor) => editor,
            Err(err) => {
                self.status_text =
                    format!("failed to open file {}: {err}", display_path(&normalized));
                return;
            }
        };

        let label = editor.label();
        let tab_id = self.tabs.add_tab_with_label(label);
        self.editor_tabs.insert(tab_id, editor);
        self.tab_kinds.insert(tab_id, AppTabKind::Editor);
        self.status_text = format!("opened editor: {}", display_path(&normalized));
    }

    fn close_tab_if_possible(&mut self, tab_id: u64) {
        let entries = self.tabs.entries();
        if entries.len() <= 1 {
            self.status_text = "cannot close the last tab".to_owned();
            return;
        }

        let Some((_, closed_label)) = entries.iter().find(|(id, _)| *id == tab_id) else {
            return;
        };
        let closed_label = closed_label.clone();

        self.tab_runtimes.remove(&tab_id);
        self.editor_tabs.remove(&tab_id);
        self.tab_kinds.remove(&tab_id);

        let active_before = self.tabs.active_id();
        let remaining = entries
            .into_iter()
            .filter(|(id, _)| *id != tab_id)
            .collect::<Vec<_>>();
        let fallback_active = if active_before != tab_id {
            active_before
        } else {
            remaining.first().map(|(id, _)| *id).unwrap_or(0)
        };
        self.tabs.replace_tabs(remaining, fallback_active);

        if self.active_tab_kind() == AppTabKind::Terminal {
            self.ensure_tab_runtime(self.tabs.active_id());
        }
        self.status_text = format!("closed tab: {closed_label}");
    }

    fn close_active_tab_if_possible(&mut self) {
        let active_id = self.tabs.active_id();
        self.close_tab_if_possible(active_id);
    }

    fn apply_sidebar_actions(&mut self, actions: Vec<SidebarAction>) {
        for action in actions {
            match action {
                SidebarAction::OpenFile(path) => self.open_file_in_editor_tab(path),
            }
        }
    }

    fn render_editor_view(&mut self, ui: &mut egui::Ui, ctx: &egui::Context) {
        let tab_id = self.tabs.active_id();
        let mut close_requested = false;
        let mut status = None;
        let mut next_label = None;

        if let Some(editor) = self.editor_tabs.get_mut(&tab_id) {
            ui.horizontal_wrapped(|ui| {
                ui.label(
                    egui::RichText::new("VIM EDITOR")
                        .monospace()
                        .strong()
                        .color(theme::TEXT_MUTED),
                );
                ui.separator();
                ui.label(
                    egui::RichText::new(display_path(&editor.file_path))
                        .monospace()
                        .color(theme::TEXT_PRIMARY),
                );
                ui.separator();
                let mode_color = match editor.mode {
                    VimMode::Normal => theme::ACCENT_BLUE,
                    VimMode::Insert => theme::SUCCESS,
                    VimMode::Command => theme::TEXT_BRIGHT,
                };
                ui.label(
                    egui::RichText::new(editor.mode_label())
                        .monospace()
                        .strong()
                        .color(mode_color),
                );
                if ui.small_button("save").clicked() {
                    if let Err(err) = editor.write_to_disk() {
                        editor.status_line = format!("write failed: {err}");
                    }
                }
            });

            let command = process_editor_events(editor, ctx);
            if command == EditorCommand::CloseRequested {
                close_requested = true;
            }

            ui.separator();
            let command_line = if editor.mode == VimMode::Command {
                format!(":{}", editor.command_buffer)
            } else {
                editor.status_line.clone()
            };
            ui.label(
                egui::RichText::new(command_line)
                    .monospace()
                    .color(theme::TEXT_BRIGHT),
            );
            ui.separator();

            egui::ScrollArea::vertical()
                .auto_shrink([false, false])
                .stick_to_bottom(false)
                .show(ui, |ui| {
                    let blink_on = ((ctx.input(|i| i.time) * 2.0).floor() as i64) % 2 == 0;
                    for (idx, line) in editor.lines.iter().enumerate() {
                        let is_cursor_line = idx == editor.cursor_line;

                        ui.horizontal_wrapped(|ui| {
                            let number = format!("{:>5}", idx + 1);
                            ui.label(
                                egui::RichText::new(number)
                                    .monospace()
                                    .color(theme::TEXT_MUTED),
                            );
                            let line_color = if is_cursor_line {
                                theme::TEXT_BRIGHT
                            } else {
                                theme::TEXT_PRIMARY
                            };
                            if is_cursor_line {
                                render_editor_line_with_cursor(
                                    ui,
                                    line,
                                    editor.cursor_col,
                                    editor.mode,
                                    blink_on,
                                    line_color,
                                );
                            } else {
                                ui.label(egui::RichText::new(line).monospace().color(line_color));
                            }
                        });
                    }
                });

            status = Some(format!(
                "editor: {} | {}",
                display_path(&editor.file_path),
                editor.mode_label()
            ));
            next_label = Some(editor.label());
        } else {
            ui.label(
                egui::RichText::new("editor tab state missing")
                    .monospace()
                    .color(theme::ERROR),
            );
        }

        if let Some(label) = next_label {
            let _ = self.tabs.set_tab_label(tab_id, label);
        }
        if let Some(s) = status {
            self.status_text = s;
        }
        if close_requested {
            self.close_active_tab_if_possible();
        }
    }

    fn build_workspace_snapshot(&self) -> WorkspaceSnapshot {
        let tabs = self
            .tabs
            .entries()
            .into_iter()
            .filter_map(|(tab_id, tab_label)| {
                let runtime = self.tab_runtimes.get(&tab_id)?;
                Some(SavedTabSnapshot {
                    tab_id,
                    tab_label,
                    session: runtime.session.to_snapshot(),
                })
            })
            .collect();

        let active_tab_id = if self.tab_runtimes.contains_key(&self.tabs.active_id()) {
            self.tabs.active_id()
        } else {
            self.tab_runtimes.keys().copied().min().unwrap_or(0)
        };

        WorkspaceSnapshot {
            format_version: 1,
            active_tab_id,
            tabs,
        }
    }

    fn session_snapshot_dir(&self) -> PathBuf {
        match self.session_save_path.parent() {
            Some(parent) => parent.join("snapshots"),
            None => PathBuf::from("state").join("snapshots"),
        }
    }

    fn refresh_saved_session_entries(&mut self) {
        self.saved_session_entries =
            list_saved_workspace_entries(&self.session_save_path, &self.session_snapshot_dir())
                .unwrap_or_default();
    }

    fn save_manual_workspace_snapshot(&mut self) {
        let snapshot = self.build_workspace_snapshot();
        let snapshot_dir = self.session_snapshot_dir();
        let unix_sec = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or_default();
        let path = snapshot_dir.join(format!("session-{unix_sec}.toml"));

        match save_workspace_snapshot_to_disk(&path, &snapshot) {
            Ok(()) => {
                self.status_text = format!("saved session snapshot: {}", path.display());
                self.refresh_saved_session_entries();
            }
            Err(err) => {
                self.status_text = format!("session snapshot save failed: {err}");
            }
        }
    }

    fn open_restore_picker(&mut self) {
        self.restore_picker_open = true;
        self.refresh_saved_session_entries();
    }

    fn restore_workspace_from_path(&mut self, path: &PathBuf) {
        let loaded = match load_workspace_snapshot_from_disk(path) {
            Ok(Some(snapshot)) => snapshot,
            Ok(None) => {
                self.status_text = format!("restore failed: file not found ({})", path.display());
                return;
            }
            Err(err) => {
                self.status_text = format!("restore failed: {err}");
                return;
            }
        };

        let mut new_entries = Vec::new();
        let mut new_runtimes = HashMap::new();
        let mut restored_tabs = 0usize;
        let mut failed_tabs = 0usize;

        for tab in loaded.tabs {
            let session = SessionState::from_snapshot(tab.session);
            match TabRuntime::with_session(session) {
                Ok((runtime, _pid)) => {
                    new_entries.push((tab.tab_id, tab.tab_label));
                    new_runtimes.insert(tab.tab_id, runtime);
                    restored_tabs += 1;
                }
                Err(_) => {
                    failed_tabs += 1;
                }
            }
        }

        if new_entries.is_empty() {
            let cwd = self.workspace_root.display().to_string();
            match TabRuntime::new(cwd) {
                Ok((runtime, _pid)) => {
                    new_entries.push((0, "main".to_owned()));
                    new_runtimes.insert(0, runtime);
                }
                Err(err) => {
                    self.status_text = format!("restore failed: could not spawn terminal ({err})");
                    return;
                }
            }
        }

        let active_id = loaded.active_tab_id;
        self.tabs.replace_tabs(new_entries, active_id);
        self.tab_runtimes = new_runtimes;
        self.tab_kinds = self
            .tabs
            .entries()
            .into_iter()
            .map(|(id, _)| (id, AppTabKind::Terminal))
            .collect();
        self.editor_tabs.clear();
        self.last_session_save = Instant::now();
        self.refresh_saved_session_entries();

        if failed_tabs > 0 {
            self.status_text = format!(
                "restored {} tabs from {} ({} tabs failed)",
                restored_tabs,
                path.display(),
                failed_tabs
            );
        } else {
            self.status_text = format!("restored {} tabs from {}", restored_tabs, path.display());
        }
    }

    fn poll_pty_output(&mut self) {
        for runtime in self.tab_runtimes.values_mut() {
            while let Some(chunk) = runtime.pty.try_read_chunk() {
                let lines = runtime.parser.feed(&chunk);
                let command = runtime
                    .session
                    .blocks()
                    .last()
                    .map(|block| block.command.as_str())
                    .unwrap_or_default();
                let lines = sanitize_shell_output_lines(lines, command);
                runtime.session.push_output_lines(lines);

                let pending = runtime.parser.current_line().to_owned();
                if should_hide_pending_line(&pending) {
                    runtime.session.set_pending_line(String::new());
                } else {
                    runtime.session.set_pending_line(pending);
                }
            }
        }
    }

    fn poll_ai_events(&mut self) {
        while let Ok(event) = self.ai_event_rx.try_recv() {
            match event {
                AiRunEvent::OutputChunk {
                    tab_id,
                    ai_block_id,
                    lines,
                } => {
                    if let Some(runtime) = self.tab_runtimes.get_mut(&tab_id) {
                        let _ = runtime.session.append_ai_output_lines(ai_block_id, &lines);
                    }
                }
                AiRunEvent::Completed {
                    tab_id,
                    ai_block_id,
                    exit_code,
                    duration_ms,
                } => {
                    if let Some(runtime) = self.tab_runtimes.get_mut(&tab_id) {
                        let _ =
                            runtime
                                .session
                                .complete_ai_block(ai_block_id, exit_code, duration_ms);
                        runtime.running_ai_jobs = runtime.running_ai_jobs.saturating_sub(1);
                        runtime.ai_status_line = format!(
                            "AI block #{ai_block_id} completed (exit={exit_code}, {duration_ms}ms)"
                        );
                        self.status_text = runtime.ai_status_line.clone();
                    }
                }
                AiRunEvent::Failed {
                    tab_id,
                    ai_block_id,
                    message,
                    duration_ms,
                } => {
                    if let Some(runtime) = self.tab_runtimes.get_mut(&tab_id) {
                        let _ = runtime.session.fail_ai_block(
                            ai_block_id,
                            message.clone(),
                            duration_ms,
                        );
                        runtime.running_ai_jobs = runtime.running_ai_jobs.saturating_sub(1);
                        runtime.ai_status_line =
                            format!("AI block #{ai_block_id} failed ({duration_ms}ms): {message}");
                        self.status_text = runtime.ai_status_line.clone();
                    }
                }
            }
        }
    }

    fn poll_config_reload(&mut self) {
        if self.last_config_poll.elapsed() < Duration::from_secs(1) {
            return;
        }
        self.last_config_poll = Instant::now();

        let metadata = match fs::metadata(&self.config_path) {
            Ok(meta) => meta,
            Err(err) => {
                self.status_text = format!("config stat failed: {err}");
                return;
            }
        };

        let modified = match metadata.modified() {
            Ok(value) => value,
            Err(err) => {
                self.status_text = format!("config modified-time failed: {err}");
                return;
            }
        };

        let changed = match self.config_mtime {
            Some(previous) => modified > previous,
            None => true,
        };

        if !changed {
            return;
        }

        match load_config_file(&self.config_path) {
            Ok(config) => {
                let old_session_path = self.session_save_path.clone();
                let new_session_path = PathBuf::from(&config.session.session_file);

                self.config = config;
                self.config_mtime = Some(modified);
                self.status_text = format!("config reloaded: {}", self.config_path.display());

                if old_session_path != new_session_path {
                    self.session_save_path = new_session_path;
                    self.persist_session_now();
                    self.refresh_saved_session_entries();
                }
            }
            Err(err) => {
                self.status_text = format!("config reload failed: {err}");
            }
        }
    }

    fn persist_session_if_needed(&mut self) {
        let interval = self.config.session.autosave_interval_sec.max(1);
        if self.last_session_save.elapsed() < Duration::from_secs(interval) {
            return;
        }

        self.persist_session_now();
    }

    fn persist_session_now(&mut self) {
        if self.session_save_path.as_os_str().is_empty() {
            self.status_text = "session save path is empty in config".to_owned();
            return;
        }

        let snapshot = self.build_workspace_snapshot();

        match save_workspace_snapshot_to_disk(&self.session_save_path, &snapshot) {
            Ok(()) => {
                self.last_session_save = Instant::now();
            }
            Err(err) => {
                self.status_text = format!("session save failed: {err}");
            }
        }
    }

    fn submit_input(&mut self) {
        let write_result = {
            let Some(runtime) = self.active_runtime_mut() else {
                return;
            };

            if runtime.input_buffer.trim().is_empty() {
                return;
            }

            let command = std::mem::take(&mut runtime.input_buffer);
            let command = command.trim_end().to_owned();
            let cwd = std::env::current_dir()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|_| ".".to_owned());

            runtime.session.start_command_block(command.clone(), cwd);
            runtime.input_history_cursor = None;
            runtime.pty.write_input(&format!("{command}\r\n"))
        };

        if let Err(err) = write_result {
            self.status_text = format!("write failed: {err}");
        }
    }

    fn navigate_history_up(&mut self) {
        let Some(runtime) = self.active_runtime_mut() else {
            return;
        };
        let history = runtime.session.history_search("", 200);
        if history.is_empty() {
            return;
        }

        let next_idx = match runtime.input_history_cursor {
            None => 0,
            Some(current) => (current + 1).min(history.len().saturating_sub(1)),
        };

        runtime.input_history_cursor = Some(next_idx);
        runtime.input_buffer = history[next_idx].clone();
    }

    fn navigate_history_down(&mut self) {
        let Some(runtime) = self.active_runtime_mut() else {
            return;
        };
        let history = runtime.session.history_search("", 200);
        if history.is_empty() {
            return;
        }

        match runtime.input_history_cursor {
            Some(0) => {
                runtime.input_history_cursor = None;
                runtime.input_buffer.clear();
            }
            Some(current) => {
                let next = current.saturating_sub(1);
                runtime.input_history_cursor = Some(next);
                runtime.input_buffer = history[next].clone();
            }
            None => {}
        }
    }

    fn set_pane_layout(&mut self, layout: PaneLayout) {
        let changed = if let Some(runtime) = self.active_runtime_mut() {
            match layout {
                PaneLayout::Single => runtime.pane_grid.set_single(),
                PaneLayout::VerticalSplit => runtime.pane_grid.split_vertical(),
                PaneLayout::HorizontalSplit => runtime.pane_grid.split_horizontal(),
            }
            true
        } else {
            false
        };

        if changed {
            let layout_label = match layout {
                PaneLayout::Single => "single",
                PaneLayout::VerticalSplit => "vertical",
                PaneLayout::HorizontalSplit => "horizontal",
            };
            self.status_text = format!(
                "pane layout: {layout_label} (tab: {})",
                self.tabs.active_label()
            );
        }
    }

    fn clear_active_timeline(&mut self, source: &str) {
        let cleared = if let Some(runtime) = self.active_runtime_mut() {
            runtime.session.clear_timeline();
            runtime.selected_context_block_ids.clear();
            runtime.block_search_query.clear();
            true
        } else {
            false
        };

        if cleared {
            self.status_text = format!("{source} (tab: {})", self.tabs.active_label());
        }
    }

    fn apply_palette_action(&mut self, action: PaletteAction) {
        match action {
            PaletteAction::ApplyCommand(command) => {
                self.stage_command_input(command, "loaded command from palette");
                self.palette.close();
            }
        }
    }

    fn apply_ai_panel_action(&mut self, action: AiPanelAction) {
        match action {
            AiPanelAction::RunPrompt { tool, prompt } => {
                self.start_ai_request(tool, prompt);
            }
        }
    }

    fn prune_context_block_selection(&mut self) {
        if let Some(runtime) = self.active_runtime_mut() {
            runtime
                .selected_context_block_ids
                .retain(|id| runtime.session.block_by_id(*id).is_some());
        }
    }

    fn start_ai_request(&mut self, tool: AiTool, prompt: String) {
        let tab_id = self.tabs.active_id();
        let (resolved, combined_prompt, ai_block_id, timeout_sec) = {
            let Some(runtime) = self.tab_runtimes.get_mut(&tab_id) else {
                self.status_text = "active tab runtime is unavailable".to_owned();
                return;
            };

            let mut context_ids: Vec<u64> =
                runtime.selected_context_block_ids.iter().copied().collect();
            context_ids.sort_unstable();
            let context_payload = runtime.session.build_context_payload(&context_ids, 120);
            let combined_prompt = if context_payload.is_empty() {
                prompt.clone()
            } else {
                format!("{prompt}\n\n{context_payload}")
            };

            let ai_block_id =
                runtime
                    .session
                    .start_ai_block(tool, prompt.clone(), context_ids.clone());
            runtime.running_ai_jobs += 1;
            runtime.ai_status_line = format!(
                "AI block #{ai_block_id} started with {} (context blocks: {})",
                tool.label(),
                context_ids.len()
            );

            (
                self.config.ai.resolve(tool, &combined_prompt),
                combined_prompt,
                ai_block_id,
                self.config.ai.timeout_sec.max(1),
            )
        };

        let tx = self.ai_event_tx.clone();
        let workspace_root = self.workspace_root.clone();
        let spawn_result = thread::Builder::new()
            .name(format!("ai-runner-{ai_block_id}"))
            .spawn(move || {
                run_ai_command(
                    resolved,
                    Some(combined_prompt),
                    tab_id,
                    ai_block_id,
                    timeout_sec,
                    workspace_root,
                    tx,
                );
            });

        if let Err(err) = spawn_result {
            if let Some(runtime) = self.tab_runtimes.get_mut(&tab_id) {
                runtime.running_ai_jobs = runtime.running_ai_jobs.saturating_sub(1);
                let _ = runtime.session.fail_ai_block(
                    ai_block_id,
                    format!("failed to spawn AI runner thread: {err}"),
                    0,
                );
                runtime.ai_status_line = format!("AI block #{ai_block_id} failed to start");
            }
        }
    }

    fn filtered_timeline_items(&self) -> Vec<TimelineItem> {
        let Some(runtime) = self.active_runtime() else {
            return Vec::new();
        };

        let query = runtime.block_search_query.trim().to_lowercase();
        let mut items = runtime.session.timeline_items();

        items.retain(|item| match item {
            TimelineItem::Command(block) => {
                if runtime.bookmarks_only && !block.bookmarked {
                    return false;
                }

                if query.is_empty() {
                    return true;
                }

                block.command.to_lowercase().contains(&query)
                    || block
                        .output_lines
                        .iter()
                        .any(|line| line.to_lowercase().contains(&query))
            }
            TimelineItem::Ai(block) => {
                if runtime.bookmarks_only {
                    return false;
                }

                if query.is_empty() {
                    return true;
                }

                block.prompt.to_lowercase().contains(&query)
                    || block
                        .output_lines
                        .iter()
                        .any(|line| line.to_lowercase().contains(&query))
            }
        });

        items
    }

    fn export_session_markdown(&mut self, bookmarks_only: bool) {
        let markdown = match self.active_runtime() {
            Some(runtime) => runtime.session.export_markdown(bookmarks_only),
            None => {
                self.status_text = "active tab runtime not available".to_owned();
                return;
            }
        };

        let export_dir = PathBuf::from("exports");
        if let Err(err) = fs::create_dir_all(&export_dir) {
            let message = format!("export failed (dir): {err}");
            if let Some(runtime) = self.active_runtime_mut() {
                runtime.export_message = message.clone();
            }
            self.status_text = message;
            return;
        }

        let unix_sec = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or_default();
        let suffix = if bookmarks_only { "bookmarks" } else { "all" };
        let path = export_dir.join(format!("session-{suffix}-{unix_sec}.md"));

        match fs::write(&path, markdown) {
            Ok(()) => {
                let message = format!("exported: {}", path.display());
                if let Some(runtime) = self.active_runtime_mut() {
                    runtime.export_message = message.clone();
                }
                self.status_text = message;
            }
            Err(err) => {
                let message = format!("export failed (write): {err}");
                if let Some(runtime) = self.active_runtime_mut() {
                    runtime.export_message = message.clone();
                }
                self.status_text = message;
            }
        }
    }

    fn render_main_menu(&mut self, ctx: &egui::Context) {
        let can_close_tab = self.tabs.entries().len() > 1;
        let has_terminal_runtime = self.active_runtime().is_some();
        let is_maximized = ctx.input(|i| i.viewport().maximized).unwrap_or(false);
        let current_layout = self
            .active_runtime()
            .map(|runtime| runtime.pane_grid.layout());
        let recent_commands = self
            .active_runtime()
            .map(|runtime| runtime.session.history_search("", 8))
            .unwrap_or_default();
        let recent_sessions: Vec<SavedSessionEntry> =
            self.saved_session_entries.iter().take(8).cloned().collect();
        let density = self.ui_density;
        let pane_label = self
            .active_runtime()
            .map(|runtime| runtime.pane_grid.label())
            .unwrap_or_else(|| "panes: 0 (single)".to_owned());
        let sidebar_toggle_label = if self.sidebar_visible {
            "Hide Sidebar  Ctrl+B"
        } else {
            "Show Sidebar  Ctrl+B"
        };
        let ai_panel_toggle_label = if self.ai_panel_visible {
            "Hide AI Panel"
        } else {
            "Show AI Panel"
        };

        egui::TopBottomPanel::top("main_menu")
            .exact_height(36.0)
            .frame(
                egui::Frame::new()
                    .fill(theme::BG_SURFACE_2)
                    .stroke(egui::Stroke::new(1.0, theme::BORDER))
                    .inner_margin(egui::Margin::symmetric(10, 6)),
            )
            .show(ctx, |ui| {
                let drag_bg = ui
                    .interact(
                        ui.max_rect(),
                        ui.id().with("main_menu_drag_bg"),
                        egui::Sense::click_and_drag(),
                    )
                    .on_hover_cursor(egui::CursorIcon::Default);
                if drag_bg.drag_started() {
                    ctx.send_viewport_cmd(egui::ViewportCommand::StartDrag);
                }
                if drag_bg.double_clicked() {
                    ctx.send_viewport_cmd(egui::ViewportCommand::Maximized(!is_maximized));
                }

                ui.horizontal(|ui| {
                    ui.menu_button("File", |ui| {
                        if ui.button("New Terminal Tab").clicked() {
                            self.open_new_terminal_tab();
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(
                                can_close_tab,
                                egui::Button::new("Close Active Tab  Ctrl+W"),
                            )
                            .clicked()
                        {
                            self.close_active_tab_if_possible();
                            ui.close_menu();
                        }
                        ui.separator();
                        if ui
                            .add_enabled(
                                has_terminal_runtime,
                                egui::Button::new("Export All Blocks"),
                            )
                            .clicked()
                        {
                            self.export_session_markdown(false);
                            ui.close_menu();
                        }
                        if ui
                            .add_enabled(
                                has_terminal_runtime,
                                egui::Button::new("Export Bookmarks"),
                            )
                            .clicked()
                        {
                            self.export_session_markdown(true);
                            ui.close_menu();
                        }
                        ui.separator();
                        if ui.button("Save Session Snapshot").clicked() {
                            self.save_manual_workspace_snapshot();
                            ui.close_menu();
                        }
                        if ui.button("Restore Session Snapshot").clicked() {
                            self.open_restore_picker();
                            ui.close_menu();
                        }
                        ui.menu_button("Recent Session Snapshots", |ui| {
                            if ui.button("Refresh Snapshot List").clicked() {
                                self.refresh_saved_session_entries();
                                ui.close_menu();
                            }
                            ui.separator();

                            if recent_sessions.is_empty() {
                                ui.label(
                                    egui::RichText::new("(no snapshots loaded)")
                                        .monospace()
                                        .color(theme::TEXT_MUTED),
                                );
                                ui.label(
                                    egui::RichText::new("open restore window to scan snapshots")
                                        .monospace()
                                        .color(theme::TEXT_MUTED),
                                );
                            } else {
                                for entry in &recent_sessions {
                                    let label = format!(
                                        "{}  (saved: {})",
                                        display_path(&entry.path),
                                        entry.modified_unix_sec
                                    );
                                    if ui
                                        .selectable_label(
                                            false,
                                            egui::RichText::new(label)
                                                .monospace()
                                                .color(theme::TEXT_PRIMARY),
                                        )
                                        .clicked()
                                    {
                                        let path = entry.path.clone();
                                        self.restore_workspace_from_path(&path);
                                        ui.close_menu();
                                    }
                                }
                            }
                        });
                    });

                    ui.menu_button("Pane", |ui| {
                        let single_label = if current_layout == Some(PaneLayout::Single) {
                            "[x] Single  Ctrl+1"
                        } else {
                            "[ ] Single  Ctrl+1"
                        };
                        if ui
                            .add_enabled(has_terminal_runtime, egui::Button::new(single_label))
                            .clicked()
                        {
                            self.set_pane_layout(PaneLayout::Single);
                            ui.close_menu();
                        }

                        let vertical_label = if current_layout == Some(PaneLayout::VerticalSplit) {
                            "[x] Vertical  Ctrl+2"
                        } else {
                            "[ ] Vertical  Ctrl+2"
                        };
                        if ui
                            .add_enabled(has_terminal_runtime, egui::Button::new(vertical_label))
                            .clicked()
                        {
                            self.set_pane_layout(PaneLayout::VerticalSplit);
                            ui.close_menu();
                        }

                        let horizontal_label =
                            if current_layout == Some(PaneLayout::HorizontalSplit) {
                                "[x] Horizontal  Ctrl+3"
                            } else {
                                "[ ] Horizontal  Ctrl+3"
                            };
                        if ui
                            .add_enabled(has_terminal_runtime, egui::Button::new(horizontal_label))
                            .clicked()
                        {
                            self.set_pane_layout(PaneLayout::HorizontalSplit);
                            ui.close_menu();
                        }

                        ui.separator();
                        if ui
                            .add_enabled(
                                has_terminal_runtime,
                                egui::Button::new(
                                    egui::RichText::new("Clear Active Pane").color(theme::ERROR),
                                ),
                            )
                            .clicked()
                        {
                            self.clear_active_timeline("cleared timeline");
                            ui.close_menu();
                        }
                    });

                    ui.menu_button("View", |ui| {
                        if ui.button(sidebar_toggle_label).clicked() {
                            self.toggle_sidebar_visible();
                            ui.close_menu();
                        }
                        if ui.button(ai_panel_toggle_label).clicked() {
                            self.set_ai_panel_visible(!self.ai_panel_visible);
                            ui.close_menu();
                        }
                        ui.separator();
                        ui.menu_button("Density", |ui| {
                            let compact_label = if density == UiDensity::Compact {
                                "[x] Compact"
                            } else {
                                "[ ] Compact"
                            };
                            if ui.button(compact_label).clicked() {
                                self.set_ui_density(UiDensity::Compact);
                                ui.close_menu();
                            }

                            let comfortable_label = if density == UiDensity::Comfortable {
                                "[x] Comfortable"
                            } else {
                                "[ ] Comfortable"
                            };
                            if ui.button(comfortable_label).clicked() {
                                self.set_ui_density(UiDensity::Comfortable);
                                ui.close_menu();
                            }

                            let spacious_label = if density == UiDensity::Spacious {
                                "[x] Spacious"
                            } else {
                                "[ ] Spacious"
                            };
                            if ui.button(spacious_label).clicked() {
                                self.set_ui_density(UiDensity::Spacious);
                                ui.close_menu();
                            }
                        });
                    });

                    ui.menu_button("Tools", |ui| {
                        if ui.button("Command Palette  Ctrl+Shift+P").clicked() {
                            self.palette.open();
                            ui.close_menu();
                        }
                        ui.separator();
                        ui.menu_button("Recent Commands", |ui| {
                            if recent_commands.is_empty() {
                                ui.label(
                                    egui::RichText::new("(no command history)")
                                        .monospace()
                                        .color(theme::TEXT_MUTED),
                                );
                            } else {
                                for command in &recent_commands {
                                    if ui
                                        .selectable_label(
                                            false,
                                            egui::RichText::new(command)
                                                .monospace()
                                                .color(theme::TEXT_PRIMARY),
                                        )
                                        .clicked()
                                    {
                                        self.stage_command_input(
                                            command.clone(),
                                            "loaded recent command into input",
                                        );
                                        ui.close_menu();
                                    }
                                }
                            }
                        });
                        ui.separator();

                        if ui
                            .add_enabled(
                                has_terminal_runtime,
                                egui::Button::new("Clear Selected AI Context"),
                            )
                            .clicked()
                        {
                            if let Some(runtime) = self.active_runtime_mut() {
                                runtime.selected_context_block_ids.clear();
                            }
                            self.status_text = "cleared selected AI context".to_owned();
                            ui.close_menu();
                        }
                    });

                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if ui
                            .small_button(egui::RichText::new("x").color(theme::ERROR))
                            .on_hover_text("close window")
                            .clicked()
                        {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                        }

                        let max_label = if is_maximized { "o" } else { "[]" };
                        if ui
                            .small_button(max_label)
                            .on_hover_text("maximize")
                            .clicked()
                        {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Maximized(!is_maximized));
                        }

                        if ui.small_button("_").on_hover_text("minimize").clicked() {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Minimized(true));
                        }

                        ui.separator();
                        ui.label(
                            egui::RichText::new(format!("tab: {}", self.tabs.active_label()))
                                .monospace()
                                .color(theme::TEXT_MUTED),
                        );
                        ui.separator();
                        ui.label(
                            egui::RichText::new(pane_label)
                                .monospace()
                                .color(theme::TEXT_MUTED),
                        );
                        ui.separator();
                        ui.label(
                            egui::RichText::new(format!("density: {}", density.label()))
                                .monospace()
                                .color(theme::TEXT_MUTED),
                        );

                        ui.separator();
                        let drag = ui
                            .allocate_response(
                                [ui.available_width().max(160.0), 20.0].into(),
                                egui::Sense::click_and_drag(),
                            )
                            .on_hover_cursor(egui::CursorIcon::Default);
                        if drag.drag_started() {
                            ctx.send_viewport_cmd(egui::ViewportCommand::StartDrag);
                        }
                        if drag.double_clicked() {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Maximized(!is_maximized));
                        }
                    });
                });
            });
    }

    fn render_panes(&mut self, ui: &mut egui::Ui) {
        let Some(layout) = self
            .active_runtime()
            .map(|runtime| runtime.pane_grid.layout())
        else {
            return;
        };

        match layout {
            PaneLayout::Single => {
                self.render_terminal_pane(ui, 0);
            }
            PaneLayout::VerticalSplit => {
                ui.columns(2, |cols| {
                    self.render_terminal_pane(&mut cols[0], 0);
                    self.render_terminal_pane(&mut cols[1], 1);
                });
            }
            PaneLayout::HorizontalSplit => {
                let available_width = ui.available_width();
                let available_height = ui.available_height();
                let half_height = ((available_height - 8.0) / 2.0).max(140.0);

                ui.allocate_ui(egui::vec2(available_width, half_height), |ui| {
                    self.render_terminal_pane(ui, 0);
                });
                ui.separator();
                ui.allocate_ui(egui::vec2(available_width, half_height), |ui| {
                    self.render_terminal_pane(ui, 1);
                });
            }
        }
    }

    fn render_terminal_pane(&mut self, ui: &mut egui::Ui, pane_idx: usize) {
        let is_active = self
            .active_runtime()
            .map(|runtime| runtime.pane_grid.active_pane() == pane_idx)
            .unwrap_or(false);
        let border_color = if is_active {
            theme::ACCENT_BLUE
        } else {
            theme::BORDER
        };
        let fill_color = if is_active {
            theme::BG_SURFACE_1
        } else {
            theme::BG_SURFACE_0
        };

        egui::Frame::new()
            .fill(fill_color)
            .stroke(egui::Stroke::new(
                if is_active { 1.5 } else { 1.0 },
                border_color,
            ))
            .corner_radius(egui::CornerRadius::same(8))
            .inner_margin(egui::Margin::same(10))
            .show(ui, |ui| {
                ui.horizontal(|ui| {
                    let pane_label = if is_active {
                        format!("pane-{} *", pane_idx + 1)
                    } else {
                        format!("pane-{}", pane_idx + 1)
                    };
                    if ui
                        .selectable_label(
                            is_active,
                            egui::RichText::new(pane_label)
                                .monospace()
                                .color(if is_active {
                                    theme::TEXT_BRIGHT
                                } else {
                                    theme::TEXT_MUTED
                                }),
                        )
                        .clicked()
                    {
                        if let Some(runtime) = self.active_runtime_mut() {
                            runtime.pane_grid.set_active_pane(pane_idx);
                        }
                    }
                    if ui
                        .small_button(egui::RichText::new("clear").color(theme::ERROR))
                        .on_hover_text("clear tab timeline")
                        .clicked()
                    {
                        if let Some(runtime) = self.active_runtime_mut() {
                            runtime.session.clear_timeline();
                            runtime.selected_context_block_ids.clear();
                            runtime.block_search_query.clear();
                        }
                        self.status_text = format!(
                            "cleared timeline from pane-{} ({})",
                            pane_idx + 1,
                            self.tabs.active_label()
                        );
                    }
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("tab: {}", self.tabs.active_label()))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                });
                ui.separator();
                self.render_block_cards(ui);
            });
    }

    fn render_block_cards(&mut self, ui: &mut egui::Ui) {
        let items = self.filtered_timeline_items();
        let start = items.len().saturating_sub(220);
        let mut delete_command_ids = Vec::new();
        let mut delete_ai_ids = Vec::new();

        egui::ScrollArea::vertical()
            .stick_to_bottom(true)
            .auto_shrink([false, false])
            .show(ui, |ui| {
                if items.is_empty() {
                    ui.label(
                        egui::RichText::new("(no blocks yet)")
                            .monospace()
                            .italics()
                            .color(theme::TEXT_MUTED),
                    );
                }

                for item in &items[start..] {
                    match item {
                        TimelineItem::Command(block) => {
                            if self.render_command_block_card(ui, block) {
                                delete_command_ids.push(block.id);
                            }
                        }
                        TimelineItem::Ai(block) => {
                            if self.render_ai_block_card(ui, block) {
                                delete_ai_ids.push(block.id);
                            }
                        }
                    }
                    ui.add_space(6.0);
                }

                let pending_line = self
                    .active_runtime()
                    .map(|runtime| runtime.session.pending_line().to_owned())
                    .unwrap_or_default();
                if !pending_line.is_empty() {
                    theme::elevated_frame()
                        .inner_margin(egui::Margin::same(6))
                        .show(ui, |ui| {
                            ui.label(
                                egui::RichText::new(pending_line)
                                    .monospace()
                                    .color(theme::TEXT_MUTED),
                            );
                        });
                }
            });

        for block_id in delete_command_ids {
            if let Some(runtime) = self.active_runtime_mut() {
                if runtime.session.remove_command_block(block_id) {
                    runtime.selected_context_block_ids.remove(&block_id);
                    self.status_text = format!("deleted command block #{block_id}");
                }
            }
        }

        for ai_block_id in delete_ai_ids {
            if let Some(runtime) = self.active_runtime_mut() {
                if runtime.session.remove_ai_block(ai_block_id) {
                    self.status_text = format!("deleted ai block #{ai_block_id}");
                }
            }
        }

        self.prune_context_block_selection();
    }

    fn render_command_block_card(&mut self, ui: &mut egui::Ui, block: &CommandBlock) -> bool {
        let is_ctx = self
            .active_runtime()
            .map(|runtime| runtime.selected_context_block_ids.contains(&block.id))
            .unwrap_or(false);
        let mut stroke_color = theme::BORDER;
        let mut delete_requested = false;
        if block.bookmarked {
            stroke_color = theme::SUCCESS;
        }
        if is_ctx {
            stroke_color = theme::ACCENT_BLUE;
        }

        egui::Frame::new()
            .fill(theme::BG_SURFACE_0)
            .stroke(egui::Stroke::new(1.0, stroke_color))
            .corner_radius(egui::CornerRadius::same(6))
            .inner_margin(egui::Margin::same(8))
            .show(ui, |ui| {
                ui.horizontal_wrapped(|ui| {
                    let bookmark_label = if block.bookmarked {
                        egui::RichText::new("bookmark:on").color(theme::SUCCESS)
                    } else {
                        egui::RichText::new("bookmark:off").color(theme::TEXT_MUTED)
                    };
                    if ui.small_button(bookmark_label).clicked() {
                        if let Some(runtime) = self.active_runtime_mut() {
                            let _ = runtime.session.toggle_bookmark(block.id);
                        }
                    }

                    let ctx_label = if is_ctx {
                        egui::RichText::new("context:on").color(theme::ACCENT_BLUE)
                    } else {
                        egui::RichText::new("context:off").color(theme::TEXT_MUTED)
                    };
                    if ui.small_button(ctx_label).clicked() {
                        if let Some(runtime) = self.active_runtime_mut() {
                            if is_ctx {
                                runtime.selected_context_block_ids.remove(&block.id);
                            } else {
                                runtime.selected_context_block_ids.insert(block.id);
                            }
                        }
                    }

                    if ui
                        .small_button(egui::RichText::new("copy").color(theme::TEXT_MUTED))
                        .clicked()
                    {
                        ui.ctx().copy_text(build_command_block_copy_text(block));
                        self.status_text =
                            format!("copied command block #{} to clipboard", block.id);
                    }
                    if ui
                        .small_button(egui::RichText::new("delete").color(theme::ERROR))
                        .clicked()
                    {
                        delete_requested = true;
                    }

                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("#{}", block.id))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(&block.command)
                            .monospace()
                            .strong()
                            .color(theme::TEXT_BRIGHT),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("cwd: {}", block.working_directory))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("ts: {}", block.timestamp_unix_ms))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                });

                ui.separator();

                let total_lines = block.output_lines.len();
                let visible_limit = 220usize;
                let output_start = total_lines.saturating_sub(visible_limit);

                if output_start > 0 {
                    ui.label(
                        egui::RichText::new(format!("... {} lines omitted", output_start))
                            .color(theme::TEXT_MUTED),
                    );
                }

                if total_lines == 0 {
                    ui.label(
                        egui::RichText::new("(no output yet)")
                            .italics()
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                } else {
                    for line in &block.output_lines[output_start..] {
                        self.render_output_line(ui, line);
                    }
                }
            });

        delete_requested
    }

    fn render_ai_block_card(&mut self, ui: &mut egui::Ui, block: &AiBlock) -> bool {
        let status_label = match block.status {
            AiBlockStatus::Running => "running",
            AiBlockStatus::Completed => "completed",
            AiBlockStatus::Failed => "failed",
        };

        let status_color = match block.status {
            AiBlockStatus::Running => theme::ACCENT_BLUE,
            AiBlockStatus::Completed => theme::SUCCESS,
            AiBlockStatus::Failed => theme::ERROR,
        };
        let mut delete_requested = false;

        egui::Frame::new()
            .fill(theme::BG_SURFACE_0)
            .stroke(egui::Stroke::new(1.0, status_color))
            .corner_radius(egui::CornerRadius::same(6))
            .inner_margin(egui::Margin::same(8))
            .show(ui, |ui| {
                ui.horizontal_wrapped(|ui| {
                    if ui
                        .small_button(egui::RichText::new("copy").color(theme::TEXT_MUTED))
                        .clicked()
                    {
                        ui.ctx().copy_text(build_ai_block_copy_text(block));
                        self.status_text = format!("copied AI block #{} to clipboard", block.id);
                    }
                    if ui
                        .small_button(egui::RichText::new("delete").color(theme::ERROR))
                        .clicked()
                    {
                        delete_requested = true;
                    }
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("[AI #{}]", block.id))
                            .monospace()
                            .color(theme::TEXT_BRIGHT),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("tool: {}", block.tool.label()))
                            .monospace()
                            .strong(),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("status: {status_label}")).color(status_color),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("exit: {:?}", block.exit_code)).monospace(),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!(
                            "ctx blocks: {}",
                            block.context_block_ids.len()
                        ))
                        .monospace(),
                    );
                });

                ui.separator();
                ui.label(
                    egui::RichText::new(format!("prompt: {}", block.prompt))
                        .monospace()
                        .color(theme::TEXT_MUTED),
                );
                ui.separator();

                if block.output_lines.is_empty() {
                    ui.label(
                        egui::RichText::new("(no AI output yet)")
                            .italics()
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                } else {
                    let start = block.output_lines.len().saturating_sub(220);
                    if start > 0 {
                        ui.label(
                            egui::RichText::new(format!("... {} lines omitted", start))
                                .color(theme::TEXT_MUTED),
                        );
                    }
                    for line in &block.output_lines[start..] {
                        self.render_output_line(ui, line);
                    }
                }
            });

        delete_requested
    }

    fn render_output_line(&mut self, ui: &mut egui::Ui, line: &str) {
        let line_color = if line.starts_with("[stderr]") {
            theme::ERROR
        } else {
            theme::TEXT_PRIMARY
        };

        if let Some(reference) = parse_first_file_line_ref(line, &self.workspace_root) {
            ui.horizontal_wrapped(|ui| {
                let mut label = format!("open {}:{}", reference.path.display(), reference.line);
                if let Some(col) = reference.column {
                    label = format!("{label}:{col}");
                }

                if ui
                    .small_button(egui::RichText::new(label).color(theme::ACCENT_BLUE))
                    .clicked()
                {
                    let open_cmd = build_editor_open_command(&reference);
                    if let Some(runtime) = self.active_runtime_mut() {
                        runtime.input_buffer = open_cmd;
                    }
                    self.status_text = format!(
                        "prepared open command for {}:{}",
                        reference.path.display(),
                        reference.line
                    );
                }
                ui.label(egui::RichText::new(line).monospace().color(line_color));
            });
            return;
        }

        ui.label(egui::RichText::new(line).monospace().color(line_color));
    }
}

impl eframe::App for TerminalApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if !self.theme_applied {
            theme::apply(ctx);
            self.theme_applied = true;
        }
        self.apply_ui_density(ctx);
        self.ensure_prompt_mascot_loaded(ctx);

        self.poll_pty_output();
        self.poll_ai_events();
        self.poll_config_reload();
        self.persist_session_if_needed();

        if ctx.input(|i| i.modifiers.ctrl && i.modifiers.shift && i.key_pressed(egui::Key::P)) {
            self.palette.open();
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::B)) {
            self.toggle_sidebar_visible();
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num1)) {
            self.set_pane_layout(PaneLayout::Single);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num2)) {
            self.set_pane_layout(PaneLayout::VerticalSplit);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num3)) {
            self.set_pane_layout(PaneLayout::HorizontalSplit);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::W)) {
            self.close_active_tab_if_possible();
        }

        self.render_main_menu(ctx);

        egui::TopBottomPanel::top("top_tabs")
            .exact_height(40.0)
            .frame(
                egui::Frame::new()
                    .fill(theme::BG_SURFACE_1)
                    .stroke(egui::Stroke::new(1.0, theme::BORDER))
                    .inner_margin(egui::Margin::symmetric(12, 8)),
            )
            .show(ctx, |ui| {
                if let Some(action) = self.tabs.show(ui) {
                    match action {
                        TabAction::AddTab => self.open_new_terminal_tab(),
                        TabAction::CloseTab(tab_id) => self.close_tab_if_possible(tab_id),
                    }
                }
                if self.active_tab_kind() == AppTabKind::Terminal {
                    self.ensure_tab_runtime(self.tabs.active_id());
                }
            });

        egui::TopBottomPanel::bottom("status_bar")
            .exact_height(28.0)
            .frame(
                egui::Frame::new()
                    .fill(theme::BG_SURFACE_1)
                    .stroke(egui::Stroke::new(1.0, theme::BORDER))
                    .inner_margin(egui::Margin::symmetric(10, 4)),
            )
            .show(ctx, |ui| {
                let (cmd_count, ai_count, bookmark_count, pane_label) =
                    if let Some(runtime) = self.active_runtime() {
                        (
                            runtime.session.block_count(),
                            runtime.session.ai_block_count(),
                            runtime.session.bookmarked_count(),
                            runtime.pane_grid.label(),
                        )
                    } else if let Some(editor) = self.active_editor() {
                        (
                            0,
                            0,
                            0,
                            format!("editor: {}", editor.mode_label().to_lowercase()),
                        )
                    } else {
                        (0, 0, 0, "panes: 0 (single)".to_owned())
                    };

                ui.horizontal(|ui| {
                    ui.label(
                        egui::RichText::new(&self.status_text)
                            .monospace()
                            .color(theme::TEXT_PRIMARY),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("cmd: {cmd_count}"))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("ai: {ai_count}"))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("bookmarks: {bookmark_count}"))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(pane_label)
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!("tab: {}", self.tabs.active_label()))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                    );
                    ui.separator();
                    ui.label(
                        egui::RichText::new(format!(
                            "autosave: {}s",
                            self.config.session.autosave_interval_sec.max(1)
                        ))
                        .monospace()
                        .color(theme::TEXT_MUTED),
                    );
                });
            });

        egui::TopBottomPanel::bottom("input_bar")
            .exact_height(48.0)
            .frame(
                egui::Frame::new()
                    .fill(theme::BG_SURFACE_0)
                    .stroke(egui::Stroke::new(1.0, theme::BORDER))
                    .inner_margin(egui::Margin::symmetric(10, 8)),
            )
            .show(ctx, |ui| {
                if self.active_tab_kind() == AppTabKind::Editor {
                    ui.horizontal_wrapped(|ui| {
                        ui.label(
                            egui::RichText::new("VIM")
                                .monospace()
                                .strong()
                                .color(theme::ACCENT_BLUE),
                        );
                        ui.separator();
                        ui.label(
                            egui::RichText::new("normal/insert/command mode")
                                .monospace()
                                .color(theme::TEXT_MUTED),
                        );
                    });
                } else {
                    ui.horizontal(|ui| {
                        let time_sec = ui.input(|i| i.time);
                        if self.prompt_mascot_frames.is_empty() {
                            let mascot = self.run_cat_frame(time_sec);
                            ui.label(
                                egui::RichText::new(mascot)
                                    .monospace()
                                    .strong()
                                    .color(theme::ACCENT_BLUE),
                            );
                        } else {
                            let idx =
                                ((time_sec * 12.0) as usize) % self.prompt_mascot_frames.len();
                            if let Some(texture) = self.prompt_mascot_frames.get(idx) {
                                let base = texture.size_vec2();
                                let height = 22.0;
                                let scale = if base.y > 0.0 { height / base.y } else { 1.0 };
                                let size = egui::vec2((base.x * scale).max(18.0), height);
                                ui.add(egui::Image::new((texture.id(), size)));
                            }
                        }
                        ui.label(
                            egui::RichText::new(">")
                                .monospace()
                                .strong()
                                .color(theme::TEXT_MUTED),
                        );

                        let mut request_history_up = false;
                        let mut request_history_down = false;
                        let mut request_submit = false;

                        if let Some(runtime) = self.active_runtime_mut() {
                            let input_width = (ui.available_width() - 92.0).max(120.0);
                            let response = ui.add_sized(
                                [input_width, 30.0],
                                egui::TextEdit::singleline(&mut runtime.input_buffer)
                                    .hint_text("type command and press Enter"),
                            );

                            let run_clicked = ui
                                .add_sized(
                                    [84.0, 30.0],
                                    egui::Button::new(
                                        egui::RichText::new("run")
                                            .monospace()
                                            .strong()
                                            .color(theme::ACCENT_BLUE),
                                    ),
                                )
                                .clicked();

                            if response.has_focus()
                                && ui.input(|i| i.key_pressed(egui::Key::ArrowUp))
                            {
                                request_history_up = true;
                            }

                            if response.has_focus()
                                && ui.input(|i| i.key_pressed(egui::Key::ArrowDown))
                            {
                                request_history_down = true;
                            }

                            let enter_pressed = ui.input(|i| i.key_pressed(egui::Key::Enter));
                            let submit_by_enter =
                                enter_pressed && (response.has_focus() || response.lost_focus());
                            request_submit = run_clicked || submit_by_enter;
                        } else {
                            ui.label(
                                egui::RichText::new("(no active tab runtime)")
                                    .monospace()
                                    .color(theme::TEXT_MUTED),
                            );
                        }

                        if request_history_up {
                            self.navigate_history_up();
                        }
                        if request_history_down {
                            self.navigate_history_down();
                        }
                        if request_submit {
                            self.submit_input();
                        }
                    });
                }
            });

        if self.sidebar_visible {
            egui::SidePanel::left("left_sidebar")
                .resizable(true)
                .default_width(300.0)
                .min_width(240.0)
                .frame(
                    egui::Frame::new()
                        .fill(theme::BG_APP)
                        .stroke(egui::Stroke::new(1.0, theme::BORDER))
                        .inner_margin(egui::Margin::same(10)),
                )
                .show(ctx, |ui| {
                    let actions = self.sidebar.show(ui);
                    self.apply_sidebar_actions(actions);
                });
        }

        egui::CentralPanel::default()
            .frame(
                egui::Frame::new()
                    .fill(theme::BG_APP)
                    .inner_margin(egui::Margin::symmetric(12, 10)),
            )
            .show(ctx, |ui| {
                if self.active_tab_kind() == AppTabKind::Editor {
                    self.render_editor_view(ui, ctx);
                    return;
                }

                ui.vertical(|ui| {
                    theme::toolbar_frame().show(ui, |ui| {
                        ui.horizontal_wrapped(|ui| {
                            ui.label(
                                egui::RichText::new("SEARCH")
                                    .monospace()
                                    .strong()
                                    .color(theme::TEXT_MUTED),
                            );

                            if let Some(runtime) = self.active_runtime_mut() {
                                ui.add_sized(
                                    [220.0, 24.0],
                                    egui::TextEdit::singleline(&mut runtime.block_search_query)
                                        .hint_text("command/ai output"),
                                );
                                if ui
                                    .small_button(
                                        egui::RichText::new("clear").color(theme::TEXT_MUTED),
                                    )
                                    .clicked()
                                {
                                    runtime.block_search_query.clear();
                                }

                                ui.separator();
                                ui.checkbox(&mut runtime.bookmarks_only, "bookmarks only");
                                ui.separator();
                                ui.label(
                                    egui::RichText::new(format!(
                                        "ctx: {}",
                                        runtime.selected_context_block_ids.len()
                                    ))
                                    .monospace()
                                    .color(theme::TEXT_MUTED),
                                );
                                if ui
                                    .small_button(
                                        egui::RichText::new("clear context")
                                            .color(theme::TEXT_MUTED),
                                    )
                                    .clicked()
                                {
                                    runtime.selected_context_block_ids.clear();
                                }
                            }

                            let match_count = self.filtered_timeline_items().len();
                            ui.separator();
                            ui.label(
                                egui::RichText::new(format!("matches: {match_count}"))
                                    .monospace()
                                    .color(theme::TEXT_MUTED),
                            );
                        });
                    });

                    if let Some(runtime) = self.active_runtime() {
                        if !runtime.export_message.is_empty() {
                            ui.label(
                                egui::RichText::new(&runtime.export_message)
                                    .monospace()
                                    .color(theme::ACCENT_BLUE),
                            );
                        }
                    }

                    let (selected_context_count, running_jobs, ai_status_line) =
                        if let Some(runtime) = self.active_runtime() {
                            (
                                runtime.selected_context_block_ids.len(),
                                runtime.running_ai_jobs,
                                runtime.ai_status_line.clone(),
                            )
                        } else {
                            (0, 0, "idle".to_owned())
                        };

                    if self.ai_panel_visible {
                        ui.separator();
                        if let Some(action) = self.ai_panel.show(
                            ui,
                            selected_context_count,
                            running_jobs,
                            &ai_status_line,
                        ) {
                            self.apply_ai_panel_action(action);
                        }
                    }

                    ui.separator();
                    self.render_panes(ui);
                });
            });

        let history_matches = self
            .active_runtime()
            .map(|runtime| runtime.session.history_search(self.palette.query(), 40))
            .unwrap_or_default();
        for action in self.palette.show_window(ctx, &history_matches) {
            self.apply_palette_action(action);
        }

        if self.restore_picker_open {
            let mut open = self.restore_picker_open;
            let mut selected_restore: Option<PathBuf> = None;
            egui::Window::new("Restore Session")
                .collapsible(false)
                .resizable(true)
                .default_size([760.0, 420.0])
                .frame(theme::panel_frame())
                .open(&mut open)
                .show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        if ui.button("refresh").clicked() {
                            self.refresh_saved_session_entries();
                        }
                        ui.label(
                            egui::RichText::new(format!(
                                "saved snapshots: {}",
                                self.saved_session_entries.len()
                            ))
                            .monospace()
                            .color(theme::TEXT_MUTED),
                        );
                    });
                    ui.separator();

                    if self.saved_session_entries.is_empty() {
                        ui.label(
                            egui::RichText::new("(no saved sessions yet)")
                                .monospace()
                                .italics()
                                .color(theme::TEXT_MUTED),
                        );
                    } else {
                        egui::ScrollArea::vertical()
                            .auto_shrink([false, false])
                            .show(ui, |ui| {
                                for entry in &self.saved_session_entries {
                                    ui.horizontal_wrapped(|ui| {
                                        if ui
                                            .small_button(
                                                egui::RichText::new("restore")
                                                    .color(theme::ACCENT_BLUE),
                                            )
                                            .clicked()
                                        {
                                            selected_restore = Some(entry.path.clone());
                                        }
                                        ui.label(
                                            egui::RichText::new(format!(
                                                "{}  (saved: {})",
                                                entry.path.display(),
                                                entry.modified_unix_sec
                                            ))
                                            .monospace()
                                            .color(theme::TEXT_PRIMARY),
                                        );
                                    });
                                    ui.separator();
                                }
                            });
                    }
                });

            if let Some(path) = selected_restore {
                self.restore_workspace_from_path(&path);
                open = false;
            }
            self.restore_picker_open = open;
        }

        if ctx.input(|i| i.key_pressed(egui::Key::Escape)) {
            self.palette.close();
        }

        ctx.request_repaint_after(Duration::from_millis(16));
    }
}

impl Drop for TerminalApp {
    fn drop(&mut self) {
        let snapshot = self.build_workspace_snapshot();
        let _ = save_workspace_snapshot_to_disk(&self.session_save_path, &snapshot);
    }
}

fn process_editor_events(editor: &mut EditorTabState, ctx: &egui::Context) -> EditorCommand {
    let events = ctx.input(|i| i.events.clone());
    let mut command = EditorCommand::None;

    for event in events {
        match event {
            egui::Event::Key {
                key,
                pressed,
                modifiers,
                ..
            } => {
                if !pressed {
                    continue;
                }

                if modifiers.ctrl && key == egui::Key::S {
                    if let Err(err) = editor.write_to_disk() {
                        editor.status_line = format!("write failed: {err}");
                    }
                    continue;
                }

                match editor.mode {
                    VimMode::Normal => match key {
                        egui::Key::ArrowLeft => editor.move_left(),
                        egui::Key::ArrowRight => editor.move_right(),
                        egui::Key::ArrowUp => editor.move_up(),
                        egui::Key::ArrowDown => editor.move_down(),
                        egui::Key::Escape => editor.set_mode_normal(),
                        _ => {}
                    },
                    VimMode::Insert => match key {
                        egui::Key::Escape => editor.set_mode_normal(),
                        egui::Key::ArrowLeft => editor.move_left(),
                        egui::Key::ArrowRight => editor.move_right(),
                        egui::Key::ArrowUp => editor.move_up(),
                        egui::Key::ArrowDown => editor.move_down(),
                        egui::Key::Enter => editor.insert_newline(),
                        egui::Key::Backspace => editor.backspace(),
                        egui::Key::Delete => editor.delete_char(),
                        egui::Key::Tab => editor.insert_text("    "),
                        _ => {}
                    },
                    VimMode::Command => match key {
                        egui::Key::Escape => editor.set_mode_normal(),
                        egui::Key::Backspace => {
                            editor.command_buffer.pop();
                        }
                        egui::Key::Enter => {
                            command = editor.execute_command();
                        }
                        _ => {}
                    },
                }
            }
            egui::Event::Text(text) => match editor.mode {
                VimMode::Insert => {
                    if !text.chars().any(|ch| ch.is_control()) {
                        editor.insert_text(&text);
                    }
                }
                VimMode::Command => {
                    if text == ":" && editor.command_buffer.is_empty() {
                        continue;
                    }
                    if !text.chars().any(|ch| ch.is_control()) {
                        editor.command_buffer.push_str(&text);
                    }
                }
                VimMode::Normal => {
                    for ch in text.chars() {
                        if editor.pending_normal_key == Some('d') {
                            if ch == 'd' {
                                editor.delete_current_line();
                                editor.pending_normal_key = None;
                                continue;
                            }
                            editor.pending_normal_key = None;
                        }

                        match ch {
                            'h' => {
                                editor.move_left();
                                editor.pending_normal_key = None;
                            }
                            'j' => {
                                editor.move_down();
                                editor.pending_normal_key = None;
                            }
                            'k' => {
                                editor.move_up();
                                editor.pending_normal_key = None;
                            }
                            'l' => {
                                editor.move_right();
                                editor.pending_normal_key = None;
                            }
                            '0' => {
                                editor.cursor_col = 0;
                                editor.pending_normal_key = None;
                            }
                            '$' => {
                                editor.cursor_col =
                                    editor.lines[editor.cursor_line].chars().count();
                                editor.pending_normal_key = None;
                            }
                            'i' => editor.set_mode_insert(),
                            'a' => {
                                editor.move_right();
                                editor.set_mode_insert();
                            }
                            'x' => {
                                editor.delete_char();
                                editor.pending_normal_key = None;
                            }
                            'd' => {
                                editor.pending_normal_key = Some('d');
                            }
                            'o' => {
                                editor.open_newline_below();
                                editor.pending_normal_key = None;
                            }
                            'O' => {
                                editor.open_newline_above();
                                editor.pending_normal_key = None;
                            }
                            ':' => {
                                editor.mode = VimMode::Command;
                                editor.command_buffer.clear();
                                editor.pending_normal_key = None;
                                editor.status_line = ":".to_owned();
                            }
                            _ => {
                                editor.pending_normal_key = None;
                            }
                        }
                    }
                }
            },
            _ => {}
        }
    }

    editor.ensure_cursor_bounds();
    command
}

fn render_editor_line_with_cursor(
    ui: &mut egui::Ui,
    line: &str,
    cursor_col: usize,
    mode: VimMode,
    blink_on: bool,
    line_color: egui::Color32,
) {
    let col = cursor_col.min(line.chars().count());
    let split = EditorTabState::to_byte_idx(line, col);

    if !blink_on {
        ui.label(egui::RichText::new(line).monospace().color(line_color));
        return;
    }

    let (prefix, cursor_char, suffix) = if split >= line.len() {
        (line, " ", "")
    } else {
        let mut chars = line[split..].chars();
        let first = chars.next().unwrap_or(' ');
        let first_len = first.len_utf8();
        (
            &line[..split],
            &line[split..split + first_len],
            &line[split + first_len..],
        )
    };

    if !prefix.is_empty() {
        ui.label(egui::RichText::new(prefix).monospace().color(line_color));
    }

    let cursor_bg = match mode {
        VimMode::Normal => theme::ACCENT_BLUE,
        VimMode::Insert => theme::SUCCESS,
        VimMode::Command => theme::TEXT_BRIGHT,
    };
    ui.label(
        egui::RichText::new(cursor_char)
            .monospace()
            .color(theme::BG_APP)
            .background_color(cursor_bg),
    );

    if !suffix.is_empty() {
        ui.label(egui::RichText::new(suffix).monospace().color(line_color));
    }
}

fn normalize_opened_path(path: PathBuf) -> PathBuf {
    #[cfg(windows)]
    {
        let raw = path.to_string_lossy().to_string();
        if let Some(rest) = raw.strip_prefix(r"\\?\UNC\") {
            return PathBuf::from(format!(r"\\{rest}"));
        }
        if let Some(rest) = raw.strip_prefix(r"\\?\") {
            return PathBuf::from(rest);
        }
    }

    path
}

fn display_path(path: &Path) -> String {
    normalize_opened_path(path.to_path_buf())
        .display()
        .to_string()
}

#[derive(Debug, Clone)]
struct AiLaunchAttempt {
    program: String,
    args: Vec<String>,
}

fn push_launch_attempt(
    attempts: &mut Vec<AiLaunchAttempt>,
    seen: &mut HashSet<String>,
    program: impl Into<String>,
    args: Vec<String>,
) {
    let program = program.into();
    let key = format!("{program}\u{1F}{:?}", args);
    if seen.insert(key) {
        attempts.push(AiLaunchAttempt { program, args });
    }
}

#[cfg(windows)]
fn push_windows_candidate_if_exists(
    attempts: &mut Vec<AiLaunchAttempt>,
    seen: &mut HashSet<String>,
    candidate: PathBuf,
    args: &[String],
) {
    if candidate.exists() {
        push_launch_attempt(
            attempts,
            seen,
            candidate.display().to_string(),
            args.to_vec(),
        );
    }
}

#[cfg(windows)]
fn add_windows_npm_global_bin_attempts(
    attempts: &mut Vec<AiLaunchAttempt>,
    seen: &mut HashSet<String>,
    bin_name: &str,
    args: &[String],
) {
    if let Ok(appdata) = std::env::var("APPDATA") {
        let npm_dir = PathBuf::from(appdata).join("npm");
        push_windows_candidate_if_exists(attempts, seen, npm_dir.join(bin_name), args);
        push_windows_candidate_if_exists(
            attempts,
            seen,
            npm_dir.join(format!("{bin_name}.cmd")),
            args,
        );
        push_windows_candidate_if_exists(
            attempts,
            seen,
            npm_dir.join(format!("{bin_name}.bat")),
            args,
        );
    }
}

#[cfg(windows)]
fn add_windows_npx_attempts(
    attempts: &mut Vec<AiLaunchAttempt>,
    seen: &mut HashSet<String>,
    args: &[String],
) {
    add_windows_npm_global_bin_attempts(attempts, seen, "npx", args);

    if let Ok(program_files) = std::env::var("ProgramFiles") {
        let node_dir = PathBuf::from(program_files).join("nodejs");
        push_windows_candidate_if_exists(attempts, seen, node_dir.join("npx"), args);
        push_windows_candidate_if_exists(attempts, seen, node_dir.join("npx.cmd"), args);
    }
    if let Ok(program_files_x86) = std::env::var("ProgramFiles(x86)") {
        let node_dir = PathBuf::from(program_files_x86).join("nodejs");
        push_windows_candidate_if_exists(attempts, seen, node_dir.join("npx"), args);
        push_windows_candidate_if_exists(attempts, seen, node_dir.join("npx.cmd"), args);
    }
}

fn build_ai_launch_attempts(program: &str, args: &[String]) -> Vec<AiLaunchAttempt> {
    let mut attempts = Vec::new();
    let mut seen = HashSet::new();
    push_launch_attempt(&mut attempts, &mut seen, program.to_owned(), args.to_vec());

    #[cfg(windows)]
    {
        let has_extension = Path::new(program).extension().is_some();
        if !has_extension {
            push_launch_attempt(
                &mut attempts,
                &mut seen,
                format!("{program}.exe"),
                args.to_vec(),
            );
            push_launch_attempt(
                &mut attempts,
                &mut seen,
                format!("{program}.cmd"),
                args.to_vec(),
            );
            push_launch_attempt(
                &mut attempts,
                &mut seen,
                format!("{program}.bat"),
                args.to_vec(),
            );
        }
    }

    let lower = program.to_ascii_lowercase();
    if lower.contains("codex") {
        #[cfg(windows)]
        add_windows_npm_global_bin_attempts(&mut attempts, &mut seen, "codex", args);

        let mut npx_args = vec!["-y".to_owned(), "@openai/codex".to_owned()];
        npx_args.extend(args.iter().cloned());
        push_launch_attempt(&mut attempts, &mut seen, "npx", npx_args);

        #[cfg(windows)]
        {
            let mut npx_args = vec!["-y".to_owned(), "@openai/codex".to_owned()];
            npx_args.extend(args.iter().cloned());
            add_windows_npx_attempts(&mut attempts, &mut seen, &npx_args);
        }
    }
    if lower.contains("claude") {
        #[cfg(windows)]
        add_windows_npm_global_bin_attempts(&mut attempts, &mut seen, "claude", args);

        let mut npx_args = vec!["-y".to_owned(), "@anthropic-ai/claude-code".to_owned()];
        npx_args.extend(args.iter().cloned());
        push_launch_attempt(&mut attempts, &mut seen, "npx", npx_args);

        #[cfg(windows)]
        {
            let mut npx_args = vec!["-y".to_owned(), "@anthropic-ai/claude-code".to_owned()];
            npx_args.extend(args.iter().cloned());
            add_windows_npx_attempts(&mut attempts, &mut seen, &npx_args);
        }
    }

    attempts
}

fn ai_install_hint(program: &str) -> Option<&'static str> {
    let lower = program.to_ascii_lowercase();
    if lower.contains("codex") {
        return Some(
            "Install Codex CLI: `npm install -g @openai/codex` (or use `npx @openai/codex`). \
Non-interactive usage: `codex exec \"<prompt>\"`.",
        );
    }
    if lower.contains("claude") {
        return Some(
            "Install Claude Code: `npm install -g @anthropic-ai/claude-code` (or use `npx @anthropic-ai/claude-code`). \
Non-interactive usage: `claude --print \"<prompt>\"`.",
        );
    }
    None
}

fn prepare_ai_prompt_transport(
    program: &str,
    args: &[String],
    prompt: Option<&str>,
) -> (Vec<String>, Option<String>, bool) {
    let Some(prompt) = prompt else {
        return (args.to_vec(), None, false);
    };

    let lower = program.to_ascii_lowercase();
    let is_supported = lower.contains("codex") || lower.contains("claude");

    if is_supported {
        if lower.contains("codex") {
            let mut replaced = false;
            let mut routed_args = Vec::with_capacity(args.len());
            for arg in args {
                if arg == prompt {
                    routed_args.push("-".to_owned());
                    replaced = true;
                } else {
                    routed_args.push(arg.clone());
                }
            }
            if !replaced {
                routed_args.push("-".to_owned());
            }
            return (routed_args, Some(prompt.to_owned()), true);
        }

        if lower.contains("claude") {
            let mut routed_args = Vec::with_capacity(args.len());
            let mut removed = false;
            for arg in args {
                if arg == prompt {
                    removed = true;
                    continue;
                }
                routed_args.push(arg.clone());
            }
            if !removed {
                routed_args = args.to_vec();
            }
            return (routed_args, Some(prompt.to_owned()), true);
        }
    }

    (args.to_vec(), None, false)
}

fn ensure_claude_tab_scoped_session_args(
    program: &str,
    args: &[String],
    tab_id: u64,
    workspace_root: &Path,
) -> Vec<String> {
    if !program.to_ascii_lowercase().contains("claude") {
        return args.to_vec();
    }

    let has_explicit_session_target = args.iter().any(|arg| {
        arg == "--resume"
            || arg == "-r"
            || arg.starts_with("--resume=")
            || arg == "--session-id"
            || arg.starts_with("--session-id=")
            || arg == "--no-session-persistence"
    });

    if has_explicit_session_target {
        return args.to_vec();
    }

    let session_id = build_tab_scoped_claude_session_id(tab_id, workspace_root);
    let mut updated = Vec::with_capacity(args.len() + 2);
    updated.push("--session-id".to_owned());
    updated.push(session_id);
    updated.extend(args.iter().cloned());
    updated
}

fn build_tab_scoped_claude_session_id(tab_id: u64, workspace_root: &Path) -> String {
    let workspace = workspace_root.to_string_lossy();
    let mut seed = String::with_capacity(workspace.len() + 24);
    seed.push_str(&workspace);
    seed.push('|');
    seed.push_str(&tab_id.to_string());

    let h1 = fnv1a64(workspace.as_bytes());
    let h2 = fnv1a64(seed.as_bytes());

    let part1 = ((h1 >> 32) & 0xffff_ffff) as u32;
    let part2 = ((h1 >> 16) & 0xffff) as u16;
    let mut part3 = (h1 & 0xffff) as u16;
    part3 = (part3 & 0x0fff) | 0x5000;

    let mut part4 = ((h2 >> 48) & 0xffff) as u16;
    part4 = (part4 & 0x3fff) | 0x8000;
    let part5 = h2 & 0x0000_ffff_ffff_ffff;

    format!("{part1:08x}-{part2:04x}-{part3:04x}-{part4:04x}-{part5:012x}")
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    for byte in bytes {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3u64);
    }
    hash
}

fn run_ai_command(
    command: ResolvedAiCommand,
    prompt: Option<String>,
    tab_id: u64,
    ai_block_id: u64,
    timeout_sec: u64,
    workspace_root: PathBuf,
    tx: Sender<AiRunEvent>,
) {
    let started = Instant::now();
    let program = command.program;
    let args = command.args;

    if program.trim().is_empty() {
        let _ = tx.send(AiRunEvent::Failed {
            tab_id,
            ai_block_id,
            message: "AI command program is empty in config".to_owned(),
            duration_ms: 0,
        });
        return;
    }

    let (prepared_args, stdin_payload, routed_via_stdin) =
        prepare_ai_prompt_transport(&program, &args, prompt.as_deref());
    let prepared_args =
        ensure_claude_tab_scoped_session_args(&program, &prepared_args, tab_id, &workspace_root);
    let attempts = build_ai_launch_attempts(&program, &prepared_args);
    let mut launched_program = program.clone();
    let mut launch_errors = Vec::new();
    let mut all_not_found = true;
    let needs_stdin = stdin_payload.is_some();

    let mut child_opt = None;
    for attempt in &attempts {
        let mut cmd = Command::new(&attempt.program);
        match cmd
            .args(attempt.args.iter().map(String::as_str))
            .stdin(if needs_stdin {
                Stdio::piped()
            } else {
                Stdio::null()
            })
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(child) => {
                launched_program = attempt.program.clone();
                child_opt = Some(child);
                break;
            }
            Err(err) => {
                if err.kind() != std::io::ErrorKind::NotFound {
                    all_not_found = false;
                }
                launch_errors.push(format!("{} ({err})", attempt.program));
            }
        }
    }

    let mut child = match child_opt {
        Some(child) => child,
        None => {
            let attempted = attempts
                .iter()
                .map(|a| a.program.clone())
                .collect::<Vec<_>>()
                .join(", ");
            let base_message = if all_not_found {
                format!("failed to launch {program}: executable not found (attempted: {attempted})")
            } else {
                let first_error = launch_errors
                    .first()
                    .cloned()
                    .unwrap_or_else(|| "unknown error".to_owned());
                let lower_errors = launch_errors.join(" | ").to_ascii_lowercase();
                if lower_errors.contains("too long") || lower_errors.contains("206") {
                    format!(
                        "failed to launch {program}: command line too long on Windows (attempted: {attempted})"
                    )
                } else {
                    format!("failed to launch {program}: {first_error} (attempted: {attempted})")
                }
            };
            let message = if let Some(hint) = ai_install_hint(&program) {
                format!("{base_message}\n{hint}")
            } else {
                base_message
            };
            let _ = tx.send(AiRunEvent::Failed {
                tab_id,
                ai_block_id,
                message,
                duration_ms: started.elapsed().as_millis().min(u64::MAX as u128) as u64,
            });
            return;
        }
    };

    if let Some(payload) = stdin_payload {
        match child.stdin.take() {
            Some(mut stdin) => {
                if let Err(err) = stdin.write_all(payload.as_bytes()) {
                    let _ = tx.send(AiRunEvent::Failed {
                        tab_id,
                        ai_block_id,
                        message: format!("failed writing prompt to AI stdin: {err}"),
                        duration_ms: started.elapsed().as_millis().min(u64::MAX as u128) as u64,
                    });
                    let _ = child.kill();
                    let _ = child.wait();
                    return;
                }
                let _ = stdin.flush();
            }
            None => {
                let _ = tx.send(AiRunEvent::Failed {
                    tab_id,
                    ai_block_id,
                    message: "failed launching AI process with stdin prompt".to_owned(),
                    duration_ms: started.elapsed().as_millis().min(u64::MAX as u128) as u64,
                });
                let _ = child.kill();
                let _ = child.wait();
                return;
            }
        }

        let _ = routed_via_stdin;
    }

    let had_output = Arc::new(AtomicBool::new(false));

    let stdout_handle = child.stdout.take().map(|stdout| {
        spawn_stream_reader(
            stdout,
            tab_id,
            ai_block_id,
            tx.clone(),
            had_output.clone(),
            program.clone(),
            false,
        )
    });
    let stderr_handle = child.stderr.take().map(|stderr| {
        spawn_stream_reader(
            stderr,
            tab_id,
            ai_block_id,
            tx.clone(),
            had_output.clone(),
            program.clone(),
            true,
        )
    });

    let timeout = Duration::from_secs(timeout_sec.max(1));
    let mut timed_out = false;
    let exit_code: i32;

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                exit_code = status.code().unwrap_or(-1);
                break;
            }
            Ok(None) => {
                if started.elapsed() >= timeout {
                    timed_out = true;
                    let _ = child.kill();
                    let status = child.wait().ok();
                    exit_code = status.and_then(|s| s.code()).unwrap_or(-1);
                    break;
                }
                thread::sleep(Duration::from_millis(100));
            }
            Err(err) => {
                let _ = tx.send(AiRunEvent::Failed {
                    tab_id,
                    ai_block_id,
                    message: format!("failed to poll AI process status: {err}"),
                    duration_ms: started.elapsed().as_millis().min(u64::MAX as u128) as u64,
                });
                return;
            }
        }
    }

    if let Some(handle) = stdout_handle {
        let _ = handle.join();
    }
    if let Some(handle) = stderr_handle {
        let _ = handle.join();
    }

    if !had_output.load(Ordering::Relaxed) {
        let _ = tx.send(AiRunEvent::OutputChunk {
            tab_id,
            ai_block_id,
            lines: vec!["(no output from AI CLI)".to_owned()],
        });
    }

    let elapsed = started.elapsed().as_millis().min(u64::MAX as u128) as u64;
    if timed_out {
        let _ = tx.send(AiRunEvent::Failed {
            tab_id,
            ai_block_id,
            message: format!("AI command timed out after {}s", timeout.as_secs()),
            duration_ms: elapsed,
        });
        return;
    }

    if exit_code == 0 {
        let _ = tx.send(AiRunEvent::Completed {
            tab_id,
            ai_block_id,
            exit_code,
            duration_ms: elapsed,
        });
    } else {
        let _ = tx.send(AiRunEvent::Failed {
            tab_id,
            ai_block_id,
            message: format!("{launched_program} exited with code {exit_code}"),
            duration_ms: elapsed,
        });
    }
}

fn spawn_stream_reader<R: Read + Send + 'static>(
    pipe: R,
    tab_id: u64,
    ai_block_id: u64,
    tx: Sender<AiRunEvent>,
    had_output: Arc<AtomicBool>,
    program: String,
    is_stderr: bool,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut reader = BufReader::new(pipe);
        let mut line = String::new();

        loop {
            line.clear();
            let read_result = reader.read_line(&mut line);
            let bytes = match read_result {
                Ok(bytes) => bytes,
                Err(_) => break,
            };
            if bytes == 0 {
                break;
            }

            let text = line.trim_end_matches(['\r', '\n']).to_owned();
            if text.is_empty() {
                continue;
            }

            if is_stderr && !should_emit_ai_stderr_line(&program, &text) {
                continue;
            }

            had_output.store(true, Ordering::Relaxed);
            let payload = if is_stderr {
                format!("[stderr] {text}")
            } else {
                text
            };
            if tx
                .send(AiRunEvent::OutputChunk {
                    tab_id,
                    ai_block_id,
                    lines: vec![payload],
                })
                .is_err()
            {
                break;
            }
        }
    })
}

fn should_emit_ai_stderr_line(program: &str, text: &str) -> bool {
    let lower_program = program.to_ascii_lowercase();
    if !lower_program.contains("codex") {
        return true;
    }

    let lower = text.to_ascii_lowercase();
    let error_like = [
        "error",
        "failed",
        "not found",
        "no such",
        "denied",
        "permission",
        "unauthorized",
        "forbidden",
        "timeout",
        "timed out",
        "panic",
        "exception",
        "traceback",
        "invalid",
    ];

    error_like.iter().any(|kw| lower.contains(kw))
}

fn default_config_path() -> PathBuf {
    PathBuf::from("config").join("config.toml")
}

fn load_or_init_config(path: &PathBuf) -> Result<(AppConfig, Option<SystemTime>, String)> {
    if !path.exists() {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let defaults = AppConfig::default();
        let content = config_serialize_pretty(&defaults)?;
        fs::write(path, content)?;
    }

    let config = load_config_file(path)?;
    let mtime = fs::metadata(path).ok().and_then(|m| m.modified().ok());
    Ok((
        config,
        mtime,
        format!("config loaded from {}", path.display()),
    ))
}

fn load_config_file(path: &PathBuf) -> Result<AppConfig> {
    let content = fs::read_to_string(path)?;
    config_deserialize(&content)
}

fn load_workspace_snapshot_from_disk(path: &PathBuf) -> Result<Option<WorkspaceSnapshot>> {
    if !path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(path)?;

    if let Ok(snapshot) = toml::from_str::<WorkspaceSnapshot>(&content) {
        return Ok(Some(snapshot));
    }

    if let Ok(legacy) = toml::from_str::<SessionSnapshot>(&content) {
        return Ok(Some(WorkspaceSnapshot {
            format_version: 0,
            active_tab_id: 0,
            tabs: vec![SavedTabSnapshot {
                tab_id: 0,
                tab_label: "main".to_owned(),
                session: legacy,
            }],
        }));
    }

    Err(anyhow!(
        "failed to parse session snapshot format: {}",
        path.display()
    ))
}

fn save_workspace_snapshot_to_disk(path: &PathBuf, snapshot: &WorkspaceSnapshot) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    let serialized = toml::to_string_pretty(&snapshot)?;
    fs::write(path, serialized)?;
    Ok(())
}

fn list_saved_workspace_entries(
    autosave_path: &PathBuf,
    snapshot_dir: &PathBuf,
) -> Result<Vec<SavedSessionEntry>> {
    let mut entries = Vec::new();
    let mut seen = HashSet::new();

    if autosave_path.exists() {
        let modified_unix_sec = fs::metadata(autosave_path)
            .ok()
            .and_then(|meta| meta.modified().ok())
            .and_then(|value| value.duration_since(UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or_default();
        entries.push(SavedSessionEntry {
            path: autosave_path.clone(),
            modified_unix_sec,
        });
        seen.insert(
            autosave_path
                .canonicalize()
                .unwrap_or_else(|_| autosave_path.clone()),
        );
    }

    if snapshot_dir.exists() {
        for dir_entry in fs::read_dir(snapshot_dir)? {
            let dir_entry = dir_entry?;
            let path = dir_entry.path();
            if !path.is_file() {
                continue;
            }

            if path
                .extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| !ext.eq_ignore_ascii_case("toml"))
                .unwrap_or(true)
            {
                continue;
            }

            let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
            if !seen.insert(canonical) {
                continue;
            }

            let modified_unix_sec = dir_entry
                .metadata()
                .ok()
                .and_then(|meta| meta.modified().ok())
                .and_then(|value| value.duration_since(UNIX_EPOCH).ok())
                .map(|d| d.as_secs())
                .unwrap_or_default();

            entries.push(SavedSessionEntry {
                path,
                modified_unix_sec,
            });
        }
    }

    entries.sort_by(|a, b| {
        b.modified_unix_sec
            .cmp(&a.modified_unix_sec)
            .then_with(|| a.path.cmp(&b.path))
    });

    Ok(entries)
}

fn parse_first_file_line_ref(line: &str, workspace_root: &PathBuf) -> Option<FileLineRef> {
    for token in line.split_whitespace() {
        let trimmed = token.trim_matches(|c: char| {
            matches!(
                c,
                '"' | '\'' | ',' | ';' | '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>'
            )
        });

        if trimmed.is_empty() {
            continue;
        }

        let mut split = trimmed.rsplitn(3, ':');
        let last = split.next()?;
        if !last.chars().all(|c| c.is_ascii_digit()) {
            continue;
        }

        let second = split.next()?;
        let third = split.next();

        let (path_part, line_num, column_num) = if second.chars().all(|c| c.is_ascii_digit()) {
            let path = third?;
            (
                path.to_owned(),
                second.parse::<u32>().ok()?,
                Some(last.parse::<u32>().ok()?),
            )
        } else {
            let path = match third {
                Some(head) => format!("{head}:{second}"),
                None => second.to_owned(),
            };
            (path, last.parse::<u32>().ok()?, None)
        };

        let candidate = PathBuf::from(&path_part);
        let resolved = if candidate.is_absolute() {
            candidate
        } else {
            workspace_root.join(candidate)
        };

        if resolved.exists() {
            return Some(FileLineRef {
                path: resolved,
                line: line_num,
                column: column_num,
            });
        }
    }

    None
}

fn build_editor_open_command(reference: &FileLineRef) -> String {
    match reference.column {
        Some(col) => format!(
            "code -g \"{}\":{}:{}",
            reference.path.display(),
            reference.line,
            col
        ),
        None => format!(
            "code -g \"{}\":{}",
            reference.path.display(),
            reference.line
        ),
    }
}

fn build_command_block_copy_text(block: &CommandBlock) -> String {
    let mut out = String::new();
    out.push_str(&format!("Command Block #{}\n", block.id));
    out.push_str(&format!("Command: {}\n", block.command));
    out.push_str(&format!("CWD: {}\n", block.working_directory));
    out.push_str(&format!("Timestamp(ms): {}\n", block.timestamp_unix_ms));
    out.push_str(&format!("Bookmarked: {}\n", block.bookmarked));
    out.push_str("Output:\n");
    for line in &block.output_lines {
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn build_ai_block_copy_text(block: &AiBlock) -> String {
    let mut out = String::new();
    out.push_str(&format!("AI Block #{}\n", block.id));
    out.push_str(&format!("Tool: {}\n", block.tool.label()));
    out.push_str(&format!("Status: {:?}\n", block.status));
    out.push_str(&format!("Exit: {:?}\n", block.exit_code));
    out.push_str(&format!("Prompt: {}\n", block.prompt));
    out.push_str(&format!("Context IDs: {:?}\n", block.context_block_ids));
    out.push_str("Output:\n");
    for line in &block.output_lines {
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn sanitize_shell_output_lines(lines: Vec<String>, command: &str) -> Vec<String> {
    lines
        .into_iter()
        .filter_map(|line| sanitize_shell_output_line(&line, command))
        .collect()
}

fn sanitize_shell_output_line(line: &str, command: &str) -> Option<String> {
    let line = line.trim_end_matches('\0').trim_end();
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return None;
    }

    if let Some(prompt_tail) = extract_powershell_prompt_tail(trimmed) {
        let cmd = command.trim();
        if prompt_tail.is_empty() {
            return None;
        }
        if !cmd.is_empty() && prompt_tail.eq_ignore_ascii_case(cmd) {
            return None;
        }
        if is_continuation_prompt(prompt_tail) {
            return None;
        }

        // Prompt-prefixed echo/noise lines are not command result output.
        return None;
    }

    if is_continuation_prompt(trimmed) {
        return None;
    }

    if is_directory_header_noise(trimmed) {
        return None;
    }

    let cmd = command.trim();
    if !cmd.is_empty() && trimmed.eq_ignore_ascii_case(cmd) {
        return None;
    }

    Some(line.to_owned())
}

fn should_hide_pending_line(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return true;
    }
    if extract_powershell_prompt_tail(trimmed).is_some() {
        return true;
    }
    is_continuation_prompt(trimmed)
}

fn extract_powershell_prompt_tail(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("PS ") {
        return None;
    }

    let idx = trimmed.find('>')?;
    Some(trimmed[idx + 1..].trim_start())
}

fn is_continuation_prompt(line: &str) -> bool {
    let trimmed = line.trim();
    trimmed == ">" || trimmed == ">>" || trimmed.starts_with(">>")
}

fn is_directory_header_noise(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.starts_with("Directory:") {
        return true;
    }

    if let Some(rest) = trimmed.strip_prefix(':') {
        let path = rest.trim_start();
        return looks_like_windows_path(path);
    }

    false
}

fn looks_like_windows_path(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(drive) = chars.next() else {
        return false;
    };
    let Some(colon) = chars.next() else {
        return false;
    };
    let Some(slash) = chars.next() else {
        return false;
    };

    drive.is_ascii_alphabetic() && colon == ':' && (slash == '\\' || slash == '/')
}

#[cfg(test)]
mod tests {
    use super::{
        SavedTabSnapshot, SessionState, WorkspaceSnapshot, ai_install_hint,
        build_ai_block_copy_text, build_ai_launch_attempts, build_command_block_copy_text,
        build_editor_open_command, build_tab_scoped_claude_session_id,
        ensure_claude_tab_scoped_session_args, load_workspace_snapshot_from_disk,
        parse_first_file_line_ref, prepare_ai_prompt_transport, sanitize_shell_output_lines,
        save_workspace_snapshot_to_disk, should_emit_ai_stderr_line, should_hide_pending_line,
    };
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};
    use ux_model::ai::{AiBlock, AiTool};
    use ux_model::blocks::CommandBlock;

    #[test]
    fn session_snapshot_file_roundtrip() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or_default();
        let base = std::env::temp_dir().join(format!("myterminal-c-session-{nonce}"));
        let path: PathBuf = base.join("session.toml");

        let mut session = SessionState::new("D:\\repo".to_owned());
        session.start_command_block("echo hello".to_owned(), "D:\\repo".to_owned());
        session.push_output_lines(vec!["hello".to_owned()]);

        let snapshot = WorkspaceSnapshot {
            format_version: 1,
            active_tab_id: 0,
            tabs: vec![SavedTabSnapshot {
                tab_id: 0,
                tab_label: "main".to_owned(),
                session: session.to_snapshot(),
            }],
        };

        save_workspace_snapshot_to_disk(&path, &snapshot)
            .expect("session snapshot save must succeed");
        assert!(path.exists(), "snapshot file must be created");

        let restored = load_workspace_snapshot_from_disk(&path)
            .expect("session snapshot load should not fail")
            .expect("session snapshot should exist");
        assert_eq!(restored.tabs.len(), 1);
        let restored_session = SessionState::from_snapshot(restored.tabs[0].session.clone());
        assert_eq!(restored_session.block_count(), session.block_count());

        let _ = fs::remove_file(&path);
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn legacy_session_snapshot_can_be_loaded_as_workspace() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or_default();
        let base = std::env::temp_dir().join(format!("myterminal-c-legacy-{nonce}"));
        let path: PathBuf = base.join("session.toml");

        let mut session = SessionState::new("D:\\repo".to_owned());
        session.start_command_block("echo legacy".to_owned(), "D:\\repo".to_owned());
        session.push_output_lines(vec!["legacy".to_owned()]);
        let legacy = session.to_snapshot();

        if let Some(parent) = path.parent() {
            let _ = fs::create_dir_all(parent);
        }
        let content = toml::to_string_pretty(&legacy).expect("legacy snapshot should serialize");
        fs::write(&path, content).expect("legacy snapshot should be written");

        let restored = load_workspace_snapshot_from_disk(&path)
            .expect("legacy snapshot should load")
            .expect("legacy snapshot should exist");
        assert_eq!(restored.tabs.len(), 1);
        assert_eq!(restored.tabs[0].tab_label, "main");
        let restored_session = SessionState::from_snapshot(restored.tabs[0].session.clone());
        assert_eq!(restored_session.block_count(), 1);

        let _ = fs::remove_file(&path);
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn file_line_reference_parser_detects_relative_path() {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or_default();
        let base = std::env::temp_dir().join(format!("myterminal-c-link-{nonce}"));
        let src_dir = base.join("src");
        let file_path = src_dir.join("main.rs");
        fs::create_dir_all(&src_dir).expect("temp src dir should be created");
        fs::write(&file_path, "fn main() {}\n").expect("temp file should be written");

        let line = "error at src/main.rs:12:5";
        let reference = parse_first_file_line_ref(line, &base).expect("reference should be parsed");
        assert_eq!(reference.path, file_path);
        assert_eq!(reference.line, 12);
        assert_eq!(reference.column, Some(5));

        let cmd = build_editor_open_command(&reference);
        assert!(cmd.contains("code -g"));
        assert!(cmd.contains(":12:5"));

        let _ = fs::remove_file(&file_path);
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn copy_builders_include_key_fields() {
        let mut cmd = CommandBlock::new(10, "cargo check".to_owned(), "D:\\repo".to_owned());
        cmd.output_lines = vec!["ok".to_owned()];
        cmd.bookmarked = true;
        let copied_cmd = build_command_block_copy_text(&cmd);
        assert!(copied_cmd.contains("Command Block #10"));
        assert!(copied_cmd.contains("cargo check"));
        assert!(copied_cmd.contains("Bookmarked: true"));

        let mut ai = AiBlock::new(7, AiTool::CodexCli, "summarize".to_owned(), vec![10]);
        ai.output_lines = vec!["done".to_owned()];
        let copied_ai = build_ai_block_copy_text(&ai);
        assert!(copied_ai.contains("AI Block #7"));
        assert!(copied_ai.contains("Tool: Codex CLI"));
        assert!(copied_ai.contains("summarize"));
    }

    #[test]
    fn shell_sanitizer_drops_prompt_and_echo_lines() {
        let lines = vec![
            "PS C:\\Users\\ldgyu> ls".to_owned(),
            ">> ".to_owned(),
            "  : C:\\Users\\ldgyu".to_owned(),
            "Mode   LastWriteTime".to_owned(),
            "d----- test".to_owned(),
            "PS C:\\Users\\ldgyu>".to_owned(),
        ];

        let cleaned = sanitize_shell_output_lines(lines, "ls");
        assert_eq!(
            cleaned,
            vec!["Mode   LastWriteTime".to_owned(), "d----- test".to_owned()]
        );
    }

    #[test]
    fn pending_prompt_line_is_hidden() {
        assert!(should_hide_pending_line("PS C:\\Users\\ldgyu>"));
        assert!(should_hide_pending_line(">> "));
        assert!(!should_hide_pending_line("building project..."));
    }

    #[test]
    fn codex_launch_attempts_include_npx_fallback() {
        let args = vec!["exec".to_owned(), "hello".to_owned()];
        let attempts = build_ai_launch_attempts("codex", &args);
        assert!(attempts.iter().any(|attempt| {
            attempt.program == "npx"
                && attempt.args.len() >= 3
                && attempt.args[0] == "-y"
                && attempt.args[1] == "@openai/codex"
                && attempt.args[2] == "exec"
        }));
    }

    #[test]
    fn claude_launch_attempts_include_npx_fallback() {
        let args = vec!["--print".to_owned(), "hello".to_owned()];
        let attempts = build_ai_launch_attempts("claude", &args);
        assert!(attempts.iter().any(|attempt| {
            attempt.program == "npx"
                && attempt.args.len() >= 3
                && attempt.args[0] == "-y"
                && attempt.args[1] == "@anthropic-ai/claude-code"
                && attempt.args[2] == "--print"
        }));
    }

    #[test]
    fn install_hints_match_noninteractive_modes() {
        let codex = ai_install_hint("codex").expect("codex hint should exist");
        assert!(codex.contains("codex exec"));

        let claude = ai_install_hint("claude").expect("claude hint should exist");
        assert!(claude.contains("claude --print"));
    }

    #[test]
    fn prompt_routes_to_stdin_for_claude() {
        let prompt = "hello";
        let args = vec!["--print".to_owned(), prompt.to_owned()];
        let (new_args, stdin_payload, routed) =
            prepare_ai_prompt_transport("claude", &args, Some(&prompt));
        assert!(routed);
        assert_eq!(new_args, vec!["--print".to_owned()]);
        assert_eq!(stdin_payload.as_deref(), Some(prompt));
    }

    #[test]
    fn prompt_routes_to_stdin_for_codex() {
        let prompt = "hello";
        let args = vec!["exec".to_owned(), prompt.to_owned()];
        let (new_args, stdin_payload, routed) =
            prepare_ai_prompt_transport("codex", &args, Some(&prompt));
        assert!(routed);
        assert_eq!(new_args, vec!["exec".to_owned(), "-".to_owned()]);
        assert_eq!(stdin_payload.as_deref(), Some(prompt));
    }

    #[test]
    fn claude_args_get_tab_scoped_session_id() {
        let args = vec!["--continue".to_owned(), "--print".to_owned()];
        let workspace = PathBuf::from("D:\\MyTerminal-c");
        let updated = ensure_claude_tab_scoped_session_args("claude", &args, 42, &workspace);
        assert_eq!(updated[0], "--session-id");
        assert_eq!(updated[2], "--continue");
        assert_eq!(updated[3], "--print");
        assert_eq!(updated.len(), 4);

        let session_id = &updated[1];
        assert_eq!(session_id.len(), 36);
        assert_eq!(session_id.chars().nth(8), Some('-'));
        assert_eq!(session_id.chars().nth(13), Some('-'));
        assert_eq!(session_id.chars().nth(18), Some('-'));
        assert_eq!(session_id.chars().nth(23), Some('-'));
    }

    #[test]
    fn claude_explicit_resume_is_not_overridden() {
        let args = vec!["--resume".to_owned(), "--print".to_owned()];
        let workspace = PathBuf::from("D:\\MyTerminal-c");
        let updated = ensure_claude_tab_scoped_session_args("claude", &args, 3, &workspace);
        assert_eq!(updated, args);
    }

    #[test]
    fn tab_scoped_session_id_is_stable_and_unique_per_tab() {
        let workspace = PathBuf::from("D:\\MyTerminal-c");
        let a = build_tab_scoped_claude_session_id(1, &workspace);
        let b = build_tab_scoped_claude_session_id(1, &workspace);
        let c = build_tab_scoped_claude_session_id(2, &workspace);
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn codex_info_stderr_is_filtered() {
        assert!(!should_emit_ai_stderr_line("codex", "OpenAI Codex v0.98.0"));
        assert!(!should_emit_ai_stderr_line("codex", "session id: abc"));
    }

    #[test]
    fn codex_error_stderr_is_kept() {
        assert!(should_emit_ai_stderr_line(
            "codex",
            "Error: Not inside a trusted directory"
        ));
    }

    #[test]
    fn non_codex_stderr_is_not_filtered() {
        assert!(should_emit_ai_stderr_line("claude", "some stderr line"));
        assert!(should_emit_ai_stderr_line("python", "random stderr"));
    }
}
