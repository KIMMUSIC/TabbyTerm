use eframe::egui;
use std::fs;

const NERD_FONT_NAME: &str = "symbols-nerd-font-mono";
const NERD_FONT_SYMBOLS_ONLY: &[u8] =
    include_bytes!("../../../../assets/fonts/SymbolsNerdFontMono-Regular.ttf");
const KOREAN_FALLBACK_FONT_NAME: &str = "windows-malgun-gothic";
const KOREAN_FALLBACK_FONT_PATHS: &[&str] = &["C:\\Windows\\Fonts\\malgun.ttf"];

pub const BG_APP: egui::Color32 = egui::Color32::from_rgb(30, 30, 30);
pub const BG_SURFACE_0: egui::Color32 = egui::Color32::from_rgb(37, 37, 38);
pub const BG_SURFACE_1: egui::Color32 = egui::Color32::from_rgb(45, 45, 48);
pub const BG_SURFACE_2: egui::Color32 = egui::Color32::from_rgb(56, 56, 61);
pub const BORDER: egui::Color32 = egui::Color32::from_rgb(62, 62, 66);
pub const TEXT_PRIMARY: egui::Color32 = egui::Color32::from_rgb(212, 212, 212);
pub const TEXT_MUTED: egui::Color32 = egui::Color32::from_rgb(133, 133, 133);
pub const TEXT_BRIGHT: egui::Color32 = egui::Color32::from_rgb(236, 236, 236);
pub const ACCENT_BLUE: egui::Color32 = egui::Color32::from_rgb(224, 161, 92);
pub const ACCENT_BLUE_SOFT: egui::Color32 = egui::Color32::from_rgb(89, 58, 32);
pub const SUCCESS: egui::Color32 = egui::Color32::from_rgb(106, 153, 85);
pub const WARNING: egui::Color32 = egui::Color32::from_rgb(206, 145, 120);
pub const ERROR: egui::Color32 = egui::Color32::from_rgb(244, 135, 113);

pub fn apply(ctx: &egui::Context) {
    apply_fonts(ctx);

    let mut visuals = egui::Visuals::dark();
    visuals.override_text_color = Some(TEXT_PRIMARY);
    visuals.panel_fill = BG_APP;
    visuals.window_fill = BG_SURFACE_0;
    visuals.window_stroke = egui::Stroke::new(1.0, BORDER);
    visuals.faint_bg_color = BG_SURFACE_0;
    visuals.extreme_bg_color = BG_APP;
    visuals.code_bg_color = BG_SURFACE_1;
    visuals.hyperlink_color = ACCENT_BLUE;
    visuals.warn_fg_color = WARNING;
    visuals.error_fg_color = ERROR;

    visuals.selection.bg_fill = ACCENT_BLUE;
    visuals.selection.stroke = egui::Stroke::new(1.0, TEXT_BRIGHT);

    visuals.widgets.noninteractive.bg_fill = BG_SURFACE_0;
    visuals.widgets.noninteractive.weak_bg_fill = BG_SURFACE_0;
    visuals.widgets.noninteractive.bg_stroke = egui::Stroke::new(1.0, BORDER);
    visuals.widgets.noninteractive.fg_stroke = egui::Stroke::new(1.0, TEXT_PRIMARY);

    visuals.widgets.inactive.bg_fill = BG_SURFACE_1;
    visuals.widgets.inactive.weak_bg_fill = BG_SURFACE_1;
    visuals.widgets.inactive.bg_stroke = egui::Stroke::new(1.0, BORDER);
    visuals.widgets.inactive.fg_stroke = egui::Stroke::new(1.0, TEXT_PRIMARY);

    visuals.widgets.hovered.bg_fill = BG_SURFACE_2;
    visuals.widgets.hovered.weak_bg_fill = BG_SURFACE_2;
    visuals.widgets.hovered.bg_stroke = egui::Stroke::new(1.0, ACCENT_BLUE);
    visuals.widgets.hovered.fg_stroke = egui::Stroke::new(1.0, TEXT_BRIGHT);

    visuals.widgets.active.bg_fill = ACCENT_BLUE_SOFT;
    visuals.widgets.active.weak_bg_fill = ACCENT_BLUE_SOFT;
    visuals.widgets.active.bg_stroke = egui::Stroke::new(1.0, ACCENT_BLUE);
    visuals.widgets.active.fg_stroke = egui::Stroke::new(1.0, TEXT_BRIGHT);

    visuals.widgets.open.bg_fill = BG_SURFACE_2;
    visuals.widgets.open.weak_bg_fill = BG_SURFACE_2;
    visuals.widgets.open.bg_stroke = egui::Stroke::new(1.0, ACCENT_BLUE);
    visuals.widgets.open.fg_stroke = egui::Stroke::new(1.0, TEXT_PRIMARY);

    let mut style = (*ctx.style()).clone();
    style.visuals = visuals;
    style.spacing.item_spacing = egui::vec2(8.0, 8.0);
    style.spacing.button_padding = egui::vec2(10.0, 5.0);
    style.spacing.interact_size = egui::vec2(48.0, 24.0);
    style.spacing.indent = 16.0;

    style
        .text_styles
        .insert(egui::TextStyle::Heading, egui::FontId::monospace(18.0));
    style
        .text_styles
        .insert(egui::TextStyle::Body, egui::FontId::monospace(14.0));
    style
        .text_styles
        .insert(egui::TextStyle::Button, egui::FontId::monospace(13.0));
    style
        .text_styles
        .insert(egui::TextStyle::Monospace, egui::FontId::monospace(14.0));
    style
        .text_styles
        .insert(egui::TextStyle::Small, egui::FontId::monospace(12.0));

    ctx.set_style(style);
}

fn apply_fonts(ctx: &egui::Context) {
    let mut fonts = egui::FontDefinitions::default();
    fonts.font_data.insert(
        NERD_FONT_NAME.to_owned(),
        egui::FontData::from_static(NERD_FONT_SYMBOLS_ONLY).into(),
    );
    if let Some(korean_font) = load_first_font_bytes(KOREAN_FALLBACK_FONT_PATHS) {
        fonts.font_data.insert(
            KOREAN_FALLBACK_FONT_NAME.to_owned(),
            egui::FontData::from_owned(korean_font).into(),
        );
    }

    if let Some(mono_family) = fonts.families.get_mut(&egui::FontFamily::Monospace) {
        mono_family.insert(0, NERD_FONT_NAME.to_owned());
        if fonts.font_data.contains_key(KOREAN_FALLBACK_FONT_NAME) {
            mono_family.push(KOREAN_FALLBACK_FONT_NAME.to_owned());
        }
    }

    if let Some(prop_family) = fonts.families.get_mut(&egui::FontFamily::Proportional) {
        prop_family.insert(0, NERD_FONT_NAME.to_owned());
        if fonts.font_data.contains_key(KOREAN_FALLBACK_FONT_NAME) {
            prop_family.push(KOREAN_FALLBACK_FONT_NAME.to_owned());
        }
    }

    ctx.set_fonts(fonts);
}

fn load_first_font_bytes(paths: &[&str]) -> Option<Vec<u8>> {
    for path in paths {
        if let Ok(bytes) = fs::read(path) {
            return Some(bytes);
        }
    }
    None
}

pub fn panel_frame() -> egui::Frame {
    egui::Frame::new()
        .fill(BG_SURFACE_0)
        .stroke(egui::Stroke::new(1.0, BORDER))
        .corner_radius(egui::CornerRadius::same(8))
        .inner_margin(egui::Margin::same(8))
}

pub fn elevated_frame() -> egui::Frame {
    egui::Frame::new()
        .fill(BG_SURFACE_1)
        .stroke(egui::Stroke::new(1.0, BORDER))
        .corner_radius(egui::CornerRadius::same(8))
        .inner_margin(egui::Margin::same(8))
}

pub fn card_frame() -> egui::Frame {
    egui::Frame::new()
        .fill(BG_SURFACE_0)
        .stroke(egui::Stroke::new(1.0, BORDER))
        .corner_radius(egui::CornerRadius::same(6))
        .inner_margin(egui::Margin::same(8))
}

pub fn toolbar_frame() -> egui::Frame {
    egui::Frame::new()
        .fill(BG_SURFACE_1)
        .stroke(egui::Stroke::new(1.0, BORDER))
        .corner_radius(egui::CornerRadius::same(6))
        .inner_margin(egui::Margin::symmetric(10, 8))
}
