use anyhow::{anyhow, Context};
use cosmic_text::{
    fontdb::Database, Attrs, Buffer, Color, FontSystem, LayoutGlyph, Metrics, Shaping, SwashCache,
};
use indexmap::IndexMap;
use std::path::PathBuf;
use tiny_skia::{Paint, Pixmap, Rect, Transform};

use crate::{
    fonts::{FontDatabase, SYS_LOCALE},
    high::HWrite,
};

pub enum OutputFormat {
    WriterGeneral(Box<dyn HWrite>),
    WriterPicture(Box<dyn HWrite>),
    WriterTikZ(Box<dyn HWrite>),
    Image(PathBuf),
}

pub struct LayoutGeometry {
    pub height: f32,
    pub width: f32,
    pub glyphs: Vec<LayoutGlyph>,
}

pub struct Layout {
    pub font_system: FontSystem,
    pub swash_cache: SwashCache,
    pub text_buffer: Buffer,
    pub font_size: f32,   // in bp
    pub line_height: f32, // in bp
    pub text_width: f32,  // in bp
}

impl Layout {
    const RATIO: f32 = 2.0;

    pub fn new<S: AsRef<str>, T: IntoIterator<Item = S>>(fontdb: &FontDatabase, fonts: T) -> Self {
        let mut font_database = Database::new();

        for font in fonts.into_iter() {
            let font_str = font.as_ref();
            if fontdb.contains(font_str) {
                for font_file in fontdb.get_fontinfo(font_str) {
                    if let Err(_) = font_database.load_font_file(font_file.path.as_path()) {
                        log::warn!("Cannot load font file '{}'", font_file.path);
                    }
                }
            } else {
                log::trace!("Cannot find font '{}'", font_str);
            }
        }

        Layout {
            font_system: FontSystem::new_with_locale_and_db(
                SYS_LOCALE.as_str().to_string(),
                font_database,
            ),
            swash_cache: SwashCache::new(),
            text_buffer: Buffer::new_empty(Metrics::new(10.0 * Self::RATIO, 10.0 * Self::RATIO)),
            font_size: 0.0,
            line_height: 0.0,
            text_width: 0.0,
        }
    }

    pub fn layout(&mut self, text: &str) -> LayoutGeometry {
        let mut width = 0.0f32;
        let mut height = 0.0f32;
        let mut glyphs = vec![];

        let font_size = (self.font_size > 0.0)
            .then(|| self.font_size * Self::RATIO)
            .unwrap_or(10.0 * Self::RATIO);
        let line_height = (self.line_height > 0.0)
            .then(|| self.line_height * Self::RATIO)
            .unwrap_or(font_size);
        self.text_buffer.set_size(
            &mut self.font_system,
            (self.text_width > 0.0).then_some(self.text_width),
            None,
        );
        let metrics = Metrics::new(font_size, line_height);
        self.text_buffer.set_metrics(&mut self.font_system, metrics);
        let attrs = Attrs::new();
        self.text_buffer
            .set_text(&mut self.font_system, text, attrs, Shaping::Advanced);
        for run in self.text_buffer.layout_runs() {
            height += run.line_height;
            width = width.max(run.line_w);
            glyphs.extend_from_slice(run.glyphs);
        }

        LayoutGeometry {
            height,
            width,
            glyphs,
        }
    }

    pub fn output(
        &mut self,
        geometry: &LayoutGeometry,
        format: &mut OutputFormat,
    ) -> anyhow::Result<(), anyhow::Error> {
        use cosmic_text::fontdb::ID;
        fn get_fonts(geometry: &LayoutGeometry, db: &Database) -> (Vec<ID>, IndexMap<ID, String>) {
            let font_ids = geometry
                .glyphs
                .iter()
                .map(|v| v.font_id)
                .collect::<Vec<_>>();
            let mut fonts = IndexMap::new();
            for font in font_ids.iter() {
                if let Some(face) = db.face(*font) {
                    fonts.insert(
                        *font,
                        face.families
                            .first()
                            .map_or(face.post_script_name.to_owned(), |v| v.0.to_owned()),
                    );
                }
            }
            (font_ids, fonts)
        }

        match format {
            OutputFormat::WriterGeneral(stream) => {
                let (_, fonts) = get_fonts(geometry, self.font_system.db());
                let fonts_list_str = format!(
                    "\\TeXHighFontLists{{{}}}",
                    fonts
                        .values()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(",")
                );
                writeln!(stream, "{}", &fonts_list_str)
                    .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                writeln!(
                    stream,
                    "\\TeXHighSize{{{:.4}}}{{{:.4}}}",
                    geometry.width / Self::RATIO,
                    geometry.height / Self::RATIO
                )
                .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                for glyph in &geometry.glyphs {
                    writeln!(
                        stream,
                        "\\TeXHighGlyphEntry{{{:.4}}}{{{:.4}}}{{{:.4}}}{{{:.4}}}{{{:.4}}}{{{:.4}}}{{{:.4}}}{{{}}}{{{}}}",
                        glyph.x / Self::RATIO,
                        glyph.y / Self::RATIO,
                        glyph.w / Self::RATIO,
                        glyph.x_offset / Self::RATIO,
                        glyph.y_offset / Self::RATIO,
                        glyph.font_size / Self::RATIO,
                        glyph.line_height_opt.unwrap_or(glyph.font_size) / Self::RATIO,
                        fonts.get_index_of(&glyph.font_id).unwrap() + 1,
                        glyph.glyph_id,
                    )
                    .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                }
            }
            OutputFormat::WriterPicture(stream) => {
                let (_, fonts) = get_fonts(geometry, self.font_system.db());
                let fonts_list_str = format!(
                    "\\TeXHighFontLists{{{}}}",
                    fonts
                        .values()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(",")
                );
                writeln!(stream, "{}", &fonts_list_str)
                    .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                writeln!(
                    stream,
                    "\\begin{{picture}}({:.4}bp,{:.4}bp)",
                    geometry.width / Self::RATIO,
                    geometry.height / Self::RATIO
                )
                .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                for glyph in &geometry.glyphs {
                    writeln!(
                        stream,
                        "\\put({:.4}bp,{:.4}bp){{\\fontsize{{{:.2}}}{{{:.2}}}\\TeXHighFont{{{}}}\\TeXHighGlyph{}}}",
                        glyph.x / Self::RATIO,
                        glyph.y / Self::RATIO,
                        glyph.font_size / Self::RATIO,
                        glyph.line_height_opt.unwrap_or(glyph.font_size) / Self::RATIO,
                        fonts.get_index_of(&glyph.font_id).unwrap() + 1,
                        glyph.glyph_id,
                    )
                    .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
                }
                writeln!(stream, "\\end{{picture}}")
                    .map_err(|e| anyhow!("Cannot write to stream, cause {:?}", e))?;
            }
            OutputFormat::WriterTikZ(_stream) => {}
            OutputFormat::Image(file_path) => {
                let file_ext = file_path.extension().context(format!(
                    "Unable to find extension of '{}'",
                    file_path.display()
                ))?;
                if !matches!(file_ext.to_str(), Some("png")) {
                    return Err(anyhow!(
                        "Unsupported file type '{}'",
                        file_ext.to_str().unwrap()
                    ));
                }

                let width = (geometry.width + 0.5).round() as u32;
                let height = (geometry.height + 0.5).round() as u32;
                let mut pixmap = Pixmap::new(width, height).unwrap();
                let mut paint = Paint::default();
                paint.anti_alias = false;
                let transform = Transform::identity();
                self.text_buffer.draw(
                    &mut self.font_system,
                    &mut self.swash_cache,
                    Color::rgb(0, 0, 0),
                    |x, y, w, h, col| {
                        paint.set_color_rgba8(col.r(), col.g(), col.b(), col.a());
                        pixmap.fill_rect(
                            Rect::from_xywh(x as f32, y as f32, w as f32, h as f32).unwrap(),
                            &paint,
                            transform,
                            None,
                        );
                    },
                );

                pixmap.save_png(file_path.as_path()).context(format!(
                    "Unable to write file '{}'",
                    file_path.as_path().display()
                ))?;
            }
        }

        anyhow::Ok(())
    }

    pub fn add_font_files<S: AsRef<str>, T: IntoIterator<Item = S>>(
        &mut self,
        fonts: T,
    ) -> &mut Self {
        for font in fonts.into_iter() {
            if let Err(_) = self.font_system.db_mut().load_font_file(font.as_ref()) {
                log::warn!("Cannot load font file '{}'", font.as_ref());
            }
        }
        self
    }

    pub fn add_system_fonts(&mut self) -> &mut Self {
        self.font_system.db_mut().load_system_fonts();
        self
    }
}
