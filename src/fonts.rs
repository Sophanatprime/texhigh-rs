use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashSet},
    env::current_exe,
    fmt::Display,
    fs::File,
    io::{self, BufReader, BufWriter, Read},
    path::{Path, PathBuf},
};

use anstyle::Style;
use anyhow::{self, Context};
use compact_str::{CompactString, ToCompactString};
use dirs::data_dir;
use indexmap::{IndexMap, IndexSet};
use lazy_static::lazy_static;
use log;
use rapidfuzz;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{
    self,
    ser::{Formatter, PrettyFormatter},
    Serializer,
};
use skrifa::{
    raw::{
        tables::cmap::{CmapSubtable, PlatformId},
        types::NameId,
        CollectionRef, TableProvider,
    },
    FontRef, MetadataProvider,
};
use textwrap::{termwidth, wrap as text_wrap, Options as WrapOptions};
use walkdir::WalkDir;

use crate::unicode::{get_char_block_name, UNICODE_BLOCKS};

lazy_static! {
    /// If AppDataDirectory can be accessed, then use this directory.
    /// Otherwise, use the use the real directory of the current executable.
    pub static ref DEFAULT_DATA_PATH: Option<PathBuf> = data_dir().or(match current_exe() {
        Ok(exe) => match exe.read_link().unwrap_or(exe).parent() {
            Some(parent) => Some(parent.to_path_buf()),
            None => None,
        },
        Err(_) => None,
    });
}

/// Calculate the similarity between a and b, the value is in [0.0, 1.0].
/// The similarity will be 1.0, if these two values are identical.
pub fn similarity(a: &str, b: &str) -> f64 {
    similarity_bytes(a.as_bytes(), b.as_bytes())
}
pub fn similarity_bytes(a: &[u8], b: &[u8]) -> f64 {
    rapidfuzz::fuzz::ratio(a, b)
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct FontDatabase {
    // The file may contian duplicates.
    pub(crate) file: Vec<FontFile>,
    pub(crate) link: FontLinks,
    pub(crate) fontset: FontSets,
    pub(crate) file_count: usize,
    pub(crate) link_count: usize,
    pub(crate) link_hash: Vec<FontLinkHash>,
    pub(crate) fontset_hash: Vec<FontSetHash>,
}

impl FontDatabase {
    /// Walk through paths and read all fonts to build database.
    pub fn new_from_paths<T: AsRef<Path>, P: IntoIterator<Item = T>>(paths: P) -> Self {
        let mut dbs = FontDatabase::default();
        paths.into_iter().for_each(|p| dbs.add_items_from_path(p));
        dbs
    }
    /// Parse database from a json file.
    pub fn from_json_file<T: AsRef<Path>>(path: T) -> anyhow::Result<Self> {
        let file = File::open(path.as_ref()).context(format!(
            "Cannot read database file '{}'",
            path.as_ref().display()
        ))?;
        let reader = BufReader::new(file);
        let mut dbs: FontDatabase = serde_json::from_reader(reader).context(format!(
            "Cannot parse database file '{}'",
            path.as_ref().display()
        ))?;
        match dbs.link {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let link = std::mem::take(&mut dbs.link).into_map();
                println!("LLL: {}", link.len());
                dbs.link = link;
            }
        }
        match dbs.fontset {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let fontset = std::mem::take(&mut dbs.fontset).into_map();
                dbs.fontset = fontset;
            }
        }
        Ok(dbs)
    }

    pub fn to_database_ref(&self) -> FontDatabaseRef {
        FontDatabaseRef {
            file: &self.file,
            link: self.link.to_vec_ref(),
            fontset: self.fontset.to_vec_ref(),
            file_count: self.file_count,
            link_count: self.link_count,
            link_hash: &self.link_hash,
            fontset_hash: &self.fontset_hash,
        }
    }

    /// Save FontDataBase to a json file.
    ///
    /// If `ascii` is true, then escapes all non-ASCII characters.
    pub fn save_to_file<T: AsRef<Path>>(&self, filename: T, ascii: bool) -> anyhow::Result<()> {
        let file = File::create(filename.as_ref())
            .context(format!("Cannot open file [{:?}]", filename.as_ref()))?;
        let writer = BufWriter::new(file);
        if ascii {
            let mut ser = Serializer::with_formatter(writer, EscapedFormatter::new());
            self.serialize(&mut ser).context(format!(
                "Cannot write to file '{}'",
                filename.as_ref().display()
            ))?;
        } else {
            serde_json::to_writer_pretty(writer, self).context(format!(
                "Cannot write to file '{}'",
                filename.as_ref().display()
            ))?;
        }
        Ok(())
    }

    /// Save FontDatabase to the datadir with `filename` as a json file.
    ///
    /// Return the real filename.
    ///
    /// If `ascii` is true, then escapes all non-ASCII characters.
    pub fn save<T: AsRef<Path>>(&self, filename: T, ascii: bool) -> anyhow::Result<PathBuf> {
        let dir = DEFAULT_DATA_PATH
            .as_ref()
            .context("Cannot access the default data path")?;
        let filename = filename.as_ref().file_name().context(format!(
            "Not a valid filename '{}'",
            filename.as_ref().display()
        ))?;
        let path = dir.join(filename);
        self.save_to_file(&path, ascii)?;
        Ok(path)
    }

    /// Add a font to database.
    pub fn add_font<P: AsRef<Path>>(&mut self, path: P) {
        match self.link {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let link = std::mem::take(&mut self.link).into_map();
                self.link = link;
            }
        }
        match self.fontset {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let fontset = std::mem::take(&mut self.fontset).into_map();
                self.fontset = fontset;
            }
        }
        self.add_font_impl(path);
    }

    /// Add fonts to database.
    pub fn add_fonts<T, P>(&mut self, paths: P)
    where
        T: AsRef<Path>,
        P: IntoIterator<Item = T>,
    {
        match self.link {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let link = std::mem::take(&mut self.link).into_map();
                self.link = link;
            }
        }
        match self.fontset {
            FontLinks::Map(_) => {}
            FontLinks::Vec(_) => {
                let fontset = std::mem::take(&mut self.fontset).into_map();
                self.fontset = fontset;
            }
        }
        paths.into_iter().for_each(|p| self.add_items_from_path(p));
    }

    /// Retrieves an iterator instance that yields a `&FontFile` from given font name,
    /// which may include duplicate `FontFile`s.
    ///
    /// This is much slower than `FontDatabase::contains` for a large database,
    /// detecting the existence of the font is recommended.
    pub fn get_fontinfo(&self, name: &str) -> FileIter<'_> {
        FileIter {
            database: self,
            link_index: self.link.get_index_by_key(name).unwrap_or(usize::MAX),
            inst_index: 0,
        }
    }

    /// Get FontFile from a fuzzy font name.
    /// Retrieves an iterator instance that yields a `(&FontFile, f64)`,
    /// which may include duplicate `FontFile`s.
    pub fn get_fontinfo_fuzzy(&self, name: &str, score: f64) -> FileFuzzyIter<'_> {
        FileFuzzyIter {
            database: self,
            link_iter: self.link.fuzzy_search(name, score).into_iter(),
            inst_index: None,
            inner_index: 0,
            current_score: 0.0,
        }
    }

    /// Return true if font name is in Font Database.
    pub fn contains(&self, name: &str) -> bool {
        match &self.link {
            FontLinks::Map(map) => map.contains_key(name),
            FontLinks::Vec(vec) => vec.par_iter().find_any(|&v| v.name == name).is_some(),
        }
    }

    pub fn file(&self) -> &Vec<FontFile> {
        &self.file
    }
    pub fn link(&self) -> &FontLinks {
        &self.link
    }
    pub fn fontset(&self) -> &FontSets {
        &self.fontset
    }
    pub fn file_count(&self) -> usize {
        self.file_count
    }
    pub fn link_count(&self) -> usize {
        self.link_count
    }
    pub fn link_hash(&self) -> &Vec<FontLinkHash> {
        &self.link_hash
    }
    pub fn fontset_hash(&self) -> &Vec<FontSetHash> {
        &self.fontset_hash
    }

    fn add_items_from_path(&mut self, path: impl AsRef<Path>) {
        for p in WalkDir::new(path).follow_links(true) {
            match p {
                Ok(entry) => {
                    if entry.file_type().is_file() {
                        self.add_font_impl(entry.path());
                    } else {
                        log::trace!(
                            "Ignoring path '{}', cause not a file",
                            entry.path().display()
                        );
                    }
                }
                Err(err) => {
                    let p = err.path().unwrap_or(Path::new("")).display();
                    if let Some(io_err) = err.io_error() {
                        match io_err.kind() {
                            io::ErrorKind::PermissionDenied => {
                                log::warn!("Missing permission to read '{}'", &p);
                            }
                            _ => {
                                log::info!("Fail to read '{}'", &p);
                            }
                        }
                    } else if let Some(p) = err.loop_ancestor() {
                        log::info!("Loop found when read '{}'", p.display());
                    } else {
                        log::info!("Encountering an error when read '{}'", &p);
                    }
                }
            }
        }
    }

    fn add_font_impl(&mut self, path: impl AsRef<Path>) {
        let is_collection;
        if let Some(extension) = path.as_ref().extension() {
            let extension = extension.to_str();
            if matches!(
                extension,
                Some("ttf") | Some("TTF") | Some("otf") | Some("OTF")
            ) {
                is_collection = false;
            } else if matches!(
                extension,
                Some("ttc") | Some("TTC") | Some("otc") | Some("OTC")
            ) {
                is_collection = true;
            } else {
                log::trace!(
                    "Ignoring path '{}', cause not a opentype font",
                    path.as_ref().display()
                );
                return;
            }
        } else {
            log::trace!(
                "Ignoring path '{}', cause no file extension",
                path.as_ref().display()
            );
            return;
        }
        let Ok(mut file) = File::open(path.as_ref()) else {
            log::warn!("Fail to read '{}'", path.as_ref().display());
            return;
        };

        let mut data = vec![];
        let Ok(_) = file.read_to_end(&mut data) else {
            log::warn!("Fail to read '{}'", path.as_ref().display());
            return;
        };

        let faces_count = if is_collection {
            match CollectionRef::new(&data) {
                Ok(cr) => cr.len(),
                Err(_) => {
                    log::warn!("Fail to parse '{}", path.as_ref().display());
                    return;
                }
            }
        } else {
            1
        };
        for index in 0..faces_count {
            let filename = path.as_ref().to_string_lossy().to_string();
            parse_font(&mut self.file, filename, &data, index);
        }

        match &mut self.link {
            FontLinks::Map(map) => self
                .file
                .iter()
                .enumerate()
                .for_each(|(index, file)| parse_link(map, file, index as u32)),
            FontLinks::Vec(_) => unreachable!(),
        }
        match &mut self.fontset {
            FontLinks::Map(map) => self
                .file
                .iter()
                .enumerate()
                .for_each(|(index, file)| parse_fontset(map, file, index as u32)),
            FontLinks::Vec(_) => unreachable!(),
        }

        self.file_count = self.file.len();
        self.link_count = self.link.len();
    }
}

fn parse_font(fontfiles: &mut Vec<FontFile>, filename: String, data: &[u8], index: u32) {
    #[cfg(target_os = "windows")]
    let filename = filename.replace('\\', "/");

    let Ok(face) = FontRef::from_index(data, index) else {
        log::warn!("Fail to parse '{}'", &filename);
        return;
    };
    let Ok(name_table) = face.name() else {
        log::warn!("Fail to parse '{}'", &filename);
        return;
    };
    let string_data = name_table.string_data();

    let mut name_records = Vec::new();
    for record in name_table.name_record() {
        let Ok(name_str) = record.string(string_data) else {
            continue;
        };
        let name = name_str.chars().collect::<CompactString>();
        if name.is_empty() {
            continue;
        }
        name_records.push((record.name_id.get(), name));
    }

    let mut family = IndexSet::new();
    let mut style = IndexSet::new();
    let mut full = IndexSet::new();
    let mut postscript = IndexSet::new();
    let mut prefer_family = IndexSet::new();
    let mut prefer_style = IndexSet::new();
    for (name_id, name_str) in &name_records {
        let name_str = name_str.to_string();
        match *name_id {
            NameId::FAMILY_NAME => family.insert(name_str),
            NameId::SUBFAMILY_NAME => style.insert(name_str),
            NameId::FULL_NAME => full.insert(name_str),
            NameId::POSTSCRIPT_NAME => {
                // postscript name should be restricted to the set of printable ASCII
                // characters (U+0021 through U+007E, no U+20 space ' '),
                // less the ten characters '[', ']', '(', ')', '{', '}', '<', '>', '/', and '%'.
                // Some fonts may contain spaces, we here remove it, but keep the rest unchanged.
                if name_str.contains(' ') {
                    postscript.insert(name_str.replace(' ', ""));
                }
                postscript.insert(name_str)
            }
            NameId::TYPOGRAPHIC_FAMILY_NAME => prefer_family.insert(name_str),
            NameId::TYPOGRAPHIC_SUBFAMILY_NAME => prefer_style.insert(name_str),
            _ => false,
        };
    }

    let mut inst_style = Vec::new();
    let mut inst_postscript = Vec::new();
    let mut inst_tuple = Vec::new();
    fn get_fvar_name(name_records: &Vec<(NameId, CompactString)>, id: NameId) -> Option<String> {
        for (name_id, name_str) in name_records {
            if *name_id == id {
                return Some(name_str.to_string());
            }
        }
        None
    }
    if let Ok(fvar_table) = face.fvar() {
        if let Ok(instances) = fvar_table.instances() {
            for (idx, instance) in instances.iter().enumerate() {
                let idx = idx as u32;
                let Ok(instance) = instance else {
                    continue;
                };
                // inst_style
                let id = instance.subfamily_name_id;
                match get_fvar_name(&name_records, id) {
                    Some(name_str) => inst_style.push((name_str, idx)),
                    None => {}
                }
                // inst_postscript
                if let Some(id) = instance.post_script_name_id {
                    match get_fvar_name(&name_records, id) {
                        Some(name_str) => inst_postscript.push((name_str, idx)),
                        None => {}
                    }
                }
                // inst_tuple
                inst_tuple.push(
                    instance
                        .coordinates
                        .iter()
                        .map(|v| v.get().to_f32())
                        .collect::<Vec<_>>(),
                );
            }
        }
    }

    let fontfile = FontFile {
        path: FontPath(filename),
        index,
        family: Vec::from_iter(family.into_iter()),
        style: Vec::from_iter(style.into_iter()),
        full: Vec::from_iter(full.into_iter()),
        postscript: Vec::from_iter(postscript.into_iter()),
        prefer_family: Vec::from_iter(prefer_family.into_iter()),
        prefer_style: Vec::from_iter(prefer_style.into_iter()),
        inst_style,
        inst_postscript,
        inst_tuple,
    };

    fontfiles.push(fontfile);
}

fn parse_link(fontlinks: &mut IndexMap<String, Vec<u32>>, file: &FontFile, index: u32) {
    fn join_name<'a, T1, T2>(fl: T1, sl: T2) -> Vec<String>
    where
        T1: IntoIterator<Item = &'a String>,
        T1::IntoIter: Clone,
        T2: IntoIterator<Item = &'a String>,
        T2::IntoIter: Clone,
    {
        let mut res: Vec<String> = Vec::new();
        let fl = fl.into_iter();
        let sl = sl.into_iter();
        fl.clone().for_each(|v| res.push(v.to_owned()));
        fl.clone().for_each(|f| {
            sl.clone().into_iter().for_each(|s| {
                let r = format!("{}-{}", f, s);
                if !res.contains(&r) {
                    res.push(r);
                }
            });
        });
        res
    }
    fn store(fontlinks: &mut IndexMap<String, Vec<u32>>, key: &str, value: u32) {
        if !fontlinks.contains_key(key) {
            fontlinks.insert(key.to_owned(), Vec::new());
        }
        let this_link = fontlinks.get_mut(key).unwrap();
        if !this_link.contains(&value) {
            this_link.push(value);
        }
    }

    let FontFile {
        family,
        style,
        prefer_family,
        prefer_style,
        inst_style,
        ..
    } = file;

    file.inst_postscript
        .iter()
        .for_each(|(name, _)| store(fontlinks, name, index));
    file.postscript
        .iter()
        .for_each(|name| store(fontlinks, name, index));
    file.full
        .iter()
        .for_each(|name| store(fontlinks, name, index));

    let inst_names = join_name(family, inst_style.iter().map(|(name, _)| name));
    inst_names
        .iter()
        .for_each(|name| store(fontlinks, name, index));
    let names = if !prefer_family.is_empty() && !prefer_style.is_empty() {
        join_name(prefer_family, prefer_style)
    } else if !family.is_empty() && !style.is_empty() {
        join_name(family, style)
    } else {
        Vec::new()
    };
    names.iter().for_each(|name| store(fontlinks, name, index));
}

fn parse_fontset(fontsets: &mut IndexMap<String, Vec<u32>>, file: &FontFile, index: u32) {
    let run = if file.prefer_family.is_empty() {
        &file.family
    } else {
        &file.prefer_family
    };
    run.iter().for_each(|name| {
        if !fontsets.contains_key(name) {
            fontsets.insert(name.to_owned(), Vec::new());
        }
        let this_set = fontsets.get_mut(name).unwrap();
        if !this_set.contains(&index) {
            this_set.push(index);
        }
    });
}

#[derive(Debug, Serialize)]
pub struct FontDatabaseRef<'a, 'b> {
    pub(crate) file: &'a Vec<FontFile>,
    pub(crate) link: Vec<FontLinkRef<'b>>,
    pub(crate) fontset: Vec<FontSetRef<'b>>,
    pub(crate) file_count: usize,
    pub(crate) link_count: usize,
    pub(crate) link_hash: &'a Vec<FontLinkHash>,
    pub(crate) fontset_hash: &'a Vec<FontSetHash>,
}

impl<'a, 'b> FontDatabaseRef<'a, 'b> {
    /// Save FontDataBaseRef to a json file.
    ///
    /// If `ascii` is true, then escapes all non-ASCII characters.
    pub fn save_to_file<T: AsRef<Path>>(&self, filename: T, ascii: bool) -> anyhow::Result<()> {
        let file = File::create(filename.as_ref())
            .context(format!("Cannot open file [{:?}]", filename.as_ref()))?;
        let writer = BufWriter::new(file);
        if ascii {
            let mut ser = Serializer::with_formatter(writer, EscapedFormatter::new());
            self.serialize(&mut ser).context(format!(
                "Cannot write to file '{}'",
                filename.as_ref().display()
            ))?;
        } else {
            serde_json::to_writer_pretty(writer, self).context(format!(
                "Cannot write to file '{}'",
                filename.as_ref().display()
            ))?;
        }
        Ok(())
    }

    /// Save FontDatabaseRef to the datadir with `filename` as a json file.
    ///
    /// Return the real filename.
    ///
    /// If `ascii` is true, then escapes all non-ASCII characters.
    pub fn save<T: AsRef<Path>>(&self, filename: T, ascii: bool) -> anyhow::Result<PathBuf> {
        let dir = DEFAULT_DATA_PATH
            .as_ref()
            .context("Cannot access the default data path")?;
        let filename = filename.as_ref().file_name().context(format!(
            "Not a valid filename '{}'",
            filename.as_ref().display()
        ))?;
        let path = dir.join(filename);
        self.save_to_file(&path, ascii)?;
        Ok(path)
    }

    pub fn file(&self) -> &Vec<FontFile> {
        self.file
    }
    pub fn link(&self) -> &Vec<FontLinkRef> {
        &self.link
    }
    pub fn fontset(&self) -> &Vec<FontSetRef> {
        &self.fontset
    }
    pub fn file_count(&self) -> usize {
        self.file_count
    }
    pub fn link_count(&self) -> usize {
        self.link_count
    }
    pub fn link_hash(&self) -> &Vec<FontLinkHash> {
        self.link_hash
    }
    pub fn fontset_hash(&self) -> &Vec<FontSetHash> {
        self.fontset_hash
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FontFile {
    pub path: FontPath,
    pub index: u32,
    pub family: Vec<String>,
    pub style: Vec<String>,
    pub full: Vec<String>,
    pub postscript: Vec<String>,
    pub prefer_family: Vec<String>,
    pub prefer_style: Vec<String>,
    pub inst_style: Vec<(String, u32)>,
    pub inst_postscript: Vec<(String, u32)>,
    pub inst_tuple: Vec<Vec<f32>>,
}

impl FontFile {
    pub fn display_short(&self) -> String {
        format!(
            "{:#}",
            FontFileFormatter {
                file: self,
                max_list_len: 2,
                show_inst: false,
                read_file: false,
                very_simple: true,
                wrapped: false,
                borderless: true,
            }
        )
    }
    pub fn display_simple(&self) -> String {
        format!(
            "{:#}",
            FontFileFormatter {
                file: self,
                max_list_len: 2,
                show_inst: false,
                read_file: false,
                very_simple: false,
                wrapped: false,
                borderless: true,
            }
        )
    }
    pub fn display_full(&self) -> String {
        format!(
            "{:#}",
            FontFileFormatter {
                file: self,
                max_list_len: usize::MAX,
                show_inst: true,
                read_file: false,
                very_simple: false,
                wrapped: false,
                borderless: true,
            }
        )
    }
    pub fn display_info(&self) -> String {
        self.display_info_with(true, false)
    }
    pub fn display_info_with(&self, wrapped: bool, borderless: bool) -> String {
        format!(
            "{}",
            FontFileFormatter {
                file: self,
                max_list_len: usize::MAX,
                show_inst: true,
                read_file: true,
                very_simple: false,
                wrapped,
                borderless,
            }
        )
    }
}

struct FontFileNameFormatter<'a>(&'a Vec<String>, usize);
impl<'a> Display for FontFileNameFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.len() == 0 || self.1 == 0 {
            return write!(f, "[]");
        }
        if self.1 < 4 {
            write!(f, "[ ")?;
            let max_dotted = self.1.min(self.0.len()) - 1;
            for i in 0..max_dotted {
                write!(f, "{} , ", self.0[i])?;
            }
            write!(f, "{}", self.0[max_dotted])?;
            write!(f, "{} ]", if self.1 < self.0.len() { " , .." } else { "" })?;
            return Ok(());
        }
        let mut dl = f.debug_list();
        if self.0.len() == 0 {
            return dl.finish();
        }
        if self.1 < self.0.len() {
            dl.entries(&self.0[0..self.1]);
            dl.finish_non_exhaustive()
        } else {
            dl.entries(self.0);
            dl.finish()
        }
    }
}
struct FontFileFormatter<'a> {
    file: &'a FontFile,
    max_list_len: usize,
    show_inst: bool,
    read_file: bool,
    very_simple: bool,
    wrapped: bool,
    borderless: bool,
}
impl<'a> FontFileFormatter<'a> {
    fn display_font_file(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let textwidth = if self.wrapped {
            termwidth()
        } else {
            usize::MAX
        };
        display_font_file(
            f,
            self.file.path.as_path(),
            self.file.index,
            textwidth,
            true,
            self.borderless,
        )
    }

    fn display_font_full(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("Font Face");

        ds.field_with("Path", |fi| write!(fi, "{}", &self.file.path));
        ds.field("Face Index", &self.file.index);

        let data = [
            ("Family", &self.file.family),
            ("Style", &self.file.style),
            ("Full Name", &self.file.full),
            ("Postscript Names", &self.file.postscript),
            ("Prefer Family", &self.file.prefer_family),
            ("Prefer Style", &self.file.prefer_style),
        ];
        let len = if self.very_simple { 1 } else { data.len() };
        for (name, value) in &data[0..len] {
            ds.field_with(name, |fi| {
                FontFileNameFormatter(value, self.max_list_len).fmt(fi)
            });
        }

        if self.show_inst {
            ds.field("Style (fvar table)", &self.file.inst_style);
            ds.field("Postscript names (fvar table)", &self.file.inst_postscript);
            ds.field("fvar Instance", &self.file.inst_tuple);
            ds.finish()
        } else {
            if !self.very_simple {
                ds.field("Variable Font", &!self.file.inst_style.is_empty());
            }
            ds.finish_non_exhaustive()
        }
    }
}
impl<'a> Display for FontFileFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.read_file {
            return self.display_font_file(f);
        } else {
            return self.display_font_full(f);
        }
    }
}

pub fn display_font_file_all_face<T: AsRef<Path>, W: std::fmt::Write>(
    f: &mut W,
    path: T,
    textwidth: usize,
    styled: bool,
    borderless: bool,
) -> std::fmt::Result {
    let path_str = &format!("{}", path.as_ref().to_string_lossy().replace('\\', "/"));
    let mut data = vec![];
    let Ok(mut file) = File::open(path.as_ref()) else {
        log::error!(
            "Cannot access '{}' when display font information.",
            path_str
        );
        return Err(std::fmt::Error);
    };
    file.read_to_end(&mut data).map_err(|_| {
        log::error!("Cannot read '{}' when display font information.", path_str);
        std::fmt::Error
    })?;
    let Some(extension) = path.as_ref().extension() else {
        log::error!("Cannot find extension of '{}'", path_str);
        return Err(std::fmt::Error);
    };
    let extension = &extension.to_string_lossy().to_ascii_lowercase();
    let indices = if matches!(extension.as_str(), "ttf" | "otf") {
        1
    } else {
        let Ok(cr) = CollectionRef::new(&data) else {
            log::error!("Cannot parse font file '{}'", path_str);
            return Err(std::fmt::Error);
        };
        cr.len()
    };
    for index in 0..indices {
        display_font_data(
            f,
            Some(path_str),
            &data,
            index,
            textwidth,
            styled,
            borderless,
        )?;
    }
    Ok(())
}

pub fn display_font_file<T: AsRef<Path>, W: std::fmt::Write>(
    f: &mut W,
    path: T,
    index: u32,
    textwidth: usize,
    styled: bool,
    borderless: bool,
) -> std::fmt::Result {
    let path_str = &format!("{}", path.as_ref().display());
    let mut data = vec![];
    let Ok(mut file) = File::open(path.as_ref()) else {
        log::error!(
            "Cannot access '{}' when display font information.",
            path_str
        );
        return Err(std::fmt::Error);
    };
    file.read_to_end(&mut data).map_err(|_| {
        log::error!("Cannot read '{}' when display font information.", path_str);
        std::fmt::Error
    })?;
    let path_str = format!("{}", path.as_ref().to_string_lossy().replace('\\', "/"));
    display_font_data(
        f,
        Some(&path_str),
        &data,
        index,
        textwidth,
        styled,
        borderless,
    )
}

pub fn display_font_data<W: std::fmt::Write>(
    f: &mut W,
    path: Option<&str>,
    data: &[u8],
    index: u32,
    textwidth: usize,
    styled: bool,
    borderless: bool,
) -> std::fmt::Result {
    let face = if path.is_some() {
        FontRef::from_index(&data, index).map_err(|_| {
            log::error!(
                "Cannot parse '{}' when display font information.",
                path.unwrap()
            );
            std::fmt::Error
        })?
    } else {
        FontRef::from_index(&data, index).map_err(|_| {
            log::error!("Cannot parse font data when display font information.");
            std::fmt::Error
        })?
    };

    assert!(textwidth > 10, "Terminal width is too small.");
    let labelwidth = (textwidth - 3).min(16);
    const TABLE_BORDER: &[&str; 7] = &["┌", "─", "┐", "└", "─", "┘", "┆"];
    let indent: &str = if textwidth < 20 {
        if borderless {
            "     "
        } else {
            "┆     "
        }
    } else {
        if borderless {
            "         "
        } else {
            "┆         "
        }
    };
    let mut labelstyle = Style::new();
    let mut infostyle = Style::new();
    if styled {
        labelstyle = labelstyle.bold();
        infostyle = infostyle.underline();
    }

    if path.is_some() {
        writeln!(f, "{infostyle}{}{infostyle:#}", path.unwrap())?; // Start display
    }

    let face_string = |id: NameId| match face.localized_strings(id).english_or_first() {
        Some(ls) => ls.chars().collect(),
        None => CompactString::new(""),
    };
    let border_line = &if borderless {
        CompactString::const_new("")
    } else {
        let mut s = CompactString::const_new(TABLE_BORDER[6]);
        s.push(' ');
        s
    };
    let mut print_data = |head: &str, text: &str| {
        if text.is_empty() {
            return Ok(());
        }
        if head.is_empty() {
            writeln!(f, "{}", text)?;
        } else {
            let head = if head.len() < labelwidth {
                format!(
                    "{}{labelstyle}{}:{labelstyle:#} {}",
                    border_line,
                    head,
                    " ".repeat(labelwidth - head.len())
                )
            } else {
                format!("{}{labelstyle}{}:{labelstyle:#} ", border_line, head)
            };
            let res = text_wrap(
                text,
                WrapOptions::new(textwidth)
                    .initial_indent(&head)
                    .subsequent_indent(indent),
            );
            for l in res {
                writeln!(f, "{}", l)?;
            }
        }
        Ok(())
    };

    // Border.
    if !borderless {
        print_data(
            "",
            &format!(
                "{}{}{}",
                TABLE_BORDER[0],
                TABLE_BORDER[1].repeat(textwidth - 2),
                TABLE_BORDER[2]
            ),
        )?;
    } // End of Border

    if path.is_some() {
        let path_str = path.unwrap();
        print_data("Path", path_str)?;
        if matches!(
            &path_str[(path_str.rfind('.').unwrap_or(0) + 1)..path_str.len()],
            "ttc" | "TTC" | "otc" | "OTC"
        ) {
            print_data("Face Index", &index.to_string())?;
        }
    } else {
        print_data("Face Index", &index.to_string())?;
    }
    print_data("Version", &face_string(NameId::VERSION_STRING))?;
    print_data("Unique ID", &face_string(NameId::UNIQUE_ID))?;
    print_data("Family", &face_string(NameId::FAMILY_NAME))?;
    print_data("Style", &face_string(NameId::SUBFAMILY_NAME))?;
    print_data("Full Name", &face_string(NameId::FULL_NAME))?;
    print_data("Postscript Name", &face_string(NameId::POSTSCRIPT_NAME))?;
    print_data(
        "Postscript CID Name",
        &face_string(NameId::POSTSCRIPT_CID_NAME),
    )?;
    print_data(
        "Prefer Family",
        &face_string(NameId::TYPOGRAPHIC_FAMILY_NAME),
    )?;
    print_data(
        "Prefer Style",
        &face_string(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
    )?;
    print_data("Description", &face_string(NameId::DESCRIPTION))?;
    print_data("Designer", &face_string(NameId::DESIGNER))?;
    print_data("Designer URL", &face_string(NameId::DESIGNER_URL))?;
    print_data("Manufacturer", &face_string(NameId::MANUFACTURER))?;
    print_data("Vendor URL", &face_string(NameId::VENDOR_URL))?;
    print_data("Trademark", &face_string(NameId::TRADEMARK))?;
    print_data("Copyright", &face_string(NameId::COPYRIGHT_NOTICE))?;
    print_data("License URL", &face_string(NameId::LICENSE_URL))?;
    print_data(
        "License Description",
        &face_string(NameId::LICENSE_DESCRIPTION),
    )?;

    // Font Labels.
    let (is_serif, is_sans_serif, is_symbol, is_decorative, is_hand_written) =
        if let Ok(os2) = face.os2() {
            let panose = OS2Panose::new(os2.panose_10());
            (
                panose.is_serif(),
                panose.is_sans_serif(),
                panose.is_symbol(),
                panose.is_decorative(),
                panose.is_hand_written(),
            )
        } else {
            (false, false, false, false, false)
        };
    let is_variable_font = face.fvar().is_ok();
    let is_monospaced = face.post().map_or(false, |v| v.is_fixed_pitch() != 0);
    let is_colored = face.colr().is_ok() && face.cpal().is_ok();
    let is_math = face.table_data(skrifa::Tag::new(b"MATH")).is_some();
    let is_symbol = is_symbol
        || face.cmap().is_ok_and(|v| {
            for r in v.encoding_records() {
                if r.platform_id() == PlatformId::Windows && r.encoding_id() == 0 {
                    return true;
                }
            }
            false
        });
    let tests = [
        (is_variable_font, "Variable"),
        (is_colored, "Colored"),
        (is_math, "Math"),
        (is_serif, "Serif"),
        (is_sans_serif, "Sans Serif"),
        (is_monospaced, "Monospace"),
        (is_symbol, "Symbol"),
        (is_decorative, "Decorative"),
        (is_hand_written, "Hand Written"),
    ];
    let mut font_labels = vec![];
    for (b, s) in tests {
        if b {
            font_labels.push(s);
        }
    }
    if !font_labels.is_empty() {
        print_data("Font Labels", &font_labels.join(", "))?;
    } // End of Font Labels

    // Permission and Class.
    if let Ok(os2) = face.os2() {
        print_data(
            "Usage Permission",
            &OS2FsType::new(os2.version(), os2.fs_type()).permission(),
        )?;
        let (cls, sub) =
            OS2SFamilyClass::new(os2.version(), os2.s_family_class()).class_and_subclass();
        if !matches!(cls, "No Classification" | "RESERVED") {
            print_data(
                "Font Family Class and Subclass",
                &format!("{}, {}", cls, sub),
            )?;
        }
    } else {
        log::info!("Cannot resolve OS/2 table");
    }

    // Tables & Labels.
    let mut aval_tables = vec![];
    #[rustfmt::skip]
    const TABLE_TAGS: &[&[u8; 4]] = &[
        b"avar", b"BASE", b"CBDT", b"CBLC", b"CFF ", b"CFF2", b"cmap", b"COLR", b"CPAL",
        b"cvar", b"cvt ", b"DSIG", b"EBDT", b"EBLC", b"EBSC", b"fpgm", b"fvar", b"gasp",
        b"GDEF", b"glyf", b"GPOS", b"GSUB", b"gvar", b"hdmx", b"head", b"hhea", b"hmtx",
        b"HVAR", b"JSTF", b"kern", b"loca", b"LTSH", b"MATH", b"maxp", b"MERG", b"meta",
        b"meta", b"MVAR", b"name", b"OS/2", b"PCLT", b"post", b"prep", b"sbix", b"STAT",
        b"SVG ", b"VDMX", b"vhea", b"vmtx", b"VORG", b"VVAR",
    ];
    for &tag in TABLE_TAGS {
        if face.table_data(skrifa::Tag::new(tag)).is_some() {
            aval_tables.push(unsafe { std::str::from_utf8_unchecked(tag) });
        }
    }
    if !aval_tables.is_empty() {
        print_data("Tables", &aval_tables.join(", "))?;
    }

    // Variable.
    let mut var_name_list = vec![];
    if let Ok(fvar_table) = face.fvar() {
        if let Ok(axes) = fvar_table.axes() {
            for axis in axes {
                let tag = axis.axis_tag();
                match std::str::from_utf8(&tag.into_bytes()) {
                    Ok(tag_str) => var_name_list.push(tag_str.to_compact_string()),
                    Err(_) => log::warn!("Cannot parse to str from {:?}", tag),
                }
            }
        }
    }
    if !var_name_list.is_empty() {
        print_data("Variable Axes", &var_name_list.join(", "))?;
    } // End of Variable

    // Features.
    let mut features_list = BTreeSet::new();
    if let Ok(gsub) = face.gsub() {
        if let Ok(gsub_feat) = gsub.feature_list() {
            for feat in gsub_feat.feature_records() {
                features_list.insert(feat.feature_tag().into_bytes());
            }
        } else {
            log::info!("Cannot resolve gsub feature table");
        }
    } else {
        log::info!("Cannot resolve gsub table");
    }
    let mut features_list = BTreeSet::new();
    if let Ok(gpos) = face.gpos() {
        if let Ok(gpos_feat) = gpos.feature_list() {
            for feat in gpos_feat.feature_records() {
                features_list.insert(feat.feature_tag().into_bytes());
            }
        } else {
            log::info!("Cannot resolve gpos feature table");
        }
    } else {
        log::info!("Cannot resolve gpos table");
    }
    if !features_list.is_empty() {
        let mut fe_list = vec![];
        for tag in features_list.iter() {
            match std::str::from_utf8(tag) {
                Ok(feat_str) => fe_list.push(feat_str),
                Err(_) => log::warn!("Cannot parse to str from {:?}", tag),
            };
        }
        print_data("Features", &fe_list.join(", "))?;
    } // End of features.

    // Blocks.
    let mut chars_list = HashSet::new();
    let mut blocks: IndexMap<&str, (u32, u32, u32)> = IndexMap::new();
    for (_, &b) in UNICODE_BLOCKS.iter() {
        blocks.insert(b.name(), (0, b.start(), b.end()));
    }
    blocks.insert("No Block", (0, 0, 0));
    let mut check_cp = |cp| {
        if chars_list.insert(cp) {
            match get_char_block_name(cp) {
                Some(v) => match blocks.get_mut(v) {
                    Some(v) => v.0 += 1,
                    None => {}
                },
                None => {}
            }
        }
    };
    if let Ok(cmap) = face.cmap() {
        for records in cmap.encoding_records() {
            if let Ok(subtable) = records.subtable(cmap.offset_data()) {
                match subtable {
                    CmapSubtable::Format4(map4) => {
                        for (start, end) in map4
                            .start_code()
                            .into_iter()
                            .zip(map4.end_code().into_iter())
                        {
                            if start == end && start.get() == 0xFFFF {
                                break;
                            }
                            for cp in start.get()..=end.get() {
                                check_cp(u32::from(cp))
                            }
                        }
                    }
                    CmapSubtable::Format12(map12) => {
                        for group in map12.groups() {
                            for cp in group.start_char_code()..=group.end_char_code() {
                                check_cp(cp)
                            }
                        }
                    }
                    _ => {}
                }
            } else {
                log::info!("Cannot resolve cmap subtable");
            }
        }
    } else {
        log::info!("Cannot resolve cmap table");
    }
    blocks.sort_unstable_by(|_, v1, _, v2| v1.2.cmp(&v2.2));
    let total_chars = blocks.iter().fold(0, |acc, (_, (n, ..))| acc + n);
    print_data("Supported Characters", &total_chars.to_string())?;
    for (&k, &(used, start, end)) in blocks.iter() {
        if used > 0 {
            let span = if start == 0 && end == 0 {
                "NA".to_string()
            } else {
                format!("{:04X}..{:04X}", start, end)
            };
            let range = if end == start { 0 } else { end - start + 1 };
            print_data(
                &format!("  - Block {} [{}]", &span, &k),
                &format!(
                    "{} / {}{}",
                    used,
                    range,
                    &format!(
                        "{}{infostyle}{}{infostyle:#}",
                        if used == range { " " } else { "" },
                        if used == range { "(Full)" } else { "" }
                    ),
                ),
            )?;
        }
    } // End of Blocks

    // Border.
    if !borderless {
        print_data(
            "",
            &format!(
                "{}{}{}",
                TABLE_BORDER[3],
                TABLE_BORDER[4].repeat(textwidth - 2),
                TABLE_BORDER[5]
            ),
        )?;
    } // End of Border

    if path.is_some() {
        write!(f, "End of {infostyle}{}{infostyle:#}", path.unwrap())?;
    } else {
        write!(f, "End of font data")?;
    }

    // End display

    Ok(())
}

struct OS2FsType(u16, u16); // version, fsType
impl OS2FsType {
    fn new(version: u16, fs_type: u16) -> OS2FsType {
        OS2FsType(version, fs_type)
    }
    // https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fstype
    fn permission(&self) -> String {
        let val = self.1 & 0x000F;
        let b1 = ((val >> 1) & 0b0001) == 0b0001;
        let b2 = ((val >> 2) & 0b0001) == 0b0001;
        let b3 = ((val >> 3) & 0b0001) == 0b0001;
        let b8 = (val & 0x0100) != 0;
        let b9 = (val & 0x0200) != 0;
        match self.0 {
            0 | 1 | 2 => {
                let per = if b3 & b2 {
                    "Preview & Print & Editable embedding"
                } else if b3 {
                    "Editable embedding"
                } else if b2 {
                    "Preview & Print embedding"
                } else if b1 {
                    "Restricted License embedding"
                } else {
                    "Installable embedding"
                };
                per.to_string()
            }
            3 | _ => {
                let mut per = if b3 {
                    "Editable embedding"
                } else if b2 {
                    "Preview & Print embedding"
                } else if b1 {
                    "Restricted License embedding"
                } else {
                    "Installable embedding"
                }
                .to_string();
                if b8 {
                    per.push_str(" & No subsetting");
                }
                if b9 {
                    per.push_str(" & Bitmap embedding only");
                }
                per
            }
        }
    }
}

struct OS2SFamilyClass(i16); // version, sFamilyClass
impl OS2SFamilyClass {
    fn new(_version: u16, f_family_class: i16) -> OS2SFamilyClass {
        OS2SFamilyClass(f_family_class)
    }
    // https://learn.microsoft.com/en-us/typography/opentype/spec/ibmfc
    fn class_and_subclass(&self) -> (&'static str, &'static str) {
        let [sub, cls] = self.0.to_le_bytes();
        match (cls, sub) {
            (0, 0..=15) => ("No Classification", "NA"),
            (1, 0) => ("Oldstyle Serifs", "No Classification"),
            (1, 1) => ("Oldstyle Serifs", "IBM Rounded Legibility"),
            (1, 2) => ("Oldstyle Serifs", "Garalde"),
            (1, 3) => ("Oldstyle Serifs", "Venetian"),
            (1, 4) => ("Oldstyle Serifs", "Modified Venetian"),
            (1, 5) => ("Oldstyle Serifs", "Dutch Modern"),
            (1, 6) => ("Oldstyle Serifs", "Dutch Traditional"),
            (1, 7) => ("Oldstyle Serifs", "Contemporary"),
            (1, 8) => ("Oldstyle Serifs", "Calligraphic"),
            (1, 9..=14) => ("Oldstyle Serifs", "RESERVED"),
            (1, 15) => ("Oldstyle Serifs", "Miscellaneous"),
            (2, 0) => ("Transitional Serifs", "No Classification"),
            (2, 1) => ("Transitional Serifs", "Direct Line"),
            (2, 2) => ("Transitional Serifs", "Script"),
            (2, 3..=14) => ("Transitional Serifs", "RESERVED"),
            (2, 15) => ("Transitional Serifs", "Miscellaneous"),
            (3, 0) => ("Modern Serifs", "No Classification"),
            (3, 1) => ("Modern Serifs", "Italian"),
            (3, 2) => ("Modern Serifs", "Script"),
            (3, 3..=14) => ("Modern Serifs", "RESERVED"),
            (3, 15) => ("Modern Serifs", "Miscellaneous"),
            (4, 0) => ("Clarendon Serifs", "No Classification"),
            (4, 1) => ("Clarendon Serifs", "Clarendon"),
            (4, 2) => ("Clarendon Serifs", "Modern"),
            (4, 3) => ("Clarendon Serifs", "Traditional"),
            (4, 4) => ("Clarendon Serifs", "Newspaper"),
            (4, 5) => ("Clarendon Serifs", "Stub Serif"),
            (4, 6) => ("Clarendon Serifs", "Monotone"),
            (4, 7) => ("Clarendon Serifs", "Typewriter"),
            (4, 8..=14) => ("Clarendon Serifs", "RESERVED"),
            (4, 15) => ("Clarendon Serifs", "Miscellaneous"),
            (5, 0) => ("Slab Serifs", "No Classification"),
            (5, 1) => ("Slab Serifs", "Monotone"),
            (5, 2) => ("Slab Serifs", "Humanist"),
            (5, 3) => ("Slab Serifs", "Geometric"),
            (5, 4) => ("Slab Serifs", "Swiss"),
            (5, 5) => ("Slab Serifs", "Typewriter"),
            (5, 6..=14) => ("Slab Serifs", "RESERVED"),
            (5, 15) => ("Slab Serifs", "Miscellaneous"),
            (6, 0..=15) => ("RESERVED", "NA"),
            (7, 0) => ("Freeform Serifs", "No Classification"),
            (7, 1) => ("Freeform Serifs", "Modern"),
            (7, 2..=14) => ("Freeform Serifs", "RESERVED"),
            (7, 15) => ("Freeform Serifs", "Miscellaneous"),
            (8, 0) => ("Sans Serif", "No Classification"),
            (8, 1) => ("Sans Serif", "IBM Neo-grotesque Gothic"),
            (8, 2) => ("Sans Serif", "Humanist"),
            (8, 3) => ("Sans Serif", "Low-x Round Geometric"),
            (8, 4) => ("Sans Serif", "High-x Round Geometric"),
            (8, 5) => ("Sans Serif", "Neo-grotesque Gothic"),
            (8, 6) => ("Sans Serif", "Modified Neo-grotesque Gothic"),
            (8, 7..=8) => ("Sans Serif", "RESERVED"),
            (8, 9) => ("Sans Serif", "Typewriter Gothic"),
            (8, 10) => ("Sans Serif", "Matrix"),
            (8, 11..=14) => ("Sans Serif", "RESERVED"),
            (8, 15) => ("Sans Serif", "Miscellaneous"),
            (9, 0) => ("Ornamentals", "No Classification"),
            (9, 1) => ("Ornamentals", "Engraver"),
            (9, 2) => ("Ornamentals", "Black Letter"),
            (9, 3) => ("Ornamentals", "Decorative"),
            (9, 4) => ("Ornamentals", "Three Dimensional"),
            (9, 5..=14) => ("Ornamentals", "Decorative"),
            (9, 15) => ("Ornamentals", "Miscellaneous"),
            (10, 0) => ("Scripts", "No Classification"),
            (10, 1) => ("Scripts", "Uncial"),
            (10, 2) => ("Scripts", "Brush Joined"),
            (10, 3) => ("Scripts", "Formal Joined"),
            (10, 4) => ("Scripts", "Monotone Joined"),
            (10, 5) => ("Scripts", "Calligraphic"),
            (10, 6) => ("Scripts", "Brush Unjoined"),
            (10, 7) => ("Scripts", "Formal Joined"),
            (10, 8) => ("Scripts", "Monotone Joined"),
            (10, 9..=14) => ("Scripts", "RESERVED"),
            (10, 15) => ("Scripts", "Miscellaneous"),
            (11, 0..=15) => ("RESERVED", "NA"),
            (12, 0) => ("Symbolic", "No Classification"),
            (12, 1..=2) => ("Symbolic", "RESERVED"),
            (12, 3) => ("Symbolic", "Mixed Serif"),
            (12, 4..=5) => ("Symbolic", "RESERVED"),
            (12, 6) => ("Symbolic", "Oldstyle Serif"),
            (12, 7) => ("Symbolic", "Neo-grotesque Sans Serif"),
            (12, 8..=14) => ("Symbolic", "RESERVED"),
            (12, 15) => ("Symbolic", "Miscellaneous"),
            (13, 0..=15) => ("RESERVED", "NA"),
            (14, 0..=15) => ("RESERVED", "NA"),
            (15, 0..=15) => ("Miscellaneous", "NA"),
            _ => unimplemented!("Invalid sFamilyClass value {}", self.0),
        }
    }
}

struct OS2Panose([u8; 10]);
// see: https://monotype.github.io/panose/pan1.htm and https://monotype.github.io/panose/pan2.htm
impl OS2Panose {
    fn new(sl: &[u8]) -> OS2Panose {
        assert_eq!(sl.len(), 10);
        let mut p = [0; 10];
        p.copy_from_slice(&sl[0..10]);
        OS2Panose(p)
    }
    fn is_serif(&self) -> bool {
        let style = self.0[1];
        self.0[0] == 2 && style != 0 && style != 1 && style < 11
    }
    fn is_sans_serif(&self) -> bool {
        self.0[0] == 2 && self.0[1] >= 11
    }
    fn is_hand_written(&self) -> bool {
        self.0[0] == 3
    }
    fn is_decorative(&self) -> bool {
        self.0[0] == 4
    }
    fn is_symbol(&self) -> bool {
        self.0[0] == 5
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FontPath(String);
impl FontPath {
    pub fn as_str(&self) -> &str {
        &self.0
    }
    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }
    pub fn as_path_buf(&self) -> PathBuf {
        PathBuf::from(&self.0)
    }
}
impl Display for FontPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_path().display())
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum FontLinks {
    Map(IndexMap<String, Vec<u32>>),
    Vec(Vec<FontLink>),
}
impl FontLinks {
    pub fn len(&self) -> usize {
        match self {
            Self::Map(map) => map.len(),
            Self::Vec(vec) => vec.len(),
        }
    }
    pub fn into_vec(self) -> Self {
        match self {
            Self::Map(map) => {
                let mut vec = Vec::with_capacity(map.len());
                map.into_iter().for_each(|(key, value)| {
                    vec.push(FontLink {
                        name: key,
                        inst: value,
                    });
                });
                Self::Vec(vec)
            }
            Self::Vec(vec) => Self::Vec(vec),
        }
    }
    pub fn into_map(self) -> Self {
        match self {
            Self::Map(map) => Self::Map(map),
            Self::Vec(vec) => {
                let mut map = IndexMap::with_capacity(vec.len());
                vec.into_iter().for_each(|FontLink { name, inst }| {
                    map.insert(name, inst);
                });
                Self::Map(map)
            }
        }
    }
    pub fn to_vec_ref(&self) -> Vec<FontLinkRef> {
        let mut res = vec![];
        match self {
            Self::Map(map) => {
                map.iter().for_each(|(name, inst)| {
                    let link = FontLinkRef {
                        name: name.as_str(),
                        inst,
                    };
                    res.push(link);
                });
            }
            Self::Vec(vec) => {
                vec.iter().for_each(|FontLink { name, inst }| {
                    let link = FontLinkRef {
                        name: name.as_str(),
                        inst,
                    };
                    res.push(link);
                });
            }
        }
        res
    }
    pub fn get_map(&self) -> Option<&IndexMap<String, Vec<u32>>> {
        match self {
            Self::Map(map) => Some(map),
            Self::Vec(_) => None,
        }
    }
    pub fn get_vec(&self) -> Option<&Vec<FontLink>> {
        match self {
            Self::Map(_) => None,
            Self::Vec(vec) => Some(vec),
        }
    }

    /// Get iterator of FontLinks, only if the inner data is Map.
    pub fn iter_if_map(&self) -> Option<MapLinkIter> {
        match self {
            FontLinks::Map(map) => Some(MapLinkIter {
                link: map,
                index: 0,
            }),
            FontLinks::Vec(_) => None,
        }
    }
    pub fn iter(&self) -> LinkIter {
        LinkIter {
            link: self,
            index: 0,
        }
    }

    pub fn get_inst_by_index(&self, index: usize) -> Option<&Vec<u32>> {
        match self {
            Self::Map(map) => map.get_index(index).map(|v| v.1),
            Self::Vec(vec) => vec.get(index).map(|v| &v.inst),
        }
    }
    pub fn get_inst_by_key(&self, key: &str) -> Option<&Vec<u32>> {
        match self {
            Self::Map(map) => map.get(key),
            Self::Vec(vec) => vec.iter().find(|&v| v.name == key).map(|v| &v.inst),
        }
    }
    pub fn get_index_by_key(&self, key: &str) -> Option<usize> {
        match self {
            Self::Map(map) => map.get_index_of(key),
            Self::Vec(vec) => vec.iter().position(|v| v.name == key),
        }
    }

    pub fn fuzzy_search(&self, key: &str, score: f64) -> Vec<(usize, f64)> {
        let mut res: Vec<(usize, f64)> = match self {
            Self::Map(map) => map
                .par_iter()
                .enumerate()
                .filter_map(|(idx, (name, _))| {
                    let sim = similarity(key, name);
                    if sim < score {
                        None
                    } else {
                        Some((idx, sim))
                    }
                })
                .collect(),
            Self::Vec(vec) => vec
                .par_iter()
                .enumerate()
                .filter_map(|(idx, link)| {
                    let sim = similarity(key, &link.name);
                    if sim < score {
                        None
                    } else {
                        Some((idx, sim))
                    }
                })
                .collect(),
        };
        res.sort_by(|a, b| {
            if a.1 > b.1 {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
        res
    }
}
impl Default for FontLinks {
    fn default() -> Self {
        FontLinks::Map(IndexMap::new())
    }
}

#[derive(Clone)]
pub struct FileIter<'a> {
    database: &'a FontDatabase,
    link_index: usize,
    inst_index: usize,
}
impl<'a> Iterator for FileIter<'a> {
    type Item = &'a FontFile;
    fn next(&mut self) -> Option<Self::Item> {
        let inst = self.database.link.get_inst_by_index(self.link_index)?;
        let inst_index = self.inst_index;
        if inst_index < inst.len() {
            let index = *unsafe { inst.get_unchecked(inst_index) } as usize;
            self.inst_index += 1;
            self.database.file.get(index)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct FileFuzzyIter<'a> {
    database: &'a FontDatabase,
    link_iter: std::vec::IntoIter<(usize, f64)>,
    inst_index: Option<&'a Vec<u32>>,
    inner_index: usize,
    current_score: f64,
}

impl<'a> Iterator for FileFuzzyIter<'a> {
    type Item = (&'a FontFile, f64);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(inst) = self.inst_index {
                if self.inner_index < inst.len() {
                    let index = inst[self.inner_index] as usize;
                    let font_file = self.database.file.get(index)?;
                    self.inner_index += 1;
                    return Some((font_file, self.current_score));
                } else {
                    self.inst_index = None;
                    self.inner_index = 0;
                }
            }

            match self.link_iter.next() {
                Some((index, score)) => match self.database.link.get_inst_by_index(index) {
                    Some(u32_vec) => {
                        self.inst_index = Some(u32_vec);
                        self.current_score = score;
                        continue;
                    }
                    None => {
                        continue;
                    }
                },
                None => {
                    return None;
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct LinkIter<'a> {
    link: &'a FontLinks,
    index: usize,
}
impl<'a> Iterator for LinkIter<'a> {
    type Item = FontLinkRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.link.len() {
            let index = self.index;
            self.index += 1;
            let link = match self.link {
                FontLinks::Map(map) => map.get_index(index),
                FontLinks::Vec(vec) => {
                    let val = vec.get(index)?;
                    Some((&val.name, &val.inst))
                }
            }?;
            Some(FontLinkRef {
                name: link.0.as_str(),
                inst: link.1,
            })
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct MapLinkIter<'a> {
    link: &'a IndexMap<String, Vec<u32>>,
    index: usize,
}
impl<'a> Iterator for MapLinkIter<'a> {
    type Item = FontLinkRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.link.len() {
            let index = self.index;
            self.index += 1;
            let link = self.link.get_index(index)?;
            Some(FontLinkRef {
                name: link.0.as_str(),
                inst: link.1,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FontLink {
    pub name: String,
    pub inst: Vec<u32>,
}

#[derive(Debug, Serialize)]
pub struct FontLinkRef<'a> {
    pub name: &'a str,
    pub inst: &'a Vec<u32>,
}

pub type FontSets = FontLinks;
pub type FontSet = FontLink;
pub type FontSetRef<'a> = FontLinkRef<'a>;

#[derive(Debug, Serialize, Deserialize)]
pub struct FontLinkHash {
    pub key: usize,
    pub vals: Vec<usize>,
}

pub type FontSetHash = FontLinkHash;

struct EscapedFormatter<'a>(PrettyFormatter<'a>);
impl<'a> EscapedFormatter<'a> {
    fn new() -> Self {
        let pfmt = PrettyFormatter::new();
        EscapedFormatter(pfmt)
    }
}
impl Formatter for EscapedFormatter<'_> {
    fn write_string_fragment<W>(&mut self, writer: &mut W, fragment: &str) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        let mut buf = [0, 0];
        for c in fragment.chars() {
            if c.is_ascii() {
                writer.write(&[c as u8])?;
            } else {
                let buf = c.encode_utf16(&mut buf);
                for i in buf {
                    let _ = write!(writer, r"\u{:04x}", i)?;
                }
            }
        }
        Ok(())
    }

    fn write_null<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_null(writer)
    }

    fn write_bool<W>(&mut self, writer: &mut W, value: bool) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_bool(writer, value)
    }

    fn write_i8<W>(&mut self, writer: &mut W, value: i8) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_i8(writer, value)
    }

    fn write_i16<W>(&mut self, writer: &mut W, value: i16) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_i16(writer, value)
    }

    fn write_i32<W>(&mut self, writer: &mut W, value: i32) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_i32(writer, value)
    }

    fn write_i64<W>(&mut self, writer: &mut W, value: i64) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_i64(writer, value)
    }

    fn write_i128<W>(&mut self, writer: &mut W, value: i128) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_i128(writer, value)
    }

    fn write_u8<W>(&mut self, writer: &mut W, value: u8) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_u8(writer, value)
    }

    fn write_u16<W>(&mut self, writer: &mut W, value: u16) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_u16(writer, value)
    }

    fn write_u32<W>(&mut self, writer: &mut W, value: u32) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_u32(writer, value)
    }

    fn write_u64<W>(&mut self, writer: &mut W, value: u64) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_u64(writer, value)
    }

    fn write_u128<W>(&mut self, writer: &mut W, value: u128) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_u128(writer, value)
    }

    fn write_f32<W>(&mut self, writer: &mut W, value: f32) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_f32(writer, value)
    }

    fn write_f64<W>(&mut self, writer: &mut W, value: f64) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_f64(writer, value)
    }

    fn write_number_str<W>(&mut self, writer: &mut W, value: &str) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_number_str(writer, value)
    }

    fn begin_string<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_string(writer)
    }

    fn end_string<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_string(writer)
    }

    fn write_char_escape<W>(
        &mut self,
        writer: &mut W,
        char_escape: serde_json::ser::CharEscape,
    ) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_char_escape(writer, char_escape)
    }

    fn write_byte_array<W>(&mut self, writer: &mut W, value: &[u8]) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_byte_array(writer, value)
    }

    fn begin_array<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_array(writer)
    }

    fn end_array<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_array(writer)
    }

    fn begin_array_value<W>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_array_value(writer, first)
    }

    fn end_array_value<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_array_value(writer)
    }

    fn begin_object<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_object(writer)
    }

    fn end_object<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_object(writer)
    }

    fn begin_object_key<W>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_object_key(writer, first)
    }

    fn end_object_key<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_object_key(writer)
    }

    fn begin_object_value<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.begin_object_value(writer)
    }

    fn end_object_value<W>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.end_object_value(writer)
    }

    fn write_raw_fragment<W>(&mut self, writer: &mut W, fragment: &str) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.0.write_raw_fragment(writer, fragment)
    }
}
