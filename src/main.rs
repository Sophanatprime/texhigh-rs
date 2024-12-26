use std::collections::HashSet;
use std::env::current_exe;
use std::fs::{self, File};
use std::io::{self, BufReader, BufWriter};
use std::iter::{empty, zip};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time;

use clap::ArgMatches;
use config;
use log::{info, warn};
use rayon::prelude::*;
use texhigh::{
    config::{HighConfig, THConfig},
    fonts::{
        display_font_file_all_face, similarity_bytes, FontDatabase, FontFile, DEFAULT_DATA_PATH,
    },
    get_kpse_matches, get_matches,
    high::{HWrite, StandardFormatter},
    layout::{self, OutputFormat},
    types::{CTabSet, CatcodeStack, TokenList},
    KpseWhich,
};
use textwrap::termwidth;

#[cfg(not(debug_assertions))]
use mimalloc::MiMalloc;
#[cfg(not(debug_assertions))]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let m = get_matches();
    match m.subcommand() {
        Some(("high", high)) => command_high(high),
        Some(("font", font)) => command_font(font),
        Some(("layout", layout)) => command_layout(layout),
        Some(("text", text)) => command_text(text),
        _ => {}
    }
}

fn command_high(m: &ArgMatches) {
    let th_config = get_thconfig(&m);

    let ctabset = &th_config.ctabs;
    let ctab_name = &get_command_str(m, "current-ctab");
    let ctab = ctabset
        .get_by_name(ctab_name)
        .expect("Unknown currant catcode table name.");
    let mut ctabs = CatcodeStack::new();
    match th_config.high_config.ctabs_fallback.get(ctab_name) {
        Some(ctab_name_vec) => {
            for ctab_name in ctab_name_vec.iter().next_back().into_iter() {
                match ctabset.get_by_name(ctab_name) {
                    Some(ctab_fallback) => {
                        ctabs.push(ctab_fallback);
                    }
                    None => {
                        warn!(target: "Getting Current CTab fallback", "Unknown ctab name: {}", ctab_name)
                    }
                }
            }
        }
        None => {}
    }
    ctabs.push(ctab);

    let mut tokenlist_vec: Vec<(String, bool)> = Vec::new();
    if m.contains_id("text") {
        let text = get_command_str(m, "text").to_string();
        tokenlist_vec.push((text, false));
    } else if m.contains_id("file") {
        let fna: Vec<Vec<&String>> = m
            .get_occurrences("file")
            .unwrap()
            .map(Iterator::collect)
            .collect();
        for fv in fna.iter() {
            for f in fv {
                tokenlist_vec.push((f.to_string(), true));
            }
        }
    };
    let mut fm_vec = Vec::new();
    for (f_or_s, is_file) in tokenlist_vec.iter() {
        let (tokenlist_str, file_size) = if *is_file {
            let f_path = File::open(f_or_s).expect("Unable open tokenlist file");
            let f_len = f_path.metadata().unwrap().len();
            let mut f = BufReader::new(f_path);
            let s = io::read_to_string(&mut f).expect("Unable read tokenlist file");
            if th_config.high_config.tab_to_spaces && s.contains('\t') {
                (
                    s.replace('\t', &" ".repeat(th_config.high_config.tabs_len as usize)),
                    f_len,
                )
            } else {
                (s, f_len)
            }
        } else {
            if th_config.high_config.tab_to_spaces && f_or_s.contains('\t') {
                (
                    f_or_s.replace('\t', &" ".repeat(th_config.high_config.tabs_len as usize)),
                    u64::MIN,
                )
            } else {
                (f_or_s.to_owned(), u64::MIN)
            }
        };
        let tokenlist = TokenList::parse(tokenlist_str, &ctabs);
        let fm = StandardFormatter::new(&th_config.high_config, tokenlist);
        fm_vec.push((fm, if *is_file { f_or_s } else { "" }, file_size));
    }

    if m.contains_id("output") {
        let out = &PathBuf::from(get_command_str(m, "output"));
        if fm_vec.len() == 1 {
            let fm = &fm_vec[0].0;
            let buffer_size = get_buffer_size(fm_vec[0].2);
            info!(target: "Output", "Writing to {}, buffer size {}", out.as_os_str().to_string_lossy(), buffer_size);
            let mut f = BufWriter::with_capacity(
                buffer_size,
                fs::File::create(out).expect("Unable open output file"),
            );
            fm.format_now(&mut f).unwrap();
        } else {
            if !out.exists() || out.is_file() {
                fs::create_dir_all(out).unwrap();
            }
            for (fm, file, file_size) in fm_vec.iter() {
                let file_path = out.join(*file);
                if file_path.parent().is_some() {
                    let file_parent = file_path.parent().unwrap();
                    if !out.as_os_str().is_empty()
                        && (!file_parent.exists() || file_parent.is_file())
                    {
                        fs::create_dir_all(file_parent).unwrap();
                    }
                }
                let buffer_size = get_buffer_size(*file_size);
                info!(target: "Output", "Writing to {}, buffer size {}", file, buffer_size);
                let mut f = BufWriter::with_capacity(
                    buffer_size,
                    fs::File::create(&file_path).expect("Unable open output file"),
                );
                fm.format_now(&mut f).unwrap();
            }
        }
    } else {
        let mut f = BufWriter::with_capacity(16_000, io::stdout().lock());
        for (sfmt, _, _) in fm_vec.iter() {
            sfmt.format_now(&mut f).unwrap();
        }
    }
}

fn get_thconfig(m: &ArgMatches) -> THConfig {
    let mut th_config = THConfig::new();

    th_config
        .ctabs
        .extend(CTabSet::from_str(include_str!("prelude-ctabset.thcs")).unwrap());
    th_config.high_config = get_highconfig(m);

    let mut ctab_list: Vec<(usize, CTabSet)> = vec![];

    if m.contains_id("ctab-set") {
        let ctab_set_str = m.get_many::<String>("ctab-set").unwrap();
        let ctab_set_indices = m.indices_of("ctab-set").unwrap().collect::<Vec<_>>();
        let mut ctab_set: Vec<CTabSet> = Vec::with_capacity(ctab_set_indices.len());
        for ct in ctab_set_str {
            ctab_set.push(CTabSet::from_str(ct).expect("Cannot parse ctabset"));
        }
        ctab_list.extend(zip(ctab_set_indices, ctab_set));
    }
    if m.contains_id("ctab") {
        let ctab_set_str: Vec<Vec<&String>> = m
            .get_occurrences("ctab")
            .unwrap()
            .map(Iterator::collect)
            .collect();
        let ctab_set_indices = m.indices_of("ctab").unwrap().step_by(2).collect::<Vec<_>>();
        let mut ctab_set: Vec<CTabSet> = Vec::with_capacity(ctab_set_indices.len());
        unsafe {
            for ct in ctab_set_str {
                let s = format!("[{}] {}", ct.get_unchecked(0), ct.get_unchecked(1));
                ctab_set.push(CTabSet::from_str(&s).expect("Cannot parse ctab"));
            }
        }
        ctab_list.extend(zip(ctab_set_indices, ctab_set));
    }
    if m.contains_id("ctab-file") {
        let ctab_set_fn = m.get_many::<String>("ctab-file").unwrap();
        let ctab_set_indices = m.indices_of("ctab-file").unwrap();
        let mut ctab_set: Vec<CTabSet> = Vec::with_capacity(ctab_set_indices.len());
        for ct in ctab_set_fn {
            info!(target: "Finding ctab-file", "{}", ct);
            let mut f = BufReader::new(File::open(ct).expect("Unknown ctab-file"));
            let s = io::read_to_string(&mut f).expect("Unable read ctab-file");
            ctab_set.push(CTabSet::from_str(&s).expect("Cannot parse ctab-file"));
        }
        ctab_list.extend(zip(ctab_set_indices, ctab_set));
    }

    ctab_list.sort_by_key(|(i, _)| *i);
    for (_, ct) in ctab_list {
        th_config.ctabs.extend(ct);
    }

    th_config
}

fn get_highconfig(m: &ArgMatches) -> HighConfig {
    use config::{Config, File, FileFormat};
    let use_kpse = m.get_flag("kpse-config-file");
    let kpse = if m.contains_id("kpse-args") {
        let kpse_m = get_kpse_matches(
            m.get_many::<String>("kpse-args")
                .unwrap()
                .collect::<Vec<_>>(),
        );
        KpseWhich::from_matches(&kpse_m)
    } else {
        KpseWhich::new()
    };
    let mut config = Config::builder();
    config = config.add_source(File::from_str(
        include_str!("prelude-config.toml"),
        FileFormat::Toml,
    ));
    let mut config_list: Vec<(usize, String, bool)> = Vec::new(); // bool: is file or not.
    if m.contains_id("config-file") {
        let con_file = m
            .get_many::<String>("config-file")
            .unwrap()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        let config_indices = m.indices_of("config-file").unwrap().collect::<Vec<_>>();
        zip(config_indices, con_file).for_each(|(c, i)| config_list.push((c, i, true)));
    }
    if m.contains_id("config") {
        let mut con = Vec::new();
        for k in m
            .get_occurrences::<String>("config")
            .unwrap()
            .map(Iterator::collect::<Vec<_>>)
        {
            con.push(format!(
                "{} = '''{}'''",
                unsafe { k.get_unchecked(0) },
                normalize_quote(unsafe { k.get_unchecked(1) })
            ));
        }
        let config_indices = m
            .indices_of("config")
            .unwrap()
            .step_by(2)
            .collect::<Vec<_>>();
        zip(config_indices, con).for_each(|(c, i)| config_list.push((c, i, false)));
    }
    config_list.sort_by_key(|(i, _, _)| *i);
    for (_, config_str, is_file) in config_list.iter() {
        if *is_file {
            if use_kpse {
                match kpse.output(config_str) {
                    Ok(source) => {
                        for f in &source {
                            info!(target: "Finding config file", "{}", f);
                            config = config.add_source(File::new(f, FileFormat::Toml));
                        }
                    }
                    Err(_) => warn!(target: "Finding config file", "Unknown file: {}", config_str),
                }
            } else {
                info!(target: "Finding config file", "{}", config_str);
                config = config.add_source(File::new(config_str, FileFormat::Toml));
            }
        } else {
            config = config.add_source(File::from_str(config_str, FileFormat::Toml));
        }
    }
    config
        .build()
        .expect("Cannot parse configs")
        .try_deserialize()
        .unwrap()
}

fn get_buffer_size(file_size: u64) -> usize {
    if file_size < 80_000 {
        8_000
    } else if file_size < 800_000 {
        32_000
    } else {
        64_000
    }
}

fn command_font(m: &ArgMatches) {
    match m.subcommand() {
        Some(("build", cmd_m)) => {
            let paths: Vec<&String> = cmd_m.get_many("paths").unwrap_or_default().collect();
            let status = build_db(&paths, !cmd_m.get_flag("no-default-paths"));
            if !status {
                println!("Failed to build Font Database");
            }
        }
        Some(("find", cmd_m)) => {
            let dbfile = DEFAULT_DATA_PATH
                .as_ref()
                .expect("Cannot access the default data path")
                .join("texhigh-fontdb.json");
            if dbfile.exists() || build_db(empty::<&str>(), true) {
                match read_db(&dbfile) {
                    Ok(db) => find_font(&db, cmd_m),
                    Err(e) => eprintln!("{}", e),
                }
            } else {
                eprintln!("Cannot read or build Font Database");
            }
        }
        _ => {}
    }
}

fn build_db<T, P>(paths: P, default: bool) -> bool
where
    T: AsRef<Path>,
    P: IntoIterator<Item = T>,
    P::IntoIter: Clone,
{
    // TODO: This should remove a path with the special syntax of kpathsea.
    fn normalize_kpse_paths(paths: Vec<String>) -> Vec<String> {
        paths
    }

    let timer = time::Instant::now();
    let db = if default {
        #[cfg(target_os = "windows")]
        let sys_font_paths = vec!["C:\\windows\\fonts"];
        #[cfg(target_os = "macos")]
        let sys_font_paths = vec!["/Library/Fonts", "/System/Library/Fonts"];
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        let sys_font_paths = vec!["/usr/local/share/fonts", "/usr/share/fonts"];

        // TODO: Need to use kpathsea/cpathsea to get truetype and opentype directories,
        // and remove the use of normalize_kpse_paths
        let kpse_path = current_exe().and_then(|mut p| {
            p.set_file_name("kpsewhich");
            p.set_extension(std::env::consts::EXE_EXTENSION);
            Ok(p)
        });
        let is_in_tex_bin_dir = kpse_path.map_or(false, |p| p.exists());
        let tex_truetype_paths = if is_in_tex_bin_dir {
            let mut res = vec![];
            if let Some(texmf_dist_dir) = KpseWhich::var_value("TEXMFDIST", false) {
                res.push(format!("{}/fonts/truetype", &texmf_dist_dir));
                res.push(format!("{}/fonts/opentype", &texmf_dist_dir));
            }
            if let Some(texmf_local_dir) = KpseWhich::var_value("TEXMFLOCAL", false) {
                res.push(format!("{}/fonts/truetype", &texmf_local_dir));
                res.push(format!("{}/fonts/opentype", &texmf_local_dir));
            }
            normalize_kpse_paths(res)
        } else {
            vec![]
        };

        let input_paths = paths.into_iter();

        println!("Finding truetype fonts from: [");
        tex_truetype_paths
            .iter()
            .for_each(|p| println!("    {},", p));
        sys_font_paths.iter().for_each(|p| println!("    {},", p));
        input_paths
            .clone()
            .for_each(|p| println!("    {},", p.as_ref().display()));
        println!("]");

        let mut db = FontDatabase::new_from_paths(&tex_truetype_paths);
        db.add_fonts(&sys_font_paths);
        db.add_fonts(input_paths);
        db
    } else {
        let input_paths = paths.into_iter();
        println!("Finding truetype fonts from: [");
        input_paths
            .clone()
            .for_each(|p| println!("    {},", p.as_ref().display()));
        println!("]");
        FontDatabase::new_from_paths(input_paths)
    };

    let filename = DEFAULT_DATA_PATH
        .as_ref()
        .expect("Cannot access the default data path")
        .join("texhigh-fontdb.json");
    let status = match db.save_to_file(&filename, false) {
        Ok(_) => {
            println!(
                "Found {} font face(s), {} indexed names, writing to '{}'",
                db.file_count(),
                db.link_count(),
                filename.display()
            );
            true
        }
        Err(e) => {
            eprintln!("{}", e);
            false
        }
    };

    log::info!(
        "Takes {:#}ms to build font database",
        timer.elapsed().as_millis()
    );
    status
}

fn read_db<P: AsRef<Path>>(dbfile: P) -> anyhow::Result<FontDatabase> {
    let timer = time::Instant::now();
    let res = FontDatabase::from_json_file(dbfile);
    log::info!(
        "Takes {:#}ms to parse font database",
        timer.elapsed().as_millis()
    );
    res
}

fn find_font(db: &FontDatabase, cmd_m: &ArgMatches) {
    let mut dup: HashSet<*const FontFile> = HashSet::new();
    let is_family = |f: &FontFile, n: &String| {
        f.family.contains(n) || f.full.contains(n) || f.postscript.contains(n)
    };

    let fontname = &get_command_str_except(cmd_m, "name", "Require a font name");

    if cmd_m.get_flag("local") {
        println!("Read font from '{}'", fontname);
        let textwidth = if cmd_m.get_flag("unwrapped") {
            usize::MAX
        } else {
            termwidth()
        };
        let mut res = String::new();
        display_font_file_all_face(
            &mut res,
            fontname,
            textwidth,
            true,
            cmd_m.get_flag("borderless"),
        )
        .expect(&format!("Cannot display font '{}'", fontname));
        println!("Local Font File: {}\n{}", fontname, &res);
        return;
    }

    let only_family = cmd_m.get_flag("only-family");
    let fmt_method: Box<dyn Fn(&FontFile) -> String> = if cmd_m.get_flag("info") {
        Box::new(|ff| {
            FontFile::display_info_with(
                ff,
                !cmd_m.get_flag("unwrapped"),
                cmd_m.get_flag("borderless"),
            )
        })
    } else if cmd_m.get_flag("full") {
        Box::new(FontFile::display_full)
    } else if cmd_m.get_flag("short") {
        Box::new(FontFile::display_short)
    } else {
        Box::new(FontFile::display_simple)
    };

    if let Some(extension) = Path::new(fontname).extension() {
        if let Some(extension) = extension.to_str() {
            let extension = extension.to_ascii_lowercase();

            if matches!(extension.as_str(), "ttf" | "ttc" | "otf" | "otc") {
                let target_path = Path::new(fontname);
                let target_stem = target_path.file_stem();
                print!("Find font file: '{}'. ", target_path.display());

                let res = if let Some(target_dir) = target_path.parent() {
                    db.file()
                        .par_iter()
                        .filter(|&f| {
                            let font_path = f.path.as_path();
                            let font_extension = font_path
                                .extension()
                                .unwrap_or_default()
                                .to_str()
                                .unwrap_or_default();
                            if font_extension.to_ascii_lowercase().as_str() == extension
                                && font_path.file_stem() == target_stem
                            {
                                if let Some(font_dir) = font_path.parent() {
                                    font_dir.ends_with(target_dir)
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        })
                        .collect::<Vec<_>>()
                } else {
                    db.file()
                        .par_iter()
                        .filter(|&f| {
                            let font_path = f.path.as_path();
                            let font_extension = font_path
                                .extension()
                                .unwrap_or_default()
                                .to_str()
                                .unwrap_or_default();
                            font_extension.to_ascii_lowercase().as_str() == extension
                                && font_path.file_stem() == target_stem
                        })
                        .collect::<Vec<_>>()
                };
                // Display the database font.
                println!("Found {} entries.", res.len());
                for (idx, file) in res.iter().enumerate() {
                    println!("Entry {}: {}", idx, fmt_method(file))
                }
                return;
            }
        }
    }

    if cmd_m.get_flag("starts") {
        let fuzzy = *cmd_m.get_one::<f64>("fuzzy").unwrap_or(&1.0);
        let inst_it = db
            .link()
            .get_map()
            .expect("Illegal data type")
            .par_iter()
            .filter(|&(name, _)| {
                if fontname.len() == 0 || name.len() < fontname.len() {
                    false
                } else if fuzzy == 1.0 {
                    name.starts_with(fontname)
                } else {
                    let min = fontname.len();
                    let extra_len = name.as_bytes()[0..min]
                        .iter()
                        .filter(|&&v| v == b' ')
                        .count();
                    let the_name = &name.as_bytes()[0..name.len().min(min + extra_len)];
                    !(similarity_bytes(fontname.as_bytes(), the_name) < fuzzy)
                }
            })
            .collect::<Vec<_>>();
        let mut res = vec![];
        for &(_, inst) in inst_it.iter() {
            for &file_index in inst {
                let fnfile = &db.file()[file_index as usize];
                if dup.insert(fnfile) {
                    res.push(fnfile);
                }
            }
        }
        res.sort_unstable_by(|a, b| a.path.as_str().cmp(b.path.as_str()));

        print!("Find font name: '{}'. ", fontname);
        println!("Found {} entries.", res.len());
        for (idx, file) in res.iter().enumerate() {
            println!("Entry {}: {}", idx, fmt_method(file))
        }
    } else if cmd_m.contains_id("fuzzy") {
        let fuzzy_score = *cmd_m.get_one::<f64>("fuzzy").unwrap();
        print!("Find font name (fuzzy {:4}): '{}'. ", fuzzy_score, fontname);
        let res = db.get_fontinfo_fuzzy(fontname, fuzzy_score);
        let res = if only_family {
            res.filter(|&(f, _)| dup.insert(f) && is_family(f, fontname))
                .collect::<Vec<_>>()
        } else {
            res.filter(|&(f, _)| dup.insert(f)).collect::<Vec<_>>()
        };
        println!("Found {} entries.", res.len());
        for (idx, (file, score)) in res.iter().enumerate() {
            println!(
                "Entry {} (Score: {}): {}",
                idx,
                round_n(*score, 4),
                fmt_method(file)
            );
        }
    } else {
        print!("Find font name: '{}'. ", fontname);
        if db.contains(fontname) {
            let res = db.get_fontinfo(fontname);
            let res = if only_family {
                res.filter(|&f| dup.insert(f) && is_family(f, fontname))
                    .collect::<Vec<_>>()
            } else {
                res.filter(|&f| dup.insert(f)).collect::<Vec<_>>()
            };
            println!("Found {} entries.", res.len());
            for (idx, file) in res.iter().enumerate() {
                println!("Entry {}: {}", idx, fmt_method(file))
            }
        } else {
            println!("Found 0 entries.");
        }
    }
}

fn command_layout(m: &ArgMatches) {
    let font_size = *m.get_one::<f32>("fontsize").unwrap_or(&0.0);
    let line_height = *m.get_one::<f32>("lineheight").unwrap_or(&font_size);
    let width = *m.get_one::<f32>("width").unwrap_or(&0.0);
    let mut fonts = vec![];
    if let Some(font_ref) = m.get_many::<String>("fonts") {
        font_ref.for_each(|v| fonts.extend(normalize_quote(v).split(',')));
    }

    let dbfile = DEFAULT_DATA_PATH
        .as_ref()
        .expect("Cannot access the default data path")
        .join("texhigh-fontdb.json");
    let fontdb = if fonts.is_empty() {
        FontDatabase::new_empty()
    } else if dbfile.exists() || build_db(empty::<&str>(), true) {
        match read_db(&dbfile) {
            Ok(db) => db,
            Err(e) => {
                eprintln!("{}", e);
                FontDatabase::new_empty()
            }
        }
    } else {
        eprintln!("Cannot read or build Font Database");
        FontDatabase::new_empty()
    };

    let output = m.get_one::<String>("output").map_or("picture", |v| v);
    let output = normalize_quote(output);
    let mut form = if output == "picture" {
        OutputFormat::WriterPicture(Box::new(io::stdout().lock()))
    } else if output == "general" {
        OutputFormat::WriterGeneral(Box::new(io::stdout().lock()))
    } else {
        OutputFormat::Image(PathBuf::from(output))
    };

    let mut lay = layout::Layout::new(&fontdb, &fonts);

    lay.add_system_fonts();
    lay.font_size = font_size;
    lay.line_height = line_height;
    lay.text_width = width;
    let geometry = lay.layout(&get_command_str(m, "text"));

    if let Err(e) = lay.output(&geometry, &mut form) {
        eprintln!("Cannot layout text, cause {}", e);
    }
}

fn command_text(m: &ArgMatches) {
    use regex::{Captures, Regex};
    use unicode_linebreak::linebreaks;
    use unicode_names2::name as uni_name;
    use unicode_script::UnicodeScript;
    use unicode_segmentation::UnicodeSegmentation;
    use yeslogic_unicode_blocks::find_unicode_block;

    let text = &get_command_str(m, "text");
    let escaped = m.get_flag("escaped");
    let printer: Box<dyn Fn(&str) -> String> = if escaped {
        Box::new(|s: &str| {
            let mut res = String::new();
            for c in s.chars() {
                res.push_str(&format!("\\u{:04x}", c as u32));
            }
            res
        })
    } else {
        Box::new(|s: &str| format!("{:?}", s))
    };

    let from_uni_re = Regex::new(r"\\u(\{[[:xdigit:]]{1,8}\}|[[:xdigit:]]{1,8})").unwrap();
    let to_uni_re = Regex::new(r"(.)").unwrap();
    let from_uni_seq = |s: &str| {
        from_uni_re
            .replace_all(s, |c: &Captures<'_>| {
                let s = c[1].to_string();
                let start = s.starts_with('{') as usize;
                match u32::from_str_radix(&s[start..s.len() - start], 16) {
                    Ok(cp) => match char::from_u32(cp) {
                        Some(c) => format!("{}", c),
                        None => String::new(),
                    },
                    Err(_) => String::new(),
                }
            })
            .to_string()
    };
    let to_uni_seq = |s: &str| {
        to_uni_re
            .replace_all(s, |cap: &Captures<'_>| match cap[1].chars().next() {
                Some(c) => format!("\\u{:04x}", c as u32),
                None => String::new(),
            })
            .to_string()
    };

    let mut ioout = io::stdout().lock();
    writeln!(ioout, "Text: '{}'", text).expect("Cannot write to stream");
    let mut last_idx = 0;
    if m.get_flag("from-unicode") {
        writeln!(ioout, "{}", &from_uni_seq(text)).expect("Cannot write to stream");
    } else if m.get_flag("to-unicode") {
        writeln!(ioout, "{}", &to_uni_seq(text)).expect("Cannot write to stream");
    } else if m.get_flag("linebreak") {
        for (idx, op) in linebreaks(text) {
            if idx == last_idx {
                continue;
            }
            writeln!(ioout, "{}, {:?}", printer(&text[last_idx..idx]), op)
                .expect("Cannot write to stream");
            last_idx = idx;
        }
    } else if m.get_flag("cluster") {
        for s in text.graphemes(true) {
            writeln!(ioout, "{}", printer(s)).expect("Cannot write to stream");
        }
    } else if m.get_flag("word") {
        for s in text.unicode_words() {
            writeln!(ioout, "{}", printer(s)).expect("Cannot write to stream");
        }
    } else if m.get_flag("sentence") {
        for s in text.unicode_sentences() {
            writeln!(ioout, "{}", printer(s)).expect("Cannot write to stream");
        }
    } else {
        for c in text.chars() {
            let name = &match uni_name(c) {
                Some(n) => n.to_string(),
                None => "<NA>".to_string(),
            };
            let block = match find_unicode_block(c) {
                Some(blk) => blk.name(),
                None => "No Block",
            };
            writeln!(
                ioout,
                "{}, U+{:04X}, Script: '{:?}', Block: '{}', Name: '{}'",
                c,
                c as u32,
                c.script(),
                block,
                name
            )
            .expect("Cannot write to stream");
        }
    }
}

fn get_command_str<'a, 'b>(m: &'a ArgMatches, id: &'b str) -> String {
    normalize_quote(m.get_one::<String>(id).unwrap()).to_owned()
}

fn get_command_str_except<'a, 'b>(m: &'a ArgMatches, id: &'b str, except: &'b str) -> String {
    normalize_quote(m.get_one::<String>(id).expect(except)).to_owned()
}

fn round_n(num: f64, prec: i32) -> f64 {
    let mul = 10f64.powi(prec);
    (num * mul).round_ties_even() / mul
}

fn normalize_quote(s: &str) -> &str {
    if s.len() >= 2 {
        let idx_offset = (s.starts_with('\'') && s.ends_with('\'')) as usize;
        &s[idx_offset..s.len() - idx_offset]
    } else {
        s
    }
}
