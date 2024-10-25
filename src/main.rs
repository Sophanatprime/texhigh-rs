use std::fs::{self, File};
use std::io::{self, BufReader, BufWriter};
use std::iter::zip;
use std::str::FromStr;

use clap::ArgMatches;
use config;
use log::{info, warn};
use texhigh::config::{HighConfig, THConfig};
use texhigh::high::StandardFormatter;
use texhigh::types::{CTabSet, CatcodeStack, TokenList};
use texhigh::{get_kpse_matches, get_matches, KpseWhich};

fn main() {
    let m = get_matches();

    let th_config = get_thconfig(&m);

    let ctabset = &th_config.ctabs;
    let ctab_name = m.get_one::<String>("current-ctab").unwrap();
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
        // We strip exactly one set of ' character.
        let text = m.get_one::<String>("text").unwrap();
        let s_idx = text.starts_with('\'') as usize;
        let e_idx = (s_idx != 0 && text.ends_with('\'')) as usize;
        let text = text[s_idx..text.len() - e_idx].to_string();
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
        let out = std::path::Path::new(m.get_one::<String>("output").unwrap());
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
        unsafe {
            for k in m
                .get_occurrences::<String>("config")
                .unwrap()
                .map(Iterator::collect::<Vec<_>>)
            {
                let val = *k.get_unchecked(1);
                let s_idx = val.starts_with("'") as usize;
                let e_idx = (s_idx != 0 && val.ends_with("'")) as usize;
                con.push(format!(
                    "{} = '''{}'''",
                    k.get_unchecked(0),
                    &val[s_idx..val.len() - e_idx]
                ));
            }
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
