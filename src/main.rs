use std::fs::{self, File};
use std::io;
use std::iter::zip;
use std::str::FromStr;

use config;
use clap::ArgMatches;
use texhigh::config::{THConfig, HighConfig};
use texhigh::get_matches;
use texhigh::high::{StandardFormatter, HighFormat};
use texhigh::types::{CTabSet, TokenList};

fn main() {
    let m = get_matches();

    let th_config = get_thconfig(&m);

    let ctabset = &th_config.ctabs;
    let ctab_name = m.get_one::<String>("current-ctab").unwrap();
    let ctab = ctabset
        .get_by_name(ctab_name)
        .expect("Unknown currant catcode table name.");

    let tokenlist_str = if m.contains_id("text") {
        m.get_one::<String>("text").unwrap().to_string()
    } else if m.contains_id("file") {
        let f =
            File::open(m.get_one::<String>("file").unwrap()).expect("Unable open tokenlist file");
        io::read_to_string(&f).expect("Unable read tokenlist file")
    } else {
        String::new()
    };
    if !tokenlist_str.is_empty() {
        let tokenlist = TokenList::parse(tokenlist_str, &ctab);
        let fm = StandardFormatter::new(&th_config.high_config);
        if m.contains_id("output") {
            let mut f = fs::File::create(m.get_one::<String>("output").unwrap())
                .expect("Unable open output file");
            fm.fmt_tokenlist(&mut f, &tokenlist).unwrap();
        } else {
            let mut f = io::stdout().lock();
            fm.fmt_tokenlist(&mut f, &tokenlist).unwrap();
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
            let f = File::open(ct).expect("Unknown ctab-file");
            let s = io::read_to_string(&f).expect("Unable read ctab-file");
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
    let mut config = Config::builder();
    config = config.add_source(File::from_str(include_str!("prelude-config.toml"), FileFormat::Toml));
    let mut config_list: Vec<(usize, String, bool)> = Vec::new(); // bool: is file or not.
    if m.contains_id("config-file") {
        let con_file = m.get_many::<String>("config-file").unwrap().map(|s| s.to_string()).collect::<Vec<_>>();
        let config_indices = m.indices_of("config-file").unwrap().collect::<Vec<_>>();
        zip(config_indices, con_file).for_each(|(c, i)| config_list.push((c, i, true)));
    }
    if m.contains_id("config") {
        let mut con = Vec::new();
        unsafe {
            for k in m.get_occurrences::<String>("config").unwrap().map(Iterator::collect::<Vec<_>>) {
                con.push(format!("'{}' = \"\"\"{}\"\"\"", k.get_unchecked(0), k.get_unchecked(1)));
            }
        }
        let config_indices = m.indices_of("config").unwrap().step_by(2).collect::<Vec<_>>();
        zip(config_indices, con).for_each(|(c, i)| config_list.push((c, i, false)));
    }
    config_list.sort_by_key(|(i, _, _)| *i);
    for (_, config_str, is_file) in config_list.iter() {
        if *is_file {
            config = config.add_source(File::new(config_str, FileFormat::Toml));
        } else {
            config = config.add_source(File::from_str(config_str, FileFormat::Toml));
        }
    }
    config.build().expect("Cannot parse configs").try_deserialize().unwrap()
}
