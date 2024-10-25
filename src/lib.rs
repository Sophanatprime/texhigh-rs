#![feature(step_trait, iter_advance_by, round_char_boundary)]
// #![allow(unused)]

use clap::{arg, Arg, ArgAction, ArgMatches, Command};
use env_logger;
use std::{env::set_var as set_env_var, ffi::OsString};

mod kpathsea;
mod tex;
pub use kpathsea::KpseWhich;
pub mod config;
pub mod high;
pub mod range;
pub mod types;
mod unicode;

const VERSION: i32 = 0;
const REVERSION: &str = ".1.3";
const DATE: &str = "2024/10/25";
const COPYRIGHT: &str = "2024, Wenjian Chern ©";

pub fn get_matches() -> ArgMatches {
    let matches = Command::new("texhigh")
        .about("Highlight TeX texts")
        .arg(
            Arg::new("no-banner")
                .long("no-banner")
                .alias("nb")
                .action(ArgAction::SetTrue)
                .help("do not print banner"),
        )
        .arg(
            arg!(--"logging-level" <LEVEL>)
                .alias("ll")
                .value_parser(["error", "warn", "trace", "debug", "info"]),
        )
        .arg(
            Arg::new("current-ctab")
                .long("current-ctab")
                .alias("cc")
                .default_value("latex")
                .help("set current catcode table"),
        )
        .arg(
            Arg::new("ctab-set")
                .long("ctab-set")
                .alias("cs")
                .help("text of catcode table")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("ctab")
                .long("ctab")
                .short('C')
                .value_names(["name", "ctab"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("catcode table"),
        )
        .arg(
            Arg::new("ctab-file")
                .long("ctab-file")
                .short('F')
                .action(ArgAction::Append)
                .help("file of catcode table"),
        )
        .arg(
            Arg::new("config")
                .long("config")
                .alias("c")
                .value_names(["key", "value"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("text of config"),
        )
        .arg(
            Arg::new("config-file")
                .long("config-file")
                .alias("cf")
                .help("file of config")
                .action(ArgAction::Append)
                .conflicts_with("config"),
        )
        .arg(
            Arg::new("kpse-config-file")
                .long("kpse-config-file")
                .alias("kcf")
                .action(ArgAction::SetTrue)
                .help("use kpsewhich to search config files."),
        )
        .arg(
            Arg::new("text")
                .long("text")
                .short('t')
                .value_name("TEXT")
                .help("text to be highlight"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .num_args(1..)
                .value_name("FILE")
                .help("file to be highlight")
                .conflicts_with("text"),
        )
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .help("output to file"),
        )
        .arg(
            Arg::new("kpse-args")
                .last(true)
                .num_args(0..)
                .help("arguments for kpsewhich"),
        )
        .get_matches();

    if !matches.get_flag("no-banner") {
        println!(
            "This is TeXHigh, version {}{} ({}), copyright {}.",
            VERSION, REVERSION, DATE, COPYRIGHT
        );
    }
    match matches.get_one::<String>("logging-level") {
        Some(level) => {
            set_env_var("RUST_LOG", level);
        }
        None => {}
    }
    env_logger::init();

    matches
}

pub fn get_kpse_matches<I, T>(s: I) -> ArgMatches
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let matches = Command::new("kpsewhich")
        .no_binary_name(true)
        .arg(Arg::new("encoding").long("encoding"))
        .arg(Arg::new("all").long("all").action(ArgAction::SetTrue))
        .arg(
            Arg::new("must-exist")
                .long("must-exist")
                .alias("mustexist")
                .action(ArgAction::SetTrue),
        )
        .arg(Arg::new("path").long("path"))
        .arg(Arg::new("subdir").long("subdir").alias("sub-dir"))
        .arg(Arg::new("texinputs").long("texinputs").alias("tex-inputs"))
        .arg(
            Arg::new("texpath")
                .long("texpath")
                .alias("tex-path")
                .default_missing_value("")
                .num_args(0..=1)
                .conflicts_with("texinputs"),
        )
        .get_matches_from(s);
    matches
}
