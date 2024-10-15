#![feature(step_trait, char_min, iter_advance_by, round_char_boundary)]
// #![allow(unused)]

use clap::{arg, Arg, ArgAction, Command};
use env_logger;
use std::env::set_var as set_env_var;

pub mod config;
pub mod high;
pub mod range;
pub mod types;

const VERSION: i32 = 0;
const REVERSION: &str = ".1";
const DATE: &str = "2024/06/15";
const COPYRIGHT: &str = "2024, Wenjian Chern ©";

pub fn run() {
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
                .help("text of config")
                .action(ArgAction::Append),
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
                .value_name("FILE")
                .help("files to be highlight")
                .conflicts_with("text"),
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
}
