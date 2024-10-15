#![feature(step_trait, char_min, iter_advance_by)]
// #![allow(unused)]
use clap::{Arg, ArgAction, Command};

mod config;
mod high;
mod range;
mod types;

const VERSION: i32 = 0;
const REVERSION: &str = ".1";
const DATE: &str = "2024/06/15";
const COPYRIGHT: &str = "2024, Wenjian Chern ©";

pub fn run() {
    println!(
        "This is TeXHigh, version {}{} ({}), copyright {}.",
        VERSION, REVERSION, DATE, COPYRIGHT
    );

    let _m = Command::new("texhigh")
        .about("Highlight TeX texts")
        .arg(
            Arg::new("ctab")
                .long("ctab")
                .short('c')
                .help("text of catcode table")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("ctab-file")
                .long("ctab-file")
                .action(ArgAction::Append)
                .help("files of catcode table")
                .conflicts_with("ctab"),
        )
        .arg(
            Arg::new("config")
                .long("config")
                .help("text of config")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("config-file")
                .long("config-file")
                .help("files of config")
                .action(ArgAction::Append)
                .conflicts_with("config"),
        )
        .arg(
            Arg::new("text")
                .long("text")
                .short('t')
                .help("text to be highlight"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("files to be highlight")
                .conflicts_with("text"),
        )
        .get_matches();
}
