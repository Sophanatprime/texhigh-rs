#![feature(
    step_trait,
    iter_advance_by,
    round_char_boundary,
    debug_closure_helpers
)]
// #![allow(unused)]

use clap::{arg, value_parser, Arg, ArgAction, ArgMatches, Command};
use env_logger;
use std::{env::set_var as set_env_var, env::var as get_env_var, ffi::OsString};

mod language;
pub mod unicode;
pub use language::{is_same_primary_language, language, language_str, Language, PrimaryLanguage};
mod kpathsea;
mod tex;
pub use kpathsea::KpseWhich;
pub mod config;
pub mod high;
pub mod layout;
pub mod range;
// mod regtex;
pub mod fonts;
pub mod types;

const VERSION: i32 = 0;
const REVERSION: &str = ".1.4";
const DATE: &str = "2025/01/01";
const COPYRIGHT: &str = "2024-2025, Wenjian Chern ©";

pub fn get_matches() -> ArgMatches {
    let high = Command::new("high")
        .about("Highlight TeX texts and files")
        .arg(
            Arg::new("current-ctab")
                .long("current-ctab")
                .alias("cc")
                .default_value("latex")
                .help("Set current catcode table"),
        )
        .arg(
            Arg::new("ctab-set")
                .long("ctab-set")
                .alias("cs")
                .help("Parse catcode table set from texts")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("ctab")
                .long("ctab")
                .short('C')
                .value_names(["name", "ctab"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse catcode table from texts"),
        )
        .arg(
            Arg::new("ctab-file")
                .long("ctab-file")
                .short('F')
                .action(ArgAction::Append)
                .help("Parse catcode table from files"),
        )
        .arg(
            Arg::new("config")
                .long("config")
                .alias("c")
                .value_names(["key", "value"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse config from texts"),
        )
        .arg(
            Arg::new("config-file")
                .long("config-file")
                .alias("cf")
                .help("Parse config from files")
                .action(ArgAction::Append)
                .conflicts_with("config"),
        )
        .arg(
            Arg::new("kpse-config-file")
                .long("kpse-config-file")
                .alias("kcf")
                .action(ArgAction::SetTrue)
                .help("Use kpsewhich to search config files."),
        )
        .arg(
            Arg::new("text")
                .long("text")
                .short('t')
                .value_name("TEXT")
                .help("Text to be highlight"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .num_args(1..)
                .value_name("FILE")
                .help("Files to be highlight")
                .conflicts_with("text"),
        )
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .help("File to be output"),
        )
        .arg(
            Arg::new("kpse-args")
                .last(true)
                .num_args(0..)
                .help("Arguments for kpsewhich"),
        );

    let font = Command::new("font")
        .subcommand(
            Command::new("build")
                .arg(
                    Arg::new("no-default-paths")
                        .long("no-default-paths")
                        .action(ArgAction::SetTrue)
                        .help("Do not use default paths"),
                )
                .arg(
                    Arg::new("paths")
                        .num_args(0..)
                        .help("Set paths of fonts to be added"),
                ),
        )
        .subcommand(
            Command::new("find")
                .arg(
                    Arg::new("unwrapped")
                        .long("unwrapped")
                        .short('W')
                        .action(ArgAction::SetTrue)
                        .help("Do not wrap text when text is too long"),
                )
                .arg(
                    Arg::new("borderless")
                        .long("borderless")
                        .short('B')
                        .action(ArgAction::SetTrue)
                        .help("Do not display borders"),
                )
                .arg(
                    Arg::new("local")
                        .long("local")
                        .short('L')
                        .action(ArgAction::SetTrue)
                        .conflicts_with_all(["only-family", "starts", "fuzzy", "full", "short"])
                        .help("Do not search database, reading font from local directory"),
                )
                .arg(
                    Arg::new("only-family")
                        .long("only-family-name")
                        .alias("only-family")
                        .short('N')
                        .action(ArgAction::SetTrue)
                        .help("Match family name only and exactly"),
                )
                .arg(
                    Arg::new("starts")
                        .long("starts")
                        .short('S')
                        .action(ArgAction::SetTrue)
                        .conflicts_with_all(["only-family"])
                        .help("Match starts of the font name"),
                )
                .arg(
                    Arg::new("fuzzy")
                        .long("fuzzy")
                        .short('F')
                        .num_args(0..=1)
                        .default_missing_value("0.7")
                        .value_parser(value_parser!(f64))
                        .help("Use fuzzy matching, range [0.0, 1.0]"),
                )
                .arg(
                    Arg::new("info")
                        .long("info")
                        .short('i')
                        .action(ArgAction::SetTrue)
                        .help("Read font file and display informations"),
                )
                .arg(
                    Arg::new("full")
                        .long("full")
                        .short('f')
                        .action(ArgAction::SetTrue)
                        .help("Display extra informations"),
                )
                .arg(
                    Arg::new("short")
                        .long("short")
                        .short('s')
                        .action(ArgAction::SetTrue)
                        .help("Display path, index and family name only"),
                )
                .arg(
                    Arg::new("name")
                        .num_args(1)
                        .help("Font name or font path to be searched"),
                ),
        );
    let layout = Command::new("layout")
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .help("Output to a image or terminal"),
        )
        .arg(
            Arg::new("fontsize")
                .long("fontsize")
                .short('s')
                .value_parser(value_parser!(f32))
                .default_value("0.0")
                .help("Fontsize of the text, in bp, range (0, +inf)"),
        )
        .arg(
            Arg::new("fonts")
                .long("fonts")
                .short('f')
                .action(ArgAction::Append)
                .help("Main font and its fallback fonts for the text"),
        )
        .arg(
            Arg::new("width")
                .long("widtg")
                .short('w')
                .default_value("0.0")
                .value_parser(value_parser!(f32))
                .help("Maximum width of text, in bp, range (0, +inf)"),
        )
        .arg(Arg::new("text").required(true));

    let matches = Command::new("texhigh")
        .about("TeX Helper in graphics and hypertext")
        .arg(
            Arg::new("no-banner")
                .long("no-banner")
                .alias("nb")
                .action(ArgAction::SetTrue)
                .help("Do not print banner"),
        )
        .arg(
            arg!(--"logging-level" <LEVEL> "Set logging level")
                .alias("ll")
                .value_parser(["error", "warn", "info", "debug", "trace"]),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .conflicts_with("logging-level")
                .help(
                    "Increase log verbosity, (-v =: 1, -vv =: 2, ...)\n\
                     1=error, 2=warn, 3=info, 4=debug, 5=trace, 6=off",
                ),
        )
        .subcommand(high)
        .subcommand(font)
        .subcommand(layout)
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
        None => match matches.get_count("verbose") {
            0 => {}
            1 => set_env_var("RUST_LOG", "error"),
            2 => set_env_var("RUST_LOG", "warn"),
            3 => set_env_var("RUST_LOG", "info"),
            4 => set_env_var("RUST_LOG", "debug"),
            5 => set_env_var("RUST_LOG", "trace"),
            6 => set_env_var("RUST_LOG", "off"),
            _ => set_env_var("RUST_LOG", "trace"),
        },
    }

    env_logger::init();
    log::info!(
        "Logging info: {}",
        get_env_var("RUST_LOG").unwrap_or("<DEFAULT>".to_string())
    );

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
