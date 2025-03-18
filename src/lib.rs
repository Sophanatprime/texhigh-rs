#![feature(step_trait, iter_advance_by, debug_closure_helpers)]
// #![allow(unused)]

use clap::{arg, value_parser, Arg, ArgAction, ArgMatches, Command};
use env_logger;
use std::{
    env::set_var as set_env_var, env::var as get_env_var, ffi::OsString,
};

mod language;
pub mod unicode;
pub use language::{
    is_same_primary_language, language, language_str, Language,
    PrimaryLanguage,
};
mod kpathsea;
mod tex;
pub use kpathsea::KpseWhich;
pub use tex::{get_cs_type, get_cs_type_re, primitive_engine, LaTeXType};
pub mod config;
pub mod fonts;
pub mod high;
pub mod layout;
pub mod range;
pub mod regtex;
pub mod tokenlist;
pub mod types;

const FULL_VERSION: &str = env!("CARGO_PKG_VERSION");
const DATE: &str = "2025/03/18";
const COPYRIGHT: &str = "2024-2025, Wenjian Chern ©";

pub fn get_matches() -> ArgMatches {
    let high = Command::new("high")
        .about("Highlight TeX texts and files")
        .arg(
            Arg::new("enhanced")
                .long("enhanced")
                .short('e')
                .action(ArgAction::SetTrue)
                .help("Enable enhanced mode for highlighting."),
        )
        .arg(
            Arg::new("current-ctab")
                .long("current-ctab")
                .alias("cc")
                .default_value("latex")
                .help("Set the current catcode table to use for highlighting."),
        )
        .arg(
            Arg::new("ctab-set")
                .long("ctab-set")
                .alias("cs")
                .action(ArgAction::Append)
                .help("Parse and set catcode tables from the provided text."),
        )
        .arg(
            Arg::new("ctab")
                .long("ctab")
                .short('C')
                .value_names(["name", "ctab"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse a catcode table from the provided text."),
        )
        .arg(
            Arg::new("ctab-base64")
                .long("ctab-base64")
                .alias("CB")
                .value_names(["name", "ctab"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse a catcode table from the provided text where the second value is base64 encoded."),
        )
        .arg(
            Arg::new("ctab-file")
                .long("ctab-file")
                .short('F')
                .action(ArgAction::Append)
                .help("Parse a catcode table from the specified file."),
        )
        .arg(
            Arg::new("config")
                .long("config")
                .alias("c")
                .value_names(["key", "value"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse a configuration setting from the provided text."),
        )
        .arg(
            Arg::new("config-base64")
                .long("config-base64")
                .alias("cb")
                .value_names(["key", "value"])
                .num_args(2)
                .action(ArgAction::Append)
                .help("Parse a configuration setting from the provided text where the second value is base64 encoded."),
        )
        .arg(
            Arg::new("config-file")
                .long("config-file")
                .alias("cf")
                .help("Parse a configuration setting from the specified file.")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("kpse-config-file")
                .long("kpse-config-file")
                .alias("kcf")
                .action(ArgAction::SetTrue)
                .help("Use `kpsewhich` to search for configuration files."),
        )
        .arg(
            Arg::new("text")
                .long("text")
                .short('t')
                .value_name("TEXT")
                .help("Specify the text to be highlighted."),
        )
        .arg(
            Arg::new("text-base64")
                .long("text-base64")
                .alias("tb")
                .value_name("TEXT")
                .help("Specify the base64 encoded text to be highlighted."),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .num_args(1 ..)
                .value_name("FILE")
                .help("Specify one or more files to be highlighted. Conflicts with --text.")
                .conflicts_with("text"),
        )
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .help("Specify the output file or directory for the highlighted content."),
        )
        .arg(
            Arg::new("kpse-args")
                .last(true)
                .num_args(0 ..)
                .help("Pass additional arguments to `kpsewhich`."),
        );
    let font = Command::new("font")
        .about("Utilities for managing and querying fonts")
        .subcommand(
            Command::new("build")
                .about("Build font configurations")
                .arg(
                    Arg::new("no-default-paths")
                        .long("no-default-paths")
                        .action(ArgAction::SetTrue)
                        .help("Disable the use of default font paths."),
                )
                .arg(
                    Arg::new("paths")
                        .num_args(0 ..)
                        .help("Specify custom paths to fonts to be added."),
                ),
        )
        .subcommand(
            Command::new("find")
                .about("Search for fonts based on specified criteria")
                .arg(
                    Arg::new("unwrapped")
                        .long("unwrapped")
                        .short('W')
                        .action(ArgAction::SetTrue)
                        .help("Disable text wrapping when the output text is too long."),
                )
                .arg(
                    Arg::new("borderless")
                        .long("borderless")
                        .short('B')
                        .action(ArgAction::SetTrue)
                        .help("Disable the display of borders in the output."),
                )
                .arg(
                    Arg::new("local")
                        .long("local")
                        .short('L')
                        .action(ArgAction::SetTrue)
                        .conflicts_with_all(["only-family", "starts", "fuzzy", "full", "short"])
                        .help("Search for fonts only in the local directory, bypassing the database."),
                )
                .arg(
                    Arg::new("only-family")
                        .long("only-family-name")
                        .alias("only-family")
                        .short('N')
                        .action(ArgAction::SetTrue)
                        .help("Match only the exact family name of the font."),
                )
                .arg(
                    Arg::new("starts")
                        .long("starts")
                        .short('S')
                        .action(ArgAction::SetTrue)
                        .conflicts_with_all(["only-family"])
                        .help("Match fonts whose names start with the specified string."),
                )
                .arg(
                    Arg::new("fuzzy")
                        .long("fuzzy")
                        .short('F')
                        .num_args(0 ..= 1)
                        .default_missing_value("0.7")
                        .value_parser(value_parser!(f64))
                        .help("Enable fuzzy matching with a similarity threshold in the range [0.0, 1.0]. Defaults to 0.7 if no value is provided."),
                )
                .arg(
                    Arg::new("info")
                        .long("info")
                        .short('i')
                        .action(ArgAction::SetTrue)
                        .help("Display detailed information about the font file."),
                )
                .arg(
                    Arg::new("full")
                        .long("full")
                        .short('f')
                        .action(ArgAction::SetTrue)
                        .help("Display extended information about the font."),
                )
                .arg(
                    Arg::new("short")
                        .long("short")
                        .short('s')
                        .action(ArgAction::SetTrue)
                        .help("Display only the font path, index, and family name."),
                )
                .arg(
                    Arg::new("name")
                        .num_args(1)
                        .help("Specify the font name or path to search for."),
                ),
        );
    let layout = Command::new("layout")
        .arg(
            Arg::new("output")
                .long("output")
                .short('o')
                .help("Specify the output destination. Options: 'picture', 'general', or a path to a PNG file."),
        )
        .arg(
            Arg::new("fontsize")
                .long("fontsize")
                .short('s')
                .value_parser(value_parser!(f32))
                .default_value("10.0")
                .help("Set the font size of the text in bp (big points). Must be greater than 0.0."),
        )
        .arg(
            Arg::new("lineheight")
                .long("lineheight")
                .short('l')
                .value_parser(value_parser!(f32))
                .default_value("0.0")
                .help("Set the line height of the text in bp (big points). Must be greater than 0.0."),
        )
        .arg(
            Arg::new("fonts")
                .long("fonts")
                .short('f')
                .action(ArgAction::Append)
                .help("Specify the main font and its fallback fonts. System fonts are loaded automatically."),
        )
        .arg(
            Arg::new("width")
                .long("width")
                .short('w')
                .default_value("0.0")
                .value_parser(value_parser!(f32))
                .help("Set the maximum width of the text in bp (big points). Must be greater than 0.0."),
        )
        .arg(
            Arg::new("base64")
                .long("base64")
                .action(ArgAction::SetTrue)
                .help("Indicate whether the text is encoded in Base64."),
        )
        .arg(Arg::new("text").required(true));
    let text = Command::new("text")
        .about("Display text information, such as character boundaries or names.")
        .arg(
            Arg::new("cluster")
                .long("cluster")
                .alias("grapheme")
                .short('c')
                .short_alias('g')
                .action(ArgAction::SetTrue)
                .help("Display text by extended grapheme cluster boundaries.")
                .conflicts_with_all(["word", "sentence", "linebreak", "information"]),
        )
        .arg(
            Arg::new("word")
                .long("word")
                .short('w')
                .action(ArgAction::SetTrue)
                .help("Display text by word boundaries.")
                .conflicts_with_all(["sentence", "linebreak", "information"]),
        )
        .arg(
            Arg::new("sentence")
                .long("sentence")
                .short('s')
                .action(ArgAction::SetTrue)
                .help("Display text by sentence boundaries.")
                .conflicts_with_all(["linebreak", "information"]),
        )
        .arg(
            Arg::new("linebreak")
                .long("linebreak")
                .short('l')
                .action(ArgAction::SetTrue)
                .help("Display text by line break points.")
                .conflicts_with_all(["information"]),
        )
        .arg(
            Arg::new("information")
                .long("information")
                .alias("info")
                .short('i')
                .action(ArgAction::SetTrue)
                .help("List detailed information for each character in the text."),
        )
        .arg(
            Arg::new("from-unicode")
                .long("from-unicode")
                .visible_alias("from-escaped")
                .short('f')
                .action(ArgAction::SetTrue)
                .help("Parse and print text from Unicode escape sequences."),
        )
        .arg(
            Arg::new("to-unicode")
                .long("to-unicode")
                .visible_alias("escaped")
                .short('t')
                .visible_short_alias('e')
                .action(ArgAction::SetTrue)
                .help("Print the Unicode escape sequences for the text."),
        )
        .arg(
            Arg::new("normalization")
                .long("normalization")
                .short('n')
                .value_parser(["nfd", "nfkd", "nfc", "nfkc", "cjk", "safe"])
                .help("Apply Unicode normalization to the text."),
        )
        .arg(
            Arg::new("text")
                .help("The text to process. If not provided, text will be read from stdin.")
                .num_args(0..=1),
        );

    let matches = Command::new("texhigh")
        .about("TeX Helper in graphics and hypertext")
        .version(FULL_VERSION)
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
        .subcommand(text)
        .get_matches();

    if !matches.get_flag("no-banner") {
        println!(
            "This is TeXHigh, version {} ({}), copyright {}.",
            FULL_VERSION, DATE, COPYRIGHT
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
                .num_args(0 ..= 1)
                .conflicts_with("texinputs"),
        )
        .get_matches_from(s);
    matches
}
