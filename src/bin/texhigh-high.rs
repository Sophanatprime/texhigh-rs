use clap::{arg, Arg, ArgAction, ArgMatches, Command};
use std::env::{set_var as set_env_var, var as get_env_var};
use texhigh::{command_high, print_copyright};

fn main() {
    command_high(&get_matches());
}

fn get_matches() -> ArgMatches {
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

    let high = high
        .arg(
            Arg::new("no-banner")
                .global(true)
                .long("no-banner")
                .alias("nb")
                .action(ArgAction::SetTrue)
                .help("Do not print banner"),
        )
        .arg(
            arg!(--"logging-level" <LEVEL> "Set logging level")
                .global(true)
                .alias("ll")
                .value_parser(["error", "warn", "info", "debug", "trace"]),
        )
        .arg(
            Arg::new("verbose")
                .global(true)
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .conflicts_with("logging-level")
                .help(
                    "Increase log verbosity, (-v =: 1, -vv =: 2, ...)\n\
                     1=error, 2=warn, 3=info, 4=debug, 5=trace, 6=off",
                ),
        );

    let matches = high.get_matches();

    if !matches.get_flag("no-banner") {
        print_copyright("This is TeXHigh-high,");
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
