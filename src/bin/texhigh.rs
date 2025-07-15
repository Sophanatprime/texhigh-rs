use std::ffi::OsString;
use std::process::Command;
use texhigh::{
    command_font, command_high, command_layout, command_text, get_matches,
};

fn main() {
    let m = get_matches();
    match m.subcommand() {
        Some(("high", high)) => command_high(high),
        Some(("font", font)) => command_font(font),
        Some(("layout", layout)) => command_layout(layout),
        Some(("text", text)) => command_text(text),
        Some((ext_subcmd, ext_m)) => external_subcommand(
            ext_subcmd,
            ext_m.get_many::<OsString>("").unwrap().collect(),
        ),
        None => {}
    }
}

fn external_subcommand(cmd_name: &str, args: Vec<&OsString>) {
    use which::which_in;
    let current_exe =
        std::env::current_exe().expect("failed to get current exe path");
    let current_dir =
        std::env::current_dir().expect("failed to get current directory");
    let cmd_name = format!("texhigh-{}", cmd_name);
    if let Ok(which_path) =
        which_in(&cmd_name, Some(&current_exe), &current_dir)
    {
        let mut cmd = Command::new(&which_path);
        cmd.args(&args).spawn().expect(&format!(
            "Cannot run subcommand '{}' with args: '{:?}'",
            cmd_name, args
        ));
    } else {
        eprintln!("Unknown subcommand '{}'", cmd_name)
    }
}
