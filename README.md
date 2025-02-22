# texhigh

👉 [中文版](README-zh_CN.md)

**TeX** **H**elper **i**n **g**raphics and **h**ypertext.

## Installation

Copy the executable file to the `bin` directory of your TeX environment, ensuring it is in the same directory as `kpsewhich`.

## Building

Ensure that the Rust environment is correctly installed and that the nightly channel (`>=1.86.0`) is installed.

```bash
git clone https://github.com/Sophanatprime/texhigh-rs.git
cd texhigh-rs && cargo +nightly build --release
```

## Usage

```
texhigh [OPTIONS] [COMMAND]
```

Options:
- `--nb` `--no-banner`: Do not display the copyright information.
- `-v [-v ...]` `--verbose [--verbose ...]`: Display errors, warnings, logs, etc.
- `--ll <LEVEL>` `--logging-level <LEVEL>`: Set the logging level. Available values: `error`, `warn`, `info`, `debug`, `trace`.
- `-h` `--help`: Print the help text.

### `high` Subcommand

This command is used to highlight TeX text. The [texhigh](https://github.com/Sophanatprime/texhigh) package encapsulates this command, making it easy to use in LaTeX.

```
texhigh high [OPTIONS] [-- [kpse-args]...]
```

Options:
- `-e` `--enhanced`: Enable enhanced mode, which supports more features but slightly increases processing time (about 10%). It is recommended to enable this option.
- `--cc <CTAB NAME>` `--current-ctab <CTAB NAME>`: Set the Catcode Table used when parsing TeX text. Defaults to the Catcode used in LaTeX text. Available values: `initex`, `document`, `latex`, `atletter`, `latexcode`, `explon`, `atandexpl`, `latex3`, `latex3code`, `cjk`, `cjkl3`, `cjkcode`, `cjkl3code`, and Catcode Tables set in the configuration file.
- `--cs <CTAB SET>` `--ctab-set <CTAB SET>`: Save additional Catcode Tables for this use. See [Catcode Tables](#catcode-tables) for usage.
- `-C <NAME> <CTAB VALUE>` `--ctab <NAME> <CTAB VALUE>`: Save additional Catcode Tables for this use. See [Catcode Tables](#catcode-tables) for usage.
- `--CB <NAME> <CTAB>` `--ctab-base64 <NAME> <CTAB>`: Save additional Catcode Tables for this use, where `<CTAB>` is Base64 encoded. See [Catcode Tables](#catcode-tables) for usage.
- `-F <CTAB FILE>` `--ctab-file <CTAB FILE>`: Parse the Catcode Table from `<FILE>`.
- `--c <KEY> <VALUE>` `--config <KEY> <VALUE>`: Set the value of the configurable key `<KEY>`. Available keys can be found in [Configurable Items](#configurable-items).
- `--cb <KEY> <VALUE>` `--config-base64 <KEY> <VALUE>`: Set the value of the configurable key `<KEY>`, where `<VALUE>` is Base64 encoded. Available keys can be found in [Configurable Items](#configurable-items).
- `--cf <CONFIG FILE>` `--config-file <CONFIG FILE>`: Parse the configuration from a [TOML](https://toml.io/) file.
- `--kcf` `--kpse-config-file`: Use `kpsewhich` to locate the configuration file.
- `-t <TEXT>` `--text <TEXT>`: Set the TeX text to be parsed.
- `--tb <TEXT>` `--text-base64 <TEXT>`: Set the TeX text to be parsed, Base64 encoded.
- `-f <FILE 1> [<FILE 2> ...]` `--file <FILE 1> [<FILE 2> ...]`: Set the TeX file(s) to be parsed.
- `-o <OUTPUT>` `--output <OUTPUT>`: Set the output file name or directory. If not set, output will be sent to the stdout.
- `-h` `--help`: Print the help text.

#### Catcode Tables

A Catcode Table consists of a table name and character-catcode pairs. The table name is composed of characters and numbers. The following is an example of a valid Catcode Table:

```
[cjk-doc]
13 = 5  /* <return> */
32 = 10 /* <space> */
0  = 9  /* <null> */
16 = 15 /* <delete> */
`A..`Z=11 /* characters for A to Z */
`a..`z=11
`\% =14 /* escaped character */
`\\ =0
CJK_UNIFIED_IDEOGRAPHS = 11 /* Unicode block name */
```

The name of the table is enclosed in square brackets. Each line contains a character before the equals sign and the corresponding catcode after the equals sign.

The default Catcode Tables can be found in the [prelude-ctabset](src/prelude-ctabset.thcs) file.

#### Configurable Items

All configurable items can be found in the [prelude-config](src/prelude-config.toml) file.

#### Plain Text Regular Expressions

Plain text regular expressions are regular expressions for plain text. `texhigh` uses Rust's [`regex`](https://crates.io/crates/regex) crate. The full syntax can be found in [its documentation](https://docs.rs/regex/latest/regex/#syntax).

#### TeX Regular Expressions

Since TeX does not store its internal symbols as plain text, regular expressions for plain text cannot correctly match TeX's internal symbols. `texhigh` supports TeX regular expressions, allowing it to match TeX internal symbols, such as control sequences (Control Sequence), character catcodes, etc.

The TeX regular expressions supported by `texhigh` are generally similar to string regular expressions. Their syntax is almost identical to LaTeX3's `l3regex` module, except that `\b`, `\B`, `\G`, and `\u` escape sequences are not supported, and the negation of the `\c` escape sequence is not supported (i.e., `[^\c{begin}\c{end}]` is not supported).

### `font` Subcommand

This command is used to find fonts in the TeX system.

```
texhigh font [COMMAND]
```

#### `build` Command

```
texhigh font build [OPTIONS] [FONT PATHS]...
```

The `build` command is used to build a font database.

`texhigh` will read OpenType fonts from the font paths and all their subdirectories and symbolic links. Invalid paths will be ignored.

Options:
- `--no-default-paths`: Do not use the default font paths.
The default font paths are:
    * Windows: `C:\Windows\Fonts`;
    * MacOS: `/Library/Fonts` and `/System/Library/Fonts`;
    * Other systems: `/usr/local/share/fonts` and `/usr/share/fonts`;
    * If `texhigh` is in the same directory as `kpsewhich`, it will also search for OpenType font paths of the TeX environment.
- `-h` `--help`: Print the help text.

#### `find` Command

```
texhigh font find [OPTIONS] [NAME]
```

The `find` command is used to find fonts and view font information. It prints information about all fonts in the database that match `NAME`, where `NAME` can be a font name or font file name.

Options:
- `-s` `--short`: Print the path, index in the font file, and family names of all fonts in the database that match `NAME`.
- `-f` `--full`: Print information about all fonts in the database that match `NAME`.
- `-i` `--info`: Read all fonts in the database that match `NAME` and print their information.
- `-N` `--only-family-name`: When searching the database, only match the font family name.
- `-F [<FUZZY>]` `--fuzzy [<FUZZY>]`: Enable fuzzy matching, where `<FUZZY>` is a value between 0 and 1.
- `-S` `--starts`: Only match if the font name starts with `NAME`.
- `-L` `--local`: Read the font file `NAME` without searching the database and print its information.
- `-W` `--unwrapped`: When using the `-i` option, text is not automatically wrapped. It is recommended to enable this option when further processing the output information.
- `-B` `--borderless`: When using the `-i` option, text is not wrapped in boxes. It is recommended to enable this option when further processing the output information is needed.
    For example, use [ripgrep](https://github.com/BurntSushi/ripgrep) to search for font paths and names in the output:
    ```
    texhigh font find "Arial Black" -iWB | rg "Path|Family|Name"
    ```
    Result:
    ```
    Path:             C:/Windows/Fonts/ariblk.ttf (163.66 KiB)
    Family:           Arial Black
    Full Name:        Arial Black
    Postscript Name:  Arial-Black
    Prefer Family:    Arial
    Font Family Class and Subclass: Sans Serif, Neo-grotesque Gothic
    ```

### `text` Subcommand

This command is used to display text information.

```
texhigh text [OPTIONS] [TEXT]
```

Output the Unicode code point, script, line break, etc., of the characters in `TEXT`. If `TEXT` is not provided, it will be read from stdin.

Options:
- `-f` `--from-unicode` `--from-escaped`: Convert Unicode escape sequences in the text to characters. The format of the escape sequences is `\uxxxx` or `\u{x...}`, where `x` is a hexadecimal digit.
- `-t` `--to-unicode` `--escaped`: Output the text in the form of Unicode escape sequences.
- `-c` `--cluster` `-g` `--grapheme`: Split the text according to grapheme clusters.
- `-w` `--word`: Split the text according to words.
- `-s` `--sentence`: Split the text according to sentences.
- `-l` `--linebreak`: Output line break information for the text.
- `-i` `--information`: Output information about each character in the text, including scalar value, script, general category, block, and name.
- `-n <NOR>` `--normalization <NOR>`: Output the text after normalization. Available normalization methods: `nfd`, `nfkd`, `nfc`, `nfkc`, `cjk`, `safe`.

### `layout` Subcommand (Experimental)

This command is used to layout text.

```
texhigh layout [OPTIONS] <text>
```

## LICENSE

[GNU GPL Version 3](https://www.gnu.org/licenses/gpl-3.0.html)
