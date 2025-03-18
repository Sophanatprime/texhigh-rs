# texhigh

👉 [English Version](README.md)

**TeX** **H**elper **i**n **g**raphics and **h**ypertext.

# 安装

将可执行文件复制到 TeX 环境的 `bin` 目录下，保证它与 `kpsewhich` 在同一个目录下即可。

# 构建

确保 Rust 环境已正确安装并且已下载 nightly 频道（`>=1.86.0`）。
```bash
git clone https://github.com/Sophanatprime/texhigh-rs.git
cd texhigh-rs && cargo +nightly build --release
```

# 用法

```
texhigh [选项] [命令]
```

选项：
- `--nb` `--no-banner`，不输出版权信息。
- `-v [-v ...]` `--verbose [--verbose ...]`，显示错误、警告、日志等信息。
- `--ll <LEVEL>` `--logging-level <LEVEL>`，设置日志级别，可用值为 `error` `warn` `info` `debug` `trace`。
- `-h` `--help`，打印帮助文本。

## `high` 子命令

本命令用于高亮 TeX 文本。[texhigh](https://github.com/Sophanatprime/texhigh) 宏包封装了此命令，使其便于在 LaTeX 中使用。

```
texhigh high [选项] [-- [kpse-args]...]
```

选项：
- `-e` `--enhanced`，启用增强模式，它支持更多特性，但处理时间略有延长（约 10%），建议启用此选项。
- `--cc <CTAB NAME>` `--current-ctab <CTAB NAME>`
设置解析 TeX 文本时使用的类别码表（Catcode Table）。默认与 LaTeX 正文所使用的类别码（Catcode）一致。可用值：`initex` `document` `latex` `atletter` `latexcode` `explon` `atandexpl` `latex3` `latex3code` `cjk` `cjkl3` `cjkcode` `cjkl3code`，以及配置文件设置的类别码表。
- `--cs <CTAB SET>` `--ctab-set <CTAB SET>`，为此次执行保存额外的类别码表，用法见[类别码表](#类别码表)。
- `-C <NAME> <CTAB VALUE>` `--ctab <NAME> <CTAB VALUE>`，为此次执行保存额外的类别码表，用法见[类别码表](#类别码表)。
- `--CB <NAME> <CTAB>` `--ctab-base64 <NAME> <CTAB>`，为此次执行保存额外的类别码表，其中 `<CTAB>` 以 Base64 编码，用法见[类别码表](#类别码表)。
- `-F <CTAB FILE>` `--ctab-file <CTAB FILE>`，从 `<FILE>` 解析类别码表。
- `--c <KEY> <VALUE>` `--config <KEY> <VALUE>`，设置可配置键 `<KEY>` 的值，可用的键见[可配置项](#可配置项)。
- `--cb <KEY> <VALUE>` `--config-base64 <KEY> <VALUE>`，设置可配置键 `<KEY>` 的值，其中 `<VALUE>` 以 Base64 编码，可用的键见[可配置项](#可配置项)。
- `--cf <CONFIG FILE>` `--config-file <CONFIG FILE>`，从 [TOML](https://toml.io/) 文件解析配置。
- `--kcf` `--kpse-config-file`，使用 `kpsewhich` 查找配置文件的位置。
- `-t <TEXT>` `--text <TEXT>`，设置要解析的 TeX 文本。
- `--tb <TEXT>` `--text-base64 <TEXT>`，设置要解析的 TeX 文本，以 Base64 编码。
- `-f <FILE 1> [<FILE 2> ...]` `--file <FILE 1> [<FILE 2> ...]`，设置要解析的 TeX 文件。
- `-o <OUTPUT>` `--output <OUTPUT>`，设置输出的文件名或目录，若未设置，则输出到标准输出流。
- `-h` `--help`，打印帮助文本。

`kpse-args` 可用的选项，这些选项和 kpathsea 文档中描述的功能相一致：
- `--all`，列出所有匹配的文件。
- `--must-exist`，尽可能地找到所需的文件，这可能会访问磁盘或执行 `mktex` 等脚本。默认情况下只在 `ls-R` 数据库中查找。
- `--path <paths> `，在指定的路径中查找。会执行路径展开。
- `--subdir <dir>`，只给出搜索结果中目录以 `<dir>` 结尾的文件。这暗含 `--all`。
- `--texinputs <paths>`，修改 `TEXINPUTS` 环境变量，`kpsewhich` 会在此环境变量指定的路径中查找文件。
- `--texpath <paths>`，相当于设置 `--texinputs '<paths>;'`，即除了在默认的目录中查找外，还在 `<paths>` 中查找。
- `--encoding <encoding>`，设置编码。一般无需另行设置。

texhigh 会首先使用 `KPSEWHICH_EXE_FILE` 环境变量中指定的 `kpsewhich` 可执行文件，若未设置该环境变量，则直接调用 `kpsewhich`，不论其存在与否。

### 类别码表

类别码表由表名以及字符-类别码对组成，表名由字符和数字组成，下例是一个有效的类别码表：
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
方括号内是表名，每行等号前为字符，等号后为对应的类别码。

预先保存的类别码表见 [prelude-ctabset](src/prelude-ctabset.thcs) 文件。

### 可配置项

全部可配置项见 [prelude-config](src/prelude-config.toml) 文件。

### 纯文本正则表达式

纯文本正则表达式就是针对纯文本的正则表达式，`texhigh` 使用 Rust 的 [`regex`](https://crates.io/crates/regex) 库，完整的正则表达式语法见[其文档](https://docs.rs/regex/latest/regex/#syntax)。

### TeX 正则表达式

由于 TeX 并非以纯文本存储其内部符号，所以针对纯文本的正则表达式并不能正确匹配 TeX 的内部符号。`texhigh` 支持 TeX 正则表达式，使其能够匹配 TeX 内部符号，例如匹配控制序列（Control Sequence）、字符的类别码等等。

`texhigh` 支持的 TeX 正则表达式总体上与纯文本正则表达式相似，其语法与 LaTeX3 的 `l3regex` 模块近乎完全相同，除了暂不支持 `\b` `\B` `\G` `\u` 这几个转义序列，以及对 `\c` 转义序列取反（即暂不支持 `[^\c{begin}\c{end}]` 这种写法）。

## `font` 子命令

本命令用于查找系统中的字体。

```
texhigh font [命令]
```

### `build` 命令

```
texhigh font build [选项] [字体路径]...
```

`build` 命令用于构建字体数据库。

`texhigh` 会读取字体路径及其所有子目录和符号链接中的 OpenType 字体，无效的路径会被直接忽略。

选项：
- `--no-default-paths`，不使用默认的字体路径。
默认的字体路径为：
    * Windows 系统：`C:\Windows\Fonts`；
    * MacOS 系统：`/Library/Fonts` 和 `/System/Library/Fonts`；
    * 其它系统：`/usr/local/share/fonts` 和 `/usr/share/fonts`；
    * 若 `texhigh` 与 `kpsewhich` 在同一个目录下，还会查找 TeX 环境内的 OpenType 字体路径。
- `-h` `--help`，打印帮助文本。

### `find` 命令

```
texhigh font find [选项] [NAME]
```

`find` 命令用与查找字体和查看字体信息，它打印数据库中所有与 `NAME` 匹配的字体的信息，`NAME` 可以时字体名或字体文件名。

- `-s` `--short`，打印数据库中所有与 `NAME` 匹配的字体所在的路径、在字体文件中的索引以及族名。
- `-f` `--full`，打印数据库中所有与 `NAME` 匹配的字体的信息。
- `-i` `--info`，读取数据库中所有与 `NAME` 匹配的字体，并打印其信息。
- `-N` `--only-family-name`，在数据库中查找时，只匹配字体的族名。
- `-F [<FUZZY>]` `--fuzzy [<FUZZY>]`，启用模糊匹配，`<FUZZY>` 为 0 到 1 之间的值。
- `-S` `--starts`，仅当字体名以 `NAME` 开始才匹配。
- `-L` `--local`，直接读取字体文件 `NAME` 而不在数据库中搜索，并打印其信息。
- `-W` `--unwrapped`，当使用 `-i` 选项时，文字不自动折行，当需要进一步处理输出的信息时，最好开启此选项。
- `-B` `--borderless`，当使用 `-i` 选项时，文字不用方框包裹，当需要进一步处理输出的信息时，最好开启此选项。
    例如，使用 [ripgrep](https://github.com/BurntSushi/ripgrep) 搜索输出结果中的字体路径w和字体名：
    ```
    texhigh font find "Arial Black" -iWB | rg "Path|Family|Name"
    ```
    得到
    ```
    Path:             C:/Windows/Fonts/ariblk.ttf (163.66 KiB)
    Family:           Arial Black
    Full Name:        Arial Black
    Postscript Name:  Arial-Black
    Prefer Family:    Arial
    Font Family Class and Subclass: Sans Serif, Neo-grotesque Gothic
    ```

## `text` 子命令

本命令用于显示文本信息。

```
texhigh text [选项] [文字]
```

输出 `文字` 中字符的 Unicode 代码点、书写系统、断行等信息。若 `文字` 未给出，则从标准输入中读取。

选项：
- `-f` `--from-unicode` `--from-escaped`，将文字中的 Unicode 转义序列转换为字符。转义序列的格式为 `\uxxxx` 或 `\u{x...}`，其中 `x` 为 16 进制数字。
- `-t` `--to-unicode` `--escaped`，输出的文字以 Unicode 转义序列的形式展现。
- `-c` `--cluster` `-g` `--grapheme`，根据子簇分割文字。
- `-w` `--word`，根据单词分割文字。
- `-s` `--sentence`，根据句子分割文字。
- `-l` `--linebreak`，输出文字的断行信息。
- `-i` `--information`，输出文字中每个字符的信息。包括标量值（Scalar Value）、书写系统（Script）、分类（General Category）、区块（Block）以及名称（Name）。
- `-n <NOR>` `--normalization <NOR>`，输出文字经正则化后的结果。可用的正则化方法有：`nfd` `nfkd` `nfc` `nfkc` `cjk` `safe`。

## `layout` 子命令（实验性的）

本命令用于排布文字。

```
texhigh layout [OPTIONS] <text>
```

# LICENSE

[GNU GPL Version 3](https://www.gnu.org/licenses/gpl-3.0.html)
