use clap::ArgMatches;
use encoding_rs::Encoding;
use log::{error, info};
use std::env::{remove_var, set_var, var as get_var};
use std::ffi::OsStr;
use std::io;
use std::process::Command;

#[derive(Debug)]
pub struct KpseWhich {
    all: bool,
    must_exist: bool,
    path: String,
    subdir: String,
    tex_inputs: Option<String>,
    tex_inputs_bak: Option<String>,
    encoding: String,
}

impl KpseWhich {
    pub fn new() -> KpseWhich {
        let tex_inputs_bak = match get_var("TEXINPUTS") {
            Ok(v) => Some(v),
            Err(_) => None,
        };
        KpseWhich {
            all: false,
            must_exist: false,
            path: String::new(),
            subdir: String::new(),
            tex_inputs: None,
            tex_inputs_bak,
            encoding: String::new(),
        }
    }
    pub fn var_value(var: &str, expand_brace: bool) -> Option<String> {
        let mut cmd = Command::new("kpsewhich");
        if expand_brace {
            cmd.arg("--var-brace-value");
        } else {
            cmd.arg("--var-value");
        }
        cmd.arg(var);
        let Ok(output) = cmd.output() else {
            return None;
        };
        if !output.status.success() {
            error!(target: "KpseWhich Exit Status", "{:?}", output.status);
        }
        let res = match Self::get_decoder(&output.stdout, "") {
            Ok(encoding) => encoding.decode(&output.stdout).0.to_string(),
            Err(_) => String::from_utf8_lossy(&output.stdout).to_string(),
        };
        if res.is_empty() {
            None
        } else {
            Some(res.trim_ascii().to_owned())
        }
    }
    pub fn from_matches(m: &ArgMatches) -> KpseWhich {
        let all = m.get_flag("all");
        let must_exist = m.get_flag("must-exist");
        let path = match m.get_one::<String>("path") {
            Some(p) => p.to_owned(),
            None => String::new(),
        };
        let subdir = match m.get_one::<String>("subdir") {
            Some(s) => s.to_owned(),
            None => String::new(),
        };
        let tex_inputs = if m.contains_id("texpath") {
            Some(format!("{};", m.get_one::<String>("texpath").unwrap()))
        } else {
            match m.get_one::<String>("texinputs") {
                Some(s) => Some(s.to_owned()),
                None => None,
            }
        };
        let tex_inputs_bak = match get_var("TEXINPUTS") {
            Ok(v) => Some(v),
            Err(_) => None,
        };
        let encoding = match m.get_one::<String>("encoding") {
            Some(v) => v.to_string(),
            None => "".to_string(),
        };
        KpseWhich {
            all,
            must_exist,
            path,
            subdir,
            tex_inputs,
            tex_inputs_bak,
            encoding,
        }
    }

    pub fn set_all(&mut self, all: bool) -> &mut Self {
        self.all = all;
        self
    }
    pub fn set_must_exist(&mut self, must_exist: bool) -> &mut Self {
        self.must_exist = must_exist;
        self
    }
    pub fn set_path(&mut self, path: &str) -> &mut Self {
        self.path = path.into();
        self
    }
    pub fn set_subdir(&mut self, subdir: &str) -> &mut Self {
        self.subdir = subdir.into();
        self
    }
    pub fn set_inputs(&mut self, input_dirs: Option<&str>) -> &mut Self {
        self.tex_inputs = input_dirs.map(|s| s.to_string());
        self
    }

    pub fn output<S: AsRef<OsStr>>(&self, file: S) -> io::Result<Vec<String>> {
        info!(target: "KpathWhich Arguments", "{:#?}", self);
        let mut cmd = Command::new("kpsewhich");
        if self.all {
            cmd.arg("-all");
        }
        if self.must_exist {
            cmd.arg("-must-exist");
        }
        if !self.path.is_empty() {
            // in linux, somehow path can not be expand, it would be better to use texinputs
            cmd.arg(&format!("-path={}", &self.path));
        }
        if !self.subdir.is_empty() {
            cmd.arg(&format!("-subdir={}", &self.subdir));
        }
        match &self.tex_inputs {
            Some(inputs) => set_var("TEXINPUTS", inputs),
            None => remove_var("TEXINPUTS"),
        }
        cmd.arg(file);
        info!(target: "KpseWhich RUN", "{:?}", &cmd.get_args().collect::<Vec<_>>());
        let output = cmd.output()?;
        if !output.status.success() {
            error!(target: "KpseWhich Exit Status", "{:?}", output.status);
        }
        let s = Self::get_decoder(&output.stdout, &self.encoding)?
            .decode(&output.stdout)
            .0;
        let mut res = vec![];
        s.lines().for_each(|l| res.push(l.to_string()));
        Ok(res)
    }

    #[cfg(target_os = "windows")]
    fn get_decoder(_: &Vec<u8>, prefer: &str) -> io::Result<&'static Encoding> {
        let codepage = if prefer.is_empty() {
            use winapi::um::winnls::GetACP;
            let s = unsafe { GetACP() };
            codepage::to_encoding(s as u16)
        } else {
            Encoding::for_label(prefer.as_bytes())
        };
        match codepage {
            Some(cp) => {
                info!(target: "Using Encoding", "{}", cp.name());
                Ok(cp)
            }
            None => Err(io::ErrorKind::Other.into()),
        }
    }
    #[cfg(not(target_os = "windows"))]
    fn get_decoder(_: &Vec<u8>, prefer: &str) -> io::Result<&'static Encoding> {
        if prefer.is_empty() {
            use encoding_rs::UTF_8;
            Ok(UTF_8)
        } else {
            Encoding::for_label(prefer.as_bytes()).ok_or(io::ErrorKind::Other.into())
        }
    }
}

impl Drop for KpseWhich {
    fn drop(&mut self) {
        match &self.tex_inputs_bak {
            Some(inputs) => set_var("TEXINPUTS", inputs),
            None => remove_var("TEXINPUTS"),
        }
    }
}
