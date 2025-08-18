/* kpathsea.rs
Copyright (C) 2024-2025, Wenjian Chern.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. */

use clap::ArgMatches;
use encoding_rs::Encoding;
use log::{error, info};
use std::env::{remove_var, set_var, var as get_var};
use std::ffi::{OsStr, OsString};
use std::io;
use std::process::Command;

#[derive(Debug)]
pub struct KpseWhich {
    pub exe_path: Option<OsString>,
    pub all: bool,
    pub must_exist: bool,
    path: String,
    subdir: String,
    tex_inputs: Option<String>,
    tex_inputs_bak: Option<String>,
    pub encoding: String,
}

impl KpseWhich {
    pub fn new() -> KpseWhich {
        let tex_inputs_bak = match get_var("TEXINPUTS") {
            Ok(v) => Some(v),
            Err(_) => None,
        };
        KpseWhich {
            exe_path: None,
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
        KpseWhich::var_value_with_exe(var, "kpsewhich", expand_brace)
    }
    pub fn var_value_with_exe(
        var: &str,
        exe: impl AsRef<OsStr>,
        expand_brace: bool,
    ) -> Option<String> {
        let mut cmd = Command::new(exe.as_ref());
        info!(target: "KpseWhich command", "{}", exe.as_ref().display());
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
        let res = match get_decoder(&output.stdout, "") {
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
            exe_path: None,
            all,
            must_exist,
            path,
            subdir,
            tex_inputs,
            tex_inputs_bak,
            encoding,
        }
    }

    pub fn set_exe(&mut self, exe: impl AsRef<OsStr>) -> &mut Self {
        self.exe_path = Some(exe.as_ref().to_os_string());
        self
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
        let kp_exe = self
            .exe_path
            .as_ref()
            .map(|v| v.as_os_str())
            .unwrap_or(OsStr::new("kpsewhich"));
        info!(target: "KpseWhich command", "{}", kp_exe.display());
        info!(target: "KpseWhich Arguments", "{:#?}", self);
        let mut cmd = Command::new(kp_exe);
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
        let s = get_decoder(&output.stdout, &self.encoding)?
            .decode(&output.stdout)
            .0;
        let mut res = vec![];
        s.lines().for_each(|l| res.push(l.to_string()));
        Ok(res)
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

#[cfg(target_os = "windows")]
pub(crate) fn get_decoder(
    _: &Vec<u8>,
    prefer: &str,
) -> io::Result<&'static Encoding> {
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
pub(crate) fn get_decoder(
    _: &Vec<u8>,
    prefer: &str,
) -> io::Result<&'static Encoding> {
    if prefer.is_empty() {
        use encoding_rs::UTF_8;
        Ok(UTF_8)
    } else {
        Encoding::for_label(prefer.as_bytes())
            .ok_or(io::ErrorKind::Other.into())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use lazy_static::lazy_static;
    use which::which;

    use super::*;

    lazy_static! {
        static ref KPSEWHICH_EXE_FILE: PathBuf = get_var("KPSEWHICH_EXE_FILE")
            .map_or_else(
                |_| which("kpsewhich").unwrap_or_default(),
                |p| PathBuf::from(p)
            );
    }

    #[ignore]
    #[test]
    fn var_value() {
        let kpse_exe = KPSEWHICH_EXE_FILE.as_path();
        assert!(!kpse_exe.as_os_str().is_empty(), "Cannot find kpsewhich");
        let var = "TEXMFHOME";
        let res = KpseWhich::var_value_with_exe(var, kpse_exe, false);
        assert!(res.is_some(), "Cannot find variable 'TEXMFHOME'");
        let res = {
            let res = res.unwrap();
            res.trim_ascii().to_string()
        };
        assert!(
            res.len() > 0,
            "Invalid 'TEXMFHOME' value, maybe I did not read it properly"
        );
        println!("TEXMFHOME: '{}'", res);
    }

    #[ignore]
    #[test]
    fn get_value() {
        let kpse_exe = KPSEWHICH_EXE_FILE.as_path();
        let mut kpse = KpseWhich::new();
        kpse.set_exe(kpse_exe)
            .set_all(true)
            .set_inputs(Some(r"./;./target//"));
        let res = kpse.output("libtexhigh.rlib");
        println!("{:#?}", res);

        kpse.set_inputs(Some("./;./tests/Uıςօ۹Ⓔ文🤔/"));
        let res = kpse.output("file.txt");
        println!("{:#?}", res);
    }
}
