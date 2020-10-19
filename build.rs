use std::path::Component::CurDir;
use std::path::Path;
use std::env::{current_dir, set_current_dir};
use std::fs::{read_dir, File};
use std::io::Read;
use std::process::{Command, Child};

const JAVA_SRC_DIR:  &'static str = "java";
fn main() {
    //println!("cargo:rerun-if-changed={}", JAVA_SRC_DIR);

    let dir = current_dir().unwrap().join(JAVA_SRC_DIR);
    let paths = read_dir(&dir).unwrap();
    let out_dir_string = std::env::var("OUT_DIR").unwrap();
    println!("{}", out_dir_string);
    let out_dir = Path::new(out_dir_string.as_str());
    set_current_dir(out_dir);
    let mut processes: Vec<Child> = vec![];
    for path in paths {
        let path = path.unwrap();
        let process = Command::new("javac").args(&[path.path().to_str().unwrap() ,"-d", out_dir.to_str().unwrap()]).spawn().unwrap();
        processes.push(process)
    }
    for mut x in processes {
        x.wait().unwrap();
    }
}