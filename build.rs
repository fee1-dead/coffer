use std::env::current_dir;
use std::fs::{read_dir, create_dir, remove_dir_all};
use std::process::{Command, Child};
use std::path::Path;

const JAVA_SRC_DIR:  &'static str = "java";
const OUT_DIR: &'static str = "out";
fn main() {
    println!("cargo:rustc-env=JAVA_OUTPUT_DIR={}", OUT_DIR);
    println!("cargo:rerun-if-changed={}", OUT_DIR);
    let out_path = Path::new(OUT_DIR);
    if out_path.exists() {
        remove_dir_all(out_path).expect("Clean Out Dir");
    }
    create_dir(out_path).expect("Create Out Directory");
    let dir = current_dir().unwrap().join(JAVA_SRC_DIR);
    let paths = read_dir(&dir).unwrap();
    let mut processes: Vec<Child> = vec![];
    for path in paths {
        let path = path.unwrap();
        let process = Command::new("javac").args(&[path.path().to_str().unwrap() ,"-d", OUT_DIR]).spawn().unwrap();
        processes.push(process)
    }
    for mut x in processes {
        assert!(x.wait().unwrap().success());
    }
}