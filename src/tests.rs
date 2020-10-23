use std::path::Path;
use std::fs::File;
use crate::decoder::Decoder;
use crate::index::JClassIdx;
use std::convert::TryFrom;

#[test]
fn it_works() {
    assert_eq!(2 + 2, 4);
}

#[test]
fn test_parse_class_index() {
    let out_dir: &'static str = env!("JAVA_OUTPUT_DIR");
    test_folder(out_dir);
}

fn test_folder<P>(path: P) where P: AsRef<Path> {
    for dir_entry in path.as_ref().read_dir().expect("out directory") {
        let path = dir_entry.expect(&*format!("dir entry under path {}", path.as_ref().to_str().unwrap_or("INVALID_PATH"))).path();
        if path.is_dir() {
            test_folder(path);
        } else if path.extension() == Some("class".as_ref()) {
            test_parse_class(path);
        }
    }
}

fn test_parse_class<P>(path: P) where P: AsRef<Path> {
    let path = path.as_ref();
    let mut file = File::open(path).expect("Open file");
    let mut decoder = Decoder::new(&mut file);
    let jclass = JClassIdx::try_from(&mut decoder).expect(&*format!("Parsing Class {}", path.to_str().unwrap_or("INVALID_PATH")));
    println!("JClass {:#?}", jclass);
}