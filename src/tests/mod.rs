/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
use std::path::Path;
use std::fs::File;
use crate::index::JClassIdx;
use std::io::BufReader;

mod mutf8;
mod insn;

#[test]
fn test_parse_class_index() {
    //let out_dir: &'static str = env!("JAVA_OUTPUT_DIR");
    //test_folder(out_dir);
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
    let file = File::open(path).expect("Open file");
    let mut buf = BufReader::new(file);
    let _jclass = JClassIdx::try_from(&mut buf).expect(&*format!("Parsing Class {}", path.to_str().unwrap_or("INVALID_PATH")));
}
