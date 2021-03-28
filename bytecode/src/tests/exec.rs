/*
 *     This file is part of Coffer.
 *
 *     Coffer is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     Coffer is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
 */
use crate::Class;
use crate::full::{Code, MemberType};
use crate::full::Instruction::*;
use crate::loadable::Constant;
use crate::full::GetOrPut::Get;
use crate::full::MemberType::Static;
use crate::member::{MemberRef, Method, MethodAttribute};
use crate::prelude::{Type, JavaVersion};
use crate::flags::{ClassFlags, MethodFlags};
use crate::prelude::*;
use tempfile::{tempdir, TempDir};
use std::process::{Command, Child, Stdio};
use std::io::BufWriter;
use std::fs::File;

macro_rules! ignored_tests {
    ($($item: item)*) => {
        $(
        #[ignore]
        #[test]
        $item
        )*
    };
}

fn execute(code: Code) -> crate::Result<(TempDir, Child)> {
    let cls = Class {
        version: JavaVersion::J8,
        access: ClassFlags::ACC_FINAL,
        name: "Test".into(),
        super_name: Some("java/lang/Object".into()),
        interfaces: vec![],
        fields: vec![],
        methods: vec![Method {
            access: MethodFlags::ACC_STATIC | MethodFlags::ACC_FINAL | MethodFlags::ACC_PUBLIC,
            name: "main".into(),
            descriptor: Type::method([Type::array(1, Type::reference("java/lang/String"))], None),
            attributes: vec![MethodAttribute::Code(code)]
        }],
        attributes: vec![]
    };
    let dir = tempdir()?;
    let class = dir.path().join("Test.class");
    let mut wtr = BufWriter::new(File::create(class)?);
    let mut wtr2 = BufWriter::new(File::create("Test.class")?);

    cls.write_to(&mut wtr)?;
    cls.write_to(&mut wtr2)?;
    let mut cmd = Command::new("java");
    cmd.current_dir(dir.path());
    cmd.arg("Test");
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    Ok((dir, cmd.spawn()?)) // Pass the dir back so it does not get deleted
}

// These tests are ignored by default.
// To enable them, run cargo test -- --include-ignored.
ignored_tests! {
    fn execute_helloworld() -> crate::Result<()> {
        let (_dir, child) = execute(Code {
            max_locals: 1,
            max_stack: 2,
            code: vec![
                Field(Get, Static, MemberRef {
                    owner: "java/lang/System".into(),
                    name: "out".into(),
                    descriptor: Type::reference("java/io/PrintStream"),
                    itfs: false
                }.into()),
                Push(Constant::string("Hello, World!").into()),
                InvokeExact(MemberType::Virtual, MemberRef {
                    owner: "java/io/PrintStream".into(),
                    name: "println".into(),
                    descriptor: Type::method([Type::reference("java/lang/String")], None),
                    itfs: false
                }.into()),
                Return(None)
            ],
            attrs: Default::default(),
            catches: Default::default()
        })?;
        let out = child.wait_with_output()?;
        assert!(out.status.success());
        assert_eq!(&out.stderr, &[]);
        assert_eq!(&out.stdout, b"Hello, World!\n");
        Ok(())
    }
}

