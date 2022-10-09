use std::fs::File;
use std::io::BufWriter;
use std::process::{Command, Stdio};

use tempfile::{tempdir, TempDir};
use wtf_8::w;

use crate::clazz::Class;
use crate::code::GetOrPut::Get;
use crate::code::Instruction::*;
use crate::code::MemberType::Static;
use crate::code::{Code, MemberType};
use crate::flags::{ClassFlags, MethodFlags};
use crate::loadable::Constant;
use crate::member::{MemberRef, Method, MethodAttribute};
use crate::prelude::{JavaVersion, Type, *};

/// skip if java is not installed.
pub fn should_skip() -> bool {
    if let Ok(status) = Command::new("java")
        .arg("--version")
        .stdout(Stdio::null())
        .status()
    {
        if status.success() {
            return false;
        }
    }

    true
}

struct Execution(TempDir, Command);

impl Execution {
    fn case(
        &mut self,
        success: bool,
        stdin: impl AsRef<[u8]>,
        stdout: impl AsRef<[u8]>,
        stderr: impl AsRef<[u8]>,
    ) -> Result<&mut Self> {
        let mut child = self.1.spawn()?;
        child
            .stdin
            .take()
            .expect("Expected stdin")
            .write_all(stdin.as_ref())?;
        let out = child.wait_with_output()?;
        assert_eq!(out.status.success(), success);
        assert_eq!(out.stderr, stderr.as_ref());
        assert_eq!(out.stdout, stdout.as_ref());
        Ok(self)
    }
}

fn bake(code: Code) -> crate::Result<Execution> {
    let cls = Class {
        version: JavaVersion::J8,
        access: ClassFlags::ACC_FINAL,
        name: w!("Test").into(),
        super_name: Some(w!("java/lang/Object").into()),
        interfaces: vec![],
        fields: vec![],
        methods: vec![Method {
            access: MethodFlags::ACC_STATIC | MethodFlags::ACC_FINAL | MethodFlags::ACC_PUBLIC,
            name: w!("main").into(),
            descriptor: Type::method(
                [Type::array(1, Type::reference(w!("java/lang/String")))],
                None,
            ),
            attributes: vec![MethodAttribute::Code(code)],
        }],
        attributes: vec![],
    };
    let dir = tempdir()?;
    let class = dir.path().join("Test.class");
    let mut wtr = BufWriter::new(File::create(class)?);

    cls.write_to(&mut wtr)?;
    let mut cmd = Command::new("java");
    cmd.current_dir(dir.path());
    cmd.arg("Test");
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    Ok(Execution(dir, cmd)) // Pass the dir back so it does not get deleted
}

#[test]
fn execute_helloworld() -> crate::Result<()> {
    if should_skip() {
        return Ok(());
    }
    let mut exec = bake(Code {
        max_locals: 1,
        max_stack: 2,
        code: vec![
            Field(
                Get,
                Static,
                MemberRef {
                    owner: w!("java/lang/System").into(),
                    name: w!("out").into(),
                    descriptor: Type::reference(w!("java/io/PrintStream")),
                    itfs: false,
                }
                .into(),
            ),
            Push(Constant::string(w!("Hello, World!")).into()),
            InvokeExact(
                MemberType::Virtual,
                MemberRef {
                    owner: w!("java/io/PrintStream").into(),
                    name: w!("print").into(),
                    descriptor: Type::method([Type::reference(w!("java/lang/String"))], None),
                    itfs: false,
                }
                .into(),
            ),
            Return(None),
        ],
        attrs: Default::default(),
        catches: Default::default(),
    })?;
    exec.case(true, [], "Hello, World!", [])?;
    Ok(())
}

/*fn execute_tableswitch() -> crate::Result<()> {
    let label1 = Lbl(0);
    let label2 = Lbl(1);
    let mut exec = bake(Code {
        max_locals: 1,
        max_stack: 2,
        code: vec![
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
    });
    Ok(())
}*/
