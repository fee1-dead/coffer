use criterion::{Criterion, criterion_group, criterion_main, BatchSize, BenchmarkId, Throughput};
use coffer::constants::insn::*;
use std::io::{Cursor, Read};
use coffer::insn::InstructionRead;

use coffer::index::JClassIdx;
use std::thread::spawn;
use zip::ZipArchive;
use std::collections::HashSet;



fn bench_op(c: &mut Criterion) {
    let buf = [TABLESWITCH, 0, 0, 0, 12, 0, 0, 0, 10, 0, 0, 0, 12, 4, 6, 7, 4, 5, 6, 4, 7, 1, 35, 76, 34];
    c.bench_function("parse_tableswitch", |b| {
        b.iter_batched(|| {
            Cursor::new(buf)
        }, |mut c| c.read_insn(|_| {}), BatchSize::SmallInput)
    });

    let buf = [ATHROW];

    c.bench_function("parse_no_ext_bytes", |b| {
        b.iter_batched(|| {
            Cursor::new(buf)
        }, |mut c| c.read_insn(|_| {}), BatchSize::SmallInput)
    });
}

fn bench_jidx(c: &mut Criterion) -> coffer::error::Result<()> {
    // See NOTICE
    let urls = [
        "https://repo1.maven.org/maven2/com/google/guava/guava/30.0-jre/guava-30.0-jre.jar",
        "https://repo1.maven.org/maven2/com/squareup/okhttp3/okhttp/4.10.0-RC1/okhttp-4.10.0-RC1.jar",
        "https://repo1.maven.org/maven2/org/apache/spark/spark-core_2.11/2.4.7/spark-core_2.11-2.4.7.jar",
        "https://repo1.maven.org/maven2/com/google/zxing/core/3.4.1/core-3.4.1.jar",
        "https://repo1.maven.org/maven2/com/google/inject/guice/5.0.0-BETA-1/guice-5.0.0-BETA-1.jar",
        "https://repo1.maven.org/maven2/junit/junit/4.13.1/junit-4.13.1.jar",
        "https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-compiler/1.4.20-M1/kotlin-compiler-1.4.20-M1.jar",
        "https://repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.13.3/scala-compiler-2.13.3.jar"
    ];
    let classes: Vec<(String, Vec<u8>)> = class_sample::get_sample_name_bytes(100);
    println!("{} classes", classes.len());
    let mut group = c.benchmark_group("bench_jidx");
    for (name, bytes) in classes {
        group.throughput(Throughput::Bytes(bytes.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &bytes, |c, buf| {
            c.iter_batched(|| Cursor::new(buf), |mut c| JClassIdx::try_from(&mut c), BatchSize::SmallInput)
        });
    }
    /*for d in read_dir("out")? {
        let d = d?;
        let path = d.path();
        let path_ref = path.as_path();
        if !path.is_file() { continue; }
        if let Some(ext) = path.extension() {
            if ext == "class" {
                let mut file = File::open(path_ref)?;
                let meta = file.metadata()?;
                let mut bytes = vec![];
                file.read_to_end(&mut bytes);
                let byte_len = meta.len();
                let filename = path_ref.file_name().unwrap().to_str().unwrap();
                group.throughput(Throughput::Bytes(byte_len));
                group.bench_with_input(BenchmarkId::from_parameter(filename), &bytes, |c, buf| {
                    c.iter_batched(|| {
                        Cursor::new(buf)
                    }, |mut c| JClassIdx::try_from(&mut c), BatchSize::SmallInput)
                });
            }
        }
    }*/

    Ok(())
}

criterion_group!(benches, bench_op, bench_jidx);
criterion_main!(benches);