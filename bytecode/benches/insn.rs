use criterion::{Criterion, criterion_group, criterion_main, BatchSize, BenchmarkId, Throughput};
use coffer::constants::insn::*;
use std::io::Cursor;
use coffer::insn::InstructionRead;
use coffer::index::JClassIdx;

fn bench_op(c: &mut Criterion) {
    let buf = [TABLESWITCH, 0, 0, 0, 12, 0, 0, 0, 10, 0, 0, 0, 12, 4, 6, 7, 4, 5, 6, 4, 7, 1, 35, 76, 34];
    c.bench_function("parse_tableswitch", |b| {
        b.iter_batched(|| {
            Cursor::new(buf)
        }, |mut c| c.read_insn(|_| {Ok(())}), BatchSize::SmallInput)
    });

    let buf = [ATHROW];

    c.bench_function("parse_no_ext_bytes", |b| {
        b.iter_batched(|| {
            Cursor::new(buf)
        }, |mut c| c.read_insn(|_| {Ok(())}), BatchSize::SmallInput)
    });
}

fn bench_jidx(c: &mut Criterion) {
    let classes: Vec<(String, Vec<u8>)> = class_sample::get_sample_name_bytes(1024 * 10);
    println!("{} classes", classes.len());
    let mut group = c.benchmark_group("bench_jidx");
    for (name, bytes) in classes {
        group.throughput(Throughput::Bytes(bytes.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &bytes, |c, buf| {
            c.iter_batched(|| Cursor::new(buf), |mut c| JClassIdx::try_from(&mut c), BatchSize::SmallInput)
        });
    }
}

criterion_group!(benches, bench_op, bench_jidx);
criterion_main!(benches);