use std::path::Path;

use compiler::{CompileSettings, compile};
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};

/// Test scripts bundled with the compiler, sorted roughly by complexity.
const TEST_SCRIPTS: &[(&str, &str)] = &[
    // Simple: single expressions, basic statements
    (
        "large_value_push32",
        include_str!("../src/snapshot_tests/compiler/large_value_push32.tapir"),
    ),
    (
        "variables",
        include_str!("../src/snapshot_tests/compiler/variables.tapir"),
    ),
    (
        "if_basic",
        include_str!("../src/snapshot_tests/compiler/if_basic.tapir"),
    ),
    (
        "if_local_variable",
        include_str!("../src/snapshot_tests/compiler/if_local_variable.tapir"),
    ),
    (
        "globals",
        include_str!("../src/snapshot_tests/compiler/globals.tapir"),
    ),
    (
        "wait",
        include_str!("../src/snapshot_tests/compiler/wait.tapir"),
    ),
    // Medium: function calls, loops
    (
        "loop_break1",
        include_str!("../src/snapshot_tests/compiler/loop_break1.tapir"),
    ),
    (
        "really_basic_call",
        include_str!("../src/snapshot_tests/compiler/really_basic_call.tapir"),
    ),
    (
        "fn_call_one_return",
        include_str!("../src/snapshot_tests/compiler/fn_call_one_return.tapir"),
    ),
    (
        "fn_call_many_returns",
        include_str!("../src/snapshot_tests/compiler/fn_call_many_returns.tapir"),
    ),
    (
        "properties",
        include_str!("../src/snapshot_tests/compiler/properties.tapir"),
    ),
    (
        "then",
        include_str!("../src/snapshot_tests/compiler/then.tapir"),
    ),
    // Extern calls
    (
        "extern_call_basic",
        include_str!("../src/snapshot_tests/compiler/extern_call_basic.tapir"),
    ),
    (
        "extern_call_with_args",
        include_str!("../src/snapshot_tests/compiler/extern_call_with_args.tapir"),
    ),
    (
        "extern_call_in_loop",
        include_str!("../src/snapshot_tests/compiler/extern_call_in_loop.tapir"),
    ),
    (
        "extern_call_mixed_with_fn",
        include_str!("../src/snapshot_tests/compiler/extern_call_mixed_with_fn.tapir"),
    ),
    (
        "extern_call_multiple_externs",
        include_str!("../src/snapshot_tests/compiler/extern_call_multiple_externs.tapir"),
    ),
    (
        "extern_call_statement",
        include_str!("../src/snapshot_tests/compiler/extern_call_statement.tapir"),
    ),
    // Multi-assign
    (
        "multi_assign_paired",
        include_str!("../src/snapshot_tests/compiler/multi_assign_paired.tapir"),
    ),
    (
        "multi_assign_swap",
        include_str!("../src/snapshot_tests/compiler/multi_assign_swap.tapir"),
    ),
    (
        "multi_assign_use_values",
        include_str!("../src/snapshot_tests/compiler/multi_assign_use_values.tapir"),
    ),
    (
        "multi_assign_extern",
        include_str!("../src/snapshot_tests/compiler/multi_assign_extern.tapir"),
    ),
    (
        "multi_assign_function",
        include_str!("../src/snapshot_tests/compiler/multi_assign_function.tapir"),
    ),
    (
        "multi_assign_with_prop",
        include_str!("../src/snapshot_tests/compiler/multi_assign_with_prop.tapir"),
    ),
    // Complex: recursion, spawn, register allocation
    (
        "factorial_recursive",
        include_str!("../src/snapshot_tests/compiler/factorial_recursive.tapir"),
    ),
    (
        "regalloc_loop",
        include_str!("../src/snapshot_tests/compiler/regalloc_loop.tapir"),
    ),
    (
        "spawn",
        include_str!("../src/snapshot_tests/compiler/spawn.tapir"),
    ),
    (
        "spawn2",
        include_str!("../src/snapshot_tests/compiler/spawn2.tapir"),
    ),
    (
        "fix_mul_div_promotion",
        include_str!("../src/snapshot_tests/compiler/fix_mul_div_promotion.tapir"),
    ),
    (
        "optimise_division_by_subtraction",
        include_str!("../src/snapshot_tests/compiler/optimise_division_by_subtraction.tapir"),
    ),
];

fn settings(enable_optimisations: bool) -> CompileSettings {
    CompileSettings {
        available_fields: None,
        enable_optimisations,
        enable_prelude: true,
        has_event_type: true,
    }
}

/// Benchmark each test script individually with optimizations enabled.
fn bench_individual_scripts(c: &mut Criterion) {
    let mut group = c.benchmark_group("compile/individual");

    for (name, source) in TEST_SCRIPTS {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("optimized", name), source, |b, source| {
            b.iter(|| compile(Path::new("bench.tapir"), source, settings(true)).unwrap());
        });
    }

    group.finish();
}

/// Benchmark optimized vs unoptimized compilation.
fn bench_optimization_impact(c: &mut Criterion) {
    let mut group = c.benchmark_group("compile/optimization");

    // Pick a few representative scripts
    let representative = [
        "factorial_recursive",
        "spawn2",
        "optimise_division_by_subtraction",
    ];

    for name in representative {
        let source = TEST_SCRIPTS
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, s)| *s)
            .unwrap();

        group.bench_with_input(BenchmarkId::new("optimized", name), &source, |b, source| {
            b.iter(|| compile(Path::new("bench.tapir"), source, settings(true)).unwrap());
        });

        group.bench_with_input(
            BenchmarkId::new("unoptimized", name),
            &source,
            |b, source| {
                b.iter(|| compile(Path::new("bench.tapir"), source, settings(false)).unwrap());
            },
        );
    }

    group.finish();
}

/// Benchmark total compilation of all scripts (simulates compiling a project).
fn bench_aggregate(c: &mut Criterion) {
    let total_bytes: usize = TEST_SCRIPTS.iter().map(|(_, s)| s.len()).sum();

    let mut group = c.benchmark_group("compile/aggregate");
    group.throughput(Throughput::Bytes(total_bytes as u64));

    group.bench_function("all_scripts_optimized", |b| {
        b.iter(|| {
            for (name, source) in TEST_SCRIPTS {
                compile(Path::new(name), source, settings(true)).unwrap();
            }
        });
    });

    group.bench_function("all_scripts_unoptimized", |b| {
        b.iter(|| {
            for (name, source) in TEST_SCRIPTS {
                compile(Path::new(name), source, settings(false)).unwrap();
            }
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_individual_scripts,
    bench_optimization_impact,
    bench_aggregate
);
criterion_main!(benches);
