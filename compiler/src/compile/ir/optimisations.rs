use std::{
    collections::HashMap,
    hash::RandomState,
    ops::{BitOr, BitOrAssign},
};

use petgraph::{Direction, prelude::DiGraphMap, visit::Dfs};

use crate::{
    CompileSettings,
    ast::SymbolId,
    compile::{
        ir::{SymbolSpans, TapIrFunction, TapIrFunctionBlockIter},
        symtab_visitor::SymTab,
    },
    reporting::Diagnostics,
};

mod block_shuffle;
mod constant_conditional;
mod constant_folding;
mod copy_propagation;
mod dead_global_elimination;
mod dead_store_elimination;
mod duplicate_loads;
mod empty_block;
mod empty_phi;
mod global_constant_propagation;
mod immediate_arithmetic;
mod inline;
mod unreferenced_blocks_in_phi;
mod unreferenced_function;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OptimisationResult {
    DidSomething,
    DidNothing,
}

impl BitOrAssign for OptimisationResult {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitOr for OptimisationResult {
    type Output = OptimisationResult;

    fn bitor(self, rhs: Self) -> Self::Output {
        if self == OptimisationResult::DidSomething || rhs == OptimisationResult::DidSomething {
            OptimisationResult::DidSomething
        } else {
            OptimisationResult::DidNothing
        }
    }
}

fn rename_all_variables(
    function: &mut TapIrFunction,
    renames: &HashMap<SymbolId, SymbolId>,
) -> OptimisationResult {
    if renames.is_empty() {
        return OptimisationResult::DidNothing;
    }

    let renames = reduce_renames(renames);

    let mut did_something = OptimisationResult::DidNothing;

    let mut dfs = TapIrFunctionBlockIter::new_dfs(function);

    while let Some(block) = dfs.next_mut(function) {
        for symbol in block.sources_mut() {
            if let Some(renamed_symbol) = renames.get(symbol) {
                *symbol = *renamed_symbol;
                did_something = OptimisationResult::DidSomething;
            }
        }

        for symbol in block.targets_mut() {
            if let Some(renamed_symbol) = renames.get(symbol) {
                *symbol = *renamed_symbol;
                did_something = OptimisationResult::DidSomething;
            }
        }
    }

    did_something
}

fn reduce_renames(renames: &HashMap<SymbolId, SymbolId>) -> HashMap<SymbolId, SymbolId> {
    let mut rename_graph: DiGraphMap<_, _, RandomState> = DiGraphMap::default();
    for (from, to) in renames {
        rename_graph.add_edge(*to, *from, ());
    }

    let mut roots = vec![];
    for node in rename_graph.nodes() {
        if rename_graph
            .edges_directed(node, Direction::Incoming)
            .count()
            == 0
        {
            roots.push(node);
        }
    }

    let mut result = HashMap::new();
    for root in roots {
        let mut dfs = Dfs::new(&rename_graph, root);
        while let Some(rename) = dfs.next(&rename_graph) {
            if root != rename {
                result.insert(rename, root);
            }
        }
    }

    result
}

pub fn optimise(
    program: &mut Vec<TapIrFunction>,
    symtab: &mut SymTab,
    symbol_spans: &SymbolSpans,
    diagnostics: &mut Diagnostics,
    settings: &CompileSettings,
) {
    if !settings.enable_optimisations {
        return;
    }

    const MAX_ITERATIONS: u32 = 1000;

    for _ in 0..MAX_ITERATIONS {
        let mut did_something = OptimisationResult::DidNothing;

        for (_, optimisation) in OPTIMISATIONS {
            did_something |= optimisation.optimise(program, symtab, symbol_spans, diagnostics);
        }

        if did_something == OptimisationResult::DidNothing {
            return;
        }
    }

    panic!(
        "Optimizer exceeded {MAX_ITERATIONS} iterations - likely infinite loop in optimization pass"
    );
}

trait Optimisation: Sync {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        symtab: &mut SymTab,
        symbol_spans: &SymbolSpans,
        diagnostics: &mut Diagnostics,
    ) -> OptimisationResult;
}

impl Optimisation for fn(&mut [TapIrFunction]) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        _symtab: &mut SymTab,
        _symbol_spans: &SymbolSpans,
        _diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        (self)(program)
    }
}

impl Optimisation for fn(&mut TapIrFunction) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        _symtab: &mut SymTab,
        _symbol_spans: &SymbolSpans,
        _diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        let mut result = OptimisationResult::DidNothing;

        for f in program.iter_mut() {
            result |= (self)(f);
        }

        result
    }
}

impl Optimisation for fn(&mut TapIrFunction, &mut SymTab) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        symtab: &mut SymTab,
        _symbol_spans: &SymbolSpans,
        _diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        let mut result = OptimisationResult::DidNothing;

        for f in program.iter_mut() {
            result |= (self)(f, symtab);
        }

        result
    }
}

impl Optimisation for fn(&mut Vec<TapIrFunction>) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        _symtab: &mut SymTab,
        _symbol_spans: &SymbolSpans,
        _diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        (self)(program)
    }
}

impl Optimisation for fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        symtab: &mut SymTab,
        _symbol_spans: &SymbolSpans,
        _diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        (self)(program, symtab)
    }
}

impl Optimisation
    for fn(&mut TapIrFunction, &mut SymTab, &SymbolSpans, &mut Diagnostics) -> OptimisationResult
{
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        symtab: &mut SymTab,
        symbol_spans: &SymbolSpans,
        diagnostics: &mut Diagnostics,
    ) -> OptimisationResult {
        let mut result = OptimisationResult::DidNothing;

        for f in program.iter_mut() {
            result |= (self)(f, symtab, symbol_spans, diagnostics);
        }

        result
    }
}

static OPTIMISATIONS: &[(&str, &'static dyn Optimisation)] = &[
    (
        "remove_unreferenced_functions",
        &(unreferenced_function::remove_unreferenced_functions
            as fn(&mut Vec<TapIrFunction>) -> OptimisationResult),
    ),
    (
        "propagate_readonly_globals",
        &(global_constant_propagation::propagate_readonly_globals
            as fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult),
    ),
    (
        "eliminate_dead_globals",
        &(dead_global_elimination::eliminate_dead_globals
            as fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult),
    ),
    (
        "empty_phi",
        &(empty_phi::remove_empty_phis as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "simplify_blocks",
        &(block_shuffle::simplify_blocks as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "remove_constant_conditionals",
        &(constant_conditional::remove_constant_conditionals
            as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "unreferenced_blocks_in_phi",
        &(unreferenced_blocks_in_phi::remove_unreferenced_blocks_in_phi
            as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "copy_propagation",
        &(copy_propagation::copy_propagation as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "dead_store_elimination",
        &(dead_store_elimination::remove_dead_stores
            as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "constant_folding",
        &(constant_folding::constant_folding
            as fn(
                &mut TapIrFunction,
                &mut SymTab,
                &SymbolSpans,
                &mut Diagnostics,
            ) -> OptimisationResult),
    ),
    (
        "immediate_arithmetic",
        &(immediate_arithmetic::immediate_arithmetic
            as fn(&mut TapIrFunction, &mut SymTab) -> OptimisationResult),
    ),
    (
        "duplicate_loads",
        &(duplicate_loads::duplicate_loads as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "empty_block",
        &(empty_block::remove_empty_blocks as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "inline_small_functions",
        &(inline::inline_small_functions
            as fn(&mut [TapIrFunction], &mut SymTab) -> OptimisationResult),
    ),
];

#[cfg(test)]
mod test {
    use std::{collections::HashSet, fmt::Write, fs};

    use insta::{assert_snapshot, glob};

    use crate::{
        CompileSettings,
        compile::{
            desugar,
            ir::{create_ir, make_ssa, pretty_print},
            loop_visitor::visit_loop_check,
            struct_visitor,
            symtab_visitor::SymTabVisitor,
            type_visitor::TypeVisitor,
        },
        grammar,
        lexer::Lexer,
        reporting::Diagnostics,
        tokens::FileId,
        types::StructRegistry,
    };

    use super::*;

    #[test]
    fn optimisation_snapshot_tests() {
        glob!("snapshot_tests", "optimisations/**/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let mut lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser
                .parse(file_id, &mut diagnostics, lexer.iter())
                .unwrap();

            desugar::run(&mut script);

            let compile_settings = CompileSettings {
                available_fields: None,
                enable_optimisations: true,
                enable_prelude: false,
                has_event_type: true,
            };

            // Register structs before symbol resolution
            let mut struct_registry = StructRegistry::default();
            let struct_names =
                struct_visitor::register_structs(&script, &mut struct_registry, &mut diagnostics);
            struct_visitor::resolve_struct_fields(
                &script,
                &mut struct_registry,
                &struct_names,
                &mut diagnostics,
            );
            struct_visitor::resolve_all_types(&mut script, &struct_names, &mut diagnostics);

            let mut symtab_visitor = SymTabVisitor::new(
                &compile_settings,
                &mut script,
                &struct_registry,
                &mut diagnostics,
            );
            let mut type_visitor = TypeVisitor::new(
                &script.functions,
                &script.extern_functions,
                &script.builtin_functions,
                &struct_registry,
                symtab_visitor.get_symtab(),
            );

            for function in &mut script.functions {
                visit_loop_check(function, &mut diagnostics);
                symtab_visitor.visit_function(function, &mut diagnostics);
                type_visitor.visit_function(
                    function,
                    symtab_visitor.get_symtab(),
                    &mut diagnostics,
                );
            }

            assert!(
                !diagnostics.has_any(),
                "{}",
                diagnostics.pretty_string(false)
            );

            let mut symtab = symtab_visitor.into_symtab();

            let (mut irs, spans_vec): (Vec<_>, Vec<_>) = script
                .functions
                .iter()
                .map(|f| create_ir(f, &mut symtab, &struct_registry))
                .unzip();

            let mut symbol_spans = SymbolSpans::new();
            for spans in spans_vec {
                symbol_spans.extend(&spans);
            }

            for f in &mut irs {
                make_ssa(f, &mut symtab, &mut symbol_spans);
            }

            let mut output = String::new();

            writeln!(&mut output, "----------- before -------------").unwrap();

            for ir in &irs {
                pretty_print::pretty_print_tapir_function(ir, &symtab, &mut output).unwrap();
            }

            let (enabled_optimisations, _) = input.split_once('\n').unwrap();
            let (_, enabled_optimisations) = enabled_optimisations
                .split_once("# ")
                .unwrap_or_else(|| panic!("Missing optimisation comment in {}", path.display()));

            let enabled_optimisations = enabled_optimisations.split(", ").collect::<HashSet<_>>();
            let enable_all_optimisations = enabled_optimisations.contains("all");

            loop {
                let mut did_something = OptimisationResult::DidNothing;

                for (name, optimisation) in OPTIMISATIONS {
                    if enable_all_optimisations || enabled_optimisations.contains(name) {
                        let this_did_something = optimisation.optimise(
                            &mut irs,
                            &mut symtab,
                            &symbol_spans,
                            &mut diagnostics,
                        );

                        if this_did_something == OptimisationResult::DidSomething {
                            writeln!(&mut output, "\n----------- {name} -------------").unwrap();
                            for ir in &irs {
                                pretty_print::pretty_print_tapir_function(ir, &symtab, &mut output)
                                    .unwrap();
                            }
                        }

                        did_something |= this_did_something;
                    }
                }

                if did_something == OptimisationResult::DidNothing {
                    break;
                }
            }

            // Include any warnings in the snapshot output
            if diagnostics.has_any() {
                writeln!(&mut output, "\n----------- warnings -------------").unwrap();
                output.push_str(&diagnostics.pretty_string(false));
            }

            assert_snapshot!(output);
        });
    }

    #[test]
    fn reduce_renames_simple_case() {
        let mut renames = HashMap::new();
        renames.insert(SymbolId(5), SymbolId(7));
        renames.insert(SymbolId(6), SymbolId(19));

        assert_eq!(reduce_renames(&renames), renames);
    }

    #[test]
    fn reduce_renames_chained() {
        let mut renames = HashMap::new();
        renames.insert(SymbolId(5), SymbolId(7));
        renames.insert(SymbolId(7), SymbolId(12));
        renames.insert(SymbolId(6), SymbolId(19));

        let mut expected = HashMap::new();
        expected.insert(SymbolId(5), SymbolId(12));
        expected.insert(SymbolId(7), SymbolId(12));
        expected.insert(SymbolId(6), SymbolId(19));

        assert_eq!(reduce_renames(&renames), expected);
    }

    /// Regression test: optimizer should not hang on infinite loops.
    /// See regalloc_loop.tapir - this script contains an infinite loop
    /// which previously caused the optimizer to run forever.
    #[test]
    fn optimizer_terminates_on_infinite_loop() {
        use std::path::Path;

        let source = r#"
var x = 3;
loop {
    var y = x;
    if true {}
}
"#;

        let settings = CompileSettings {
            available_fields: None,
            enable_optimisations: true,
            enable_prelude: false,
            has_event_type: true,
        };

        // This should complete, not hang
        let _ = crate::compile(Path::new("test.tapir"), source, settings);
    }
}
