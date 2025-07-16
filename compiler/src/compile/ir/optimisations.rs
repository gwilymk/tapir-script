use std::{
    collections::HashMap,
    ops::{BitOr, BitOrAssign},
};

use crate::{
    CompileSettings,
    ast::SymbolId,
    compile::{
        ir::{TapIrFunction, TapIrFunctionBlockIter},
        symtab_visitor::SymTab,
    },
};

mod constant_conditional;
mod constant_folding;
mod copy_propagation;
mod dead_store_elimination;
mod duplicate_loads;
mod empty_block;
mod empty_phi;
mod unreferenced_blocks_in_phi;

#[derive(Clone, Copy, PartialEq, Eq)]
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

pub fn optimise(program: &mut Vec<TapIrFunction>, symtab: &mut SymTab, settings: &CompileSettings) {
    if !settings.enable_optimisations {
        return;
    }

    loop {
        let mut did_something = OptimisationResult::DidNothing;

        for (_, optimisation) in OPTIMISATIONS {
            did_something |= optimisation.optimise(program, symtab);
        }

        if did_something == OptimisationResult::DidNothing {
            return;
        }
    }
}

trait Optimisation: Sync {
    fn optimise(&self, program: &mut Vec<TapIrFunction>, symtab: &mut SymTab)
    -> OptimisationResult;
}

impl Optimisation for fn(program: &mut [TapIrFunction]) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        _symtab: &mut SymTab,
    ) -> OptimisationResult {
        (self)(program)
    }
}

impl Optimisation for fn(f: &mut TapIrFunction) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        _symtab: &mut SymTab,
    ) -> OptimisationResult {
        let mut result = OptimisationResult::DidNothing;

        for f in program.iter_mut() {
            result |= (self)(f);
        }

        result
    }
}

impl Optimisation for fn(f: &mut TapIrFunction, symtab: &mut SymTab) -> OptimisationResult {
    fn optimise(
        &self,
        program: &mut Vec<TapIrFunction>,
        symtab: &mut SymTab,
    ) -> OptimisationResult {
        let mut result = OptimisationResult::DidNothing;

        for f in program.iter_mut() {
            result |= (self)(f, symtab);
        }

        result
    }
}

static OPTIMISATIONS: &[(&str, &'static dyn Optimisation)] = &[
    (
        "empty_phi",
        &(empty_phi::remove_empty_phis as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "empty_block",
        &(empty_block::remove_empty_blocks as fn(&mut TapIrFunction) -> OptimisationResult),
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
            as fn(&mut TapIrFunction, &mut SymTab) -> OptimisationResult),
    ),
    (
        "duplicate_loads",
        &(duplicate_loads::duplicate_loads as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
    (
        "remove_constant_conditionals",
        &(constant_conditional::remove_constant_conditionals
            as fn(&mut TapIrFunction) -> OptimisationResult),
    ),
];

#[cfg(test)]
mod test {
    use std::{collections::HashSet, fmt::Write, fs};

    use insta::{assert_snapshot, glob};

    use crate::{
        CompileSettings, Property, Type,
        compile::{
            ir::{create_ir, make_ssa, pretty_print},
            loop_visitor::visit_loop_check,
            symtab_visitor::SymTabVisitor,
            type_visitor::TypeVisitor,
        },
        grammar,
        lexer::Lexer,
        reporting::Diagnostics,
        tokens::FileId,
    };

    use super::*;

    #[test]
    fn optimisation_snapshot_tests() {
        glob!("snapshot_tests", "optimisations/**/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let parser = grammar::ScriptParser::new();
            let file_id = FileId::new(0);

            let mut diagnostics = Diagnostics::new(file_id, path.file_name().unwrap(), &input);

            let mut script = parser.parse(file_id, &mut diagnostics, lexer).unwrap();

            let compile_settings = CompileSettings {
                properties: vec![
                    Property {
                        ty: Type::Int,
                        index: 0,
                        name: "int_prop".to_string(),
                    },
                    Property {
                        ty: Type::Fix,
                        index: 1,
                        name: "fix_prop".to_string(),
                    },
                ],
                enable_optimisations: true,
            };

            let mut symtab_visitor =
                SymTabVisitor::new(&compile_settings, &mut script.functions, &mut diagnostics);
            let mut type_visitor = TypeVisitor::new(&compile_settings, &script.functions);

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

            let mut irs = script
                .functions
                .iter()
                .map(|f| create_ir(f, &mut symtab))
                .collect::<Vec<_>>();

            for f in &mut irs {
                make_ssa(f, &mut symtab);
            }

            let mut output = String::new();

            writeln!(&mut output, "----------- before -------------").unwrap();

            for ir in &irs {
                pretty_print::pretty_print_tapir_function(ir, &symtab, &mut output).unwrap();
            }

            writeln!(&mut output, "\n----------- optimised -------------").unwrap();

            let (enabled_optimisations, _) = input.split_once('\n').unwrap();
            let (_, enabled_optimisations) = enabled_optimisations.split_once("# ").unwrap();

            let enabled_optimisations = enabled_optimisations.split(", ").collect::<HashSet<_>>();
            let enable_all_optimisations = enabled_optimisations.contains("all");

            loop {
                let mut did_something = OptimisationResult::DidNothing;

                for (name, optimisation) in OPTIMISATIONS {
                    if enable_all_optimisations || enabled_optimisations.contains(name) {
                        did_something |= optimisation.optimise(&mut irs, &mut symtab);
                    }
                }

                if did_something == OptimisationResult::DidNothing {
                    break;
                }
            }

            for ir in &irs {
                pretty_print::pretty_print_tapir_function(ir, &symtab, &mut output).unwrap();
            }

            assert_snapshot!(output);
        });
    }
}
