use std::collections::HashSet;

use agb_fixnum::Num;

use crate::compile::{
    ir::{
        Constant, TapIr, TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult,
    },
    symtab_visitor::SymTab,
};
use crate::types::Type;

/// Replace `GetGlobal` with `Constant` when a global is never written to.
///
/// This optimization scans all functions to find which globals have `SetGlobal`
/// instructions. For any `GetGlobal` where the global index is NOT in that set,
/// we can replace it with a constant instruction using the global's initial value.
pub fn propagate_readonly_globals(
    program: &mut [TapIrFunction],
    symtab: &mut SymTab,
) -> OptimisationResult {
    if symtab.globals().is_empty() {
        return OptimisationResult::DidNothing;
    }

    // Collect all global indices that are written to
    let mut written_globals = HashSet::new();
    for func in program.iter() {
        let mut dfs = TapIrFunctionBlockIter::new_dfs(func);
        while let Some(block) = dfs.next(func) {
            for instr in block.instrs() {
                if let TapIr::SetGlobal { global_index, .. } = instr {
                    written_globals.insert(*global_index);
                }
            }
        }
    }

    // Replace GetGlobal with Constant for read-only globals
    let mut did_something = OptimisationResult::DidNothing;

    for func in program.iter_mut() {
        let mut dfs = TapIrFunctionBlockIter::new_dfs(func);
        while let Some(block) = dfs.next_mut(func) {
            for instr in block.instrs_mut() {
                if let TapIr::GetGlobal {
                    target,
                    global_index,
                } = *instr
                {
                    // Skip if this global is written to
                    if written_globals.contains(&global_index) {
                        continue;
                    }

                    // Look up global info and convert to constant
                    let globals = symtab.globals();
                    if global_index >= globals.len() {
                        continue;
                    }

                    let global_info = &globals[global_index];
                    let constant = match global_info.ty {
                        Type::Int => Constant::Int(global_info.initial_value),
                        Type::Fix => Constant::Fix(Num::from_raw(global_info.initial_value)),
                        Type::Bool => Constant::Bool(global_info.initial_value != 0),
                        // Skip for types we can't convert to constants
                        Type::Error | Type::Struct(_) | Type::Task => continue,
                    };

                    *instr = TapIr::Constant(target, constant);
                    did_something = OptimisationResult::DidSomething;
                }
            }
        }
    }

    did_something
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunctionId, SymbolId};
    use crate::compile::ir::{BlockExitInstr, BlockId, FunctionModifiers, TapIrBlock};
    use crate::compile::symtab_visitor::GlobalInfo;
    use crate::tokens::{FileId, Span};

    fn create_test_symtab_with_globals(globals: Vec<(String, Type, i32)>) -> SymTab<'static> {
        use crate::compile::symtab_visitor::GlobalId;

        let mut symtab = SymTab::new_empty();

        for (i, (name, ty, initial_value)) in globals.into_iter().enumerate() {
            let global_info = GlobalInfo {
                id: GlobalId(i),
                name: name.clone(),
                ty,
                initial_value,
                span: Span::new(FileId::new(0), 0, 0),
            };
            symtab.add_global_owned(name, global_info);
        }

        symtab
    }

    fn create_simple_function(id: FunctionId, instrs: Vec<TapIr>) -> TapIrFunction {
        let block = TapIrBlock::new(
            BlockId::from_raw(0),
            instrs,
            BlockExitInstr::Return(Box::new([])),
        );
        TapIrFunction::new(
            id,
            Box::new([block]),
            FunctionModifiers {
                event_handler: None,
            },
            Box::new([]),
            Box::new([]),
        )
    }

    #[test]
    fn test_readonly_global_is_propagated() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("CONST".to_string(), Type::Int, 42)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::GetGlobal {
                target: SymbolId(10),
                global_index: 0,
            }],
        )];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        assert_eq!(
            program[0].blocks().next().unwrap().instrs()[0],
            TapIr::Constant(SymbolId(10), Constant::Int(42))
        );
    }

    #[test]
    fn test_written_global_is_not_propagated() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("counter".to_string(), Type::Int, 0)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![
                TapIr::GetGlobal {
                    target: SymbolId(10),
                    global_index: 0,
                },
                TapIr::SetGlobal {
                    global_index: 0,
                    value: SymbolId(10),
                },
            ],
        )];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidNothing);
        assert!(matches!(
            program[0].blocks().next().unwrap().instrs()[0],
            TapIr::GetGlobal { .. }
        ));
    }

    #[test]
    fn test_fix_global_propagation() {
        let mut symtab = create_test_symtab_with_globals(vec![
            ("SPEED".to_string(), Type::Fix, 256), // 1.0 in 24.8 fixed point
        ]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::GetGlobal {
                target: SymbolId(10),
                global_index: 0,
            }],
        )];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        assert_eq!(
            program[0].blocks().next().unwrap().instrs()[0],
            TapIr::Constant(SymbolId(10), Constant::Fix(Num::from_raw(256)))
        );
    }

    #[test]
    fn test_bool_global_propagation() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("DEBUG".to_string(), Type::Bool, 1)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::GetGlobal {
                target: SymbolId(10),
                global_index: 0,
            }],
        )];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        assert_eq!(
            program[0].blocks().next().unwrap().instrs()[0],
            TapIr::Constant(SymbolId(10), Constant::Bool(true))
        );
    }

    #[test]
    fn test_empty_program_does_nothing() {
        let mut symtab = create_test_symtab_with_globals(vec![]);
        let mut program: Vec<TapIrFunction> = vec![];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidNothing);
    }

    #[test]
    fn test_mixed_globals() {
        let mut symtab = create_test_symtab_with_globals(vec![
            ("CONST".to_string(), Type::Int, 42),
            ("counter".to_string(), Type::Int, 0),
            ("SPEED".to_string(), Type::Fix, 512),
        ]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![
                TapIr::GetGlobal {
                    target: SymbolId(10),
                    global_index: 0,
                },
                TapIr::GetGlobal {
                    target: SymbolId(11),
                    global_index: 1,
                },
                TapIr::SetGlobal {
                    global_index: 1,
                    value: SymbolId(11),
                },
                TapIr::GetGlobal {
                    target: SymbolId(12),
                    global_index: 2,
                },
            ],
        )];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        let instrs = program[0].blocks().next().unwrap().instrs();
        // CONST should be replaced
        assert_eq!(instrs[0], TapIr::Constant(SymbolId(10), Constant::Int(42)));
        // counter should NOT be replaced (it's written)
        assert!(matches!(
            instrs[1],
            TapIr::GetGlobal {
                global_index: 1,
                ..
            }
        ));
        // SetGlobal should remain
        assert!(matches!(instrs[2], TapIr::SetGlobal { .. }));
        // SPEED should be replaced
        assert_eq!(
            instrs[3],
            TapIr::Constant(SymbolId(12), Constant::Fix(Num::from_raw(512)))
        );
    }

    #[test]
    fn test_write_in_different_function() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("shared".to_string(), Type::Int, 0)]);

        let mut program = vec![
            create_simple_function(
                FunctionId(0),
                vec![TapIr::GetGlobal {
                    target: SymbolId(10),
                    global_index: 0,
                }],
            ),
            create_simple_function(
                FunctionId(1),
                vec![TapIr::SetGlobal {
                    global_index: 0,
                    value: SymbolId(20),
                }],
            ),
        ];

        let result = propagate_readonly_globals(&mut program, &mut symtab);

        // Should NOT propagate because global 0 is written in function 1
        assert_eq!(result, OptimisationResult::DidNothing);
        assert!(matches!(
            program[0].blocks().next().unwrap().instrs()[0],
            TapIr::GetGlobal { .. }
        ));
    }
}
