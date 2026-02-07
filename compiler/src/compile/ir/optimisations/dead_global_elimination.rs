use std::collections::HashSet;

use crate::compile::{
    ir::{TapIr, TapIrFunction, TapIrFunctionBlockIter, optimisations::OptimisationResult},
    symtab_visitor::SymTab,
};

/// Remove globals that are never read (even if they are written to).
///
/// This optimization:
/// 1. Scans all functions to collect which global indices have `GetGlobal` (are read)
/// 2. Removes `SetGlobal` instructions for unread globals
/// 3. Updates all remaining `GetGlobal`/`SetGlobal` instructions with new indices
/// 4. Removes unused globals from symtab via `retain_globals()`
pub fn eliminate_dead_globals(
    program: &mut [TapIrFunction],
    symtab: &mut SymTab,
) -> OptimisationResult {
    if symtab.globals().is_empty() {
        return OptimisationResult::DidNothing;
    }

    // Collect all global indices that are read
    let mut read_globals = HashSet::new();
    for func in program.iter() {
        let mut dfs = TapIrFunctionBlockIter::new_dfs(func);
        while let Some(block) = dfs.next(func) {
            for instr in block.instrs() {
                if let TapIr::GetGlobal { global_index, .. } = instr {
                    read_globals.insert(*global_index);
                }
            }
        }
    }

    // Track original globals count before removal
    let original_globals_count = symtab.globals().len();

    // If all globals are read, nothing to do
    if read_globals.len() == original_globals_count {
        return OptimisationResult::DidNothing;
    }

    // Build remapping from old index to new index for globals that ARE read
    let old_to_new = symtab.retain_globals(|g| read_globals.contains(&g.id.0));

    // Update all GetGlobal/SetGlobal instructions and remove SetGlobal for dead globals
    for func in program.iter_mut() {
        let mut dfs = TapIrFunctionBlockIter::new_dfs(func);
        while let Some(block) = dfs.next_mut(func) {
            // Remove SetGlobal instructions for unread globals
            block.instrs_retain(|instr| {
                if let TapIr::SetGlobal { global_index, .. } = instr {
                    // Keep only if this global is still in the mapping (i.e., it was read)
                    old_to_new.contains_key(global_index)
                } else {
                    true
                }
            });

            // Update indices in remaining GetGlobal/SetGlobal
            for instr in block.instrs_mut() {
                match instr {
                    TapIr::GetGlobal { global_index, .. }
                    | TapIr::SetGlobal { global_index, .. } => {
                        if let Some(&new_index) = old_to_new.get(global_index) {
                            *global_index = new_index;
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    OptimisationResult::DidSomething
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunctionId, SymbolId};
    use crate::compile::ir::{BlockExitInstr, BlockId, FunctionModifiers, StoreValue, TapIrBlock};
    use crate::compile::symtab_visitor::{GlobalId, GlobalInfo};
    use crate::tokens::{FileId, Span};
    use crate::types::Type;

    fn create_test_symtab_with_globals(globals: Vec<(String, Type, i32)>) -> SymTab<'static> {
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
    fn test_unread_global_is_eliminated() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("unused".to_string(), Type::Int, 0)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::SetGlobal {
                global_index: 0,
                value: StoreValue::Symbol(SymbolId(10)),
            }],
        )];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        // Global should be removed from symtab
        assert_eq!(symtab.globals().len(), 0);
        // SetGlobal should be removed
        assert_eq!(program[0].blocks().next().unwrap().instrs().len(), 0);
    }

    #[test]
    fn test_read_global_is_kept() {
        let mut symtab = create_test_symtab_with_globals(vec![("used".to_string(), Type::Int, 42)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::GetGlobal {
                target: SymbolId(10),
                global_index: 0,
            }],
        )];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidNothing);
        // Global should remain
        assert_eq!(symtab.globals().len(), 1);
        // GetGlobal should remain
        assert_eq!(program[0].blocks().next().unwrap().instrs().len(), 1);
    }

    #[test]
    fn test_mixed_globals_renumbering() {
        let mut symtab = create_test_symtab_with_globals(vec![
            ("A".to_string(), Type::Int, 1), // index 0 - will be removed
            ("B".to_string(), Type::Int, 2), // index 1 - will become 0
            ("C".to_string(), Type::Int, 3), // index 2 - will be removed
            ("D".to_string(), Type::Int, 4), // index 3 - will become 1
        ]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![
                TapIr::SetGlobal {
                    global_index: 0,
                    value: StoreValue::Symbol(SymbolId(10)),
                }, // A - dead
                TapIr::GetGlobal {
                    target: SymbolId(11),
                    global_index: 1,
                }, // B - read
                TapIr::SetGlobal {
                    global_index: 2,
                    value: StoreValue::Symbol(SymbolId(12)),
                }, // C - dead
                TapIr::GetGlobal {
                    target: SymbolId(13),
                    global_index: 3,
                }, // D - read
                TapIr::SetGlobal {
                    global_index: 1,
                    value: StoreValue::Symbol(SymbolId(14)),
                }, // B - write after read
            ],
        )];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        // Only B and D should remain
        assert_eq!(symtab.globals().len(), 2);
        assert_eq!(symtab.globals()[0].name, "B");
        assert_eq!(symtab.globals()[1].name, "D");

        let instrs = program[0].blocks().next().unwrap().instrs();
        // Should have 3 instructions: GetGlobal B, GetGlobal D, SetGlobal B
        assert_eq!(instrs.len(), 3);

        // B should now have index 0
        assert!(matches!(
            instrs[0],
            TapIr::GetGlobal {
                global_index: 0,
                ..
            }
        ));
        // D should now have index 1
        assert!(matches!(
            instrs[1],
            TapIr::GetGlobal {
                global_index: 1,
                ..
            }
        ));
        // SetGlobal B should now have index 0
        assert!(matches!(
            instrs[2],
            TapIr::SetGlobal {
                global_index: 0,
                ..
            }
        ));
    }

    #[test]
    fn test_empty_program_does_nothing() {
        let mut symtab = create_test_symtab_with_globals(vec![]);
        let mut program: Vec<TapIrFunction> = vec![];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidNothing);
    }

    #[test]
    fn test_all_globals_read_does_nothing() {
        let mut symtab = create_test_symtab_with_globals(vec![
            ("A".to_string(), Type::Int, 1),
            ("B".to_string(), Type::Int, 2),
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
            ],
        )];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidNothing);
        assert_eq!(symtab.globals().len(), 2);
    }

    #[test]
    fn test_read_in_different_function() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("shared".to_string(), Type::Int, 42)]);

        let mut program = vec![
            create_simple_function(
                FunctionId(0),
                vec![TapIr::SetGlobal {
                    global_index: 0,
                    value: StoreValue::Symbol(SymbolId(10)),
                }],
            ),
            create_simple_function(
                FunctionId(1),
                vec![TapIr::GetGlobal {
                    target: SymbolId(20),
                    global_index: 0,
                }],
            ),
        ];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        // Should NOT eliminate because global 0 is read in function 1
        assert_eq!(result, OptimisationResult::DidNothing);
        assert_eq!(symtab.globals().len(), 1);
    }

    #[test]
    fn test_global_written_but_never_read() {
        let mut symtab =
            create_test_symtab_with_globals(vec![("debug_counter".to_string(), Type::Int, 0)]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![
                TapIr::SetGlobal {
                    global_index: 0,
                    value: StoreValue::Symbol(SymbolId(10)),
                },
                TapIr::SetGlobal {
                    global_index: 0,
                    value: StoreValue::Symbol(SymbolId(11)),
                },
            ],
        )];

        let result = eliminate_dead_globals(&mut program, &mut symtab);

        assert_eq!(result, OptimisationResult::DidSomething);
        // Global should be removed
        assert_eq!(symtab.globals().len(), 0);
        // All SetGlobal instructions should be removed
        assert_eq!(program[0].blocks().next().unwrap().instrs().len(), 0);
    }

    #[test]
    fn test_global_names_updated() {
        let mut symtab = create_test_symtab_with_globals(vec![
            ("dead".to_string(), Type::Int, 0),
            ("alive".to_string(), Type::Int, 42),
        ]);

        let mut program = vec![create_simple_function(
            FunctionId(0),
            vec![TapIr::GetGlobal {
                target: SymbolId(10),
                global_index: 1,
            }],
        )];

        eliminate_dead_globals(&mut program, &mut symtab);

        // "dead" should no longer be findable
        assert!(symtab.get_global_by_name("dead").is_none());
        // "alive" should still be findable with new index
        let alive = symtab.get_global_by_name("alive").unwrap();
        assert_eq!(alive.id.0, 0); // Should now be at index 0
    }
}
