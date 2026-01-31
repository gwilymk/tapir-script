use std::collections::{HashMap, HashSet};

use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::{DfsEvent, depth_first_search},
};

use crate::{
    reporting::{Diagnostics, ErrorKind},
    tokens::Span,
    types::{StructId, StructRegistry, Type},
};

/// Edge data for struct field references: (field_name, field_span)
type FieldEdge = (String, Span);

/// Check for recursive struct definitions using petgraph.
///
/// A struct is recursive if it contains itself directly or indirectly.
/// This would cause infinite expansion during property/variable handling.
pub fn check_recursive_structs(registry: &StructRegistry, diagnostics: &mut Diagnostics) {
    // Build a directed graph: nodes are struct IDs, edges are field references
    let mut graph: DiGraph<StructId, FieldEdge> = DiGraph::new();
    let mut node_indices: HashMap<StructId, NodeIndex> = HashMap::new();

    // Add nodes for all structs
    for (idx, _def) in registry.iter().enumerate() {
        let struct_id = StructId(idx as u32);
        let node_idx = graph.add_node(struct_id);
        node_indices.insert(struct_id, node_idx);
    }

    // Add edges for struct field references
    for (idx, def) in registry.iter().enumerate() {
        let from_id = StructId(idx as u32);
        let from_node = node_indices[&from_id];

        for field in &def.fields {
            if let Type::Struct(to_id) = field.ty {
                let to_node = node_indices[&to_id];
                graph.add_edge(from_node, to_node, (field.name.clone(), field.span));
            }
        }
    }

    // Track which structs we've already reported errors for
    let mut reported: HashSet<StructId> = HashSet::new();

    // Use DFS to detect cycles - track the current path through the graph
    let mut path: Vec<NodeIndex> = Vec::new();

    depth_first_search(&graph, graph.node_indices(), |event| {
        match event {
            DfsEvent::Discover(node, _) => {
                path.push(node);
            }
            DfsEvent::BackEdge(from, to) => {
                // Found a cycle! The cycle goes: to -> ... -> from -> to
                let cycle_start_id = graph[to];

                // Skip if we've already reported this cycle
                if reported.contains(&cycle_start_id) {
                    return;
                }

                // Extract the cycle path from our tracked path
                let cycle_start_pos = path.iter().position(|&n| n == to).unwrap();
                let cycle_nodes: Vec<_> = path[cycle_start_pos..].to_vec();

                // Build the cycle path with field information
                // Each step: current_node --field--> next_node
                let mut cycle_path: Vec<(StructId, String, Span, StructId)> = Vec::new();

                for window in cycle_nodes.windows(2) {
                    let current_node = window[0];
                    let next_node = window[1];

                    // Find the edge between current and next
                    if let Some(edge) = graph.find_edge(current_node, next_node) {
                        let (field_name, field_span) = graph[edge].clone();
                        cycle_path.push((
                            graph[current_node],
                            field_name,
                            field_span,
                            graph[next_node],
                        ));
                    }
                }

                // Add the back edge (from -> to) which closes the cycle
                if let Some(edge) = graph.find_edge(from, to) {
                    let (field_name, field_span) = graph[edge].clone();
                    cycle_path.push((graph[from], field_name, field_span, graph[to]));
                }

                // Report the error
                let def = registry.get(cycle_start_id);
                let mut error = ErrorKind::RecursiveStruct {
                    name: def.name.clone(),
                }
                .at(
                    def.span,
                    crate::reporting::DiagnosticMessage::StructDefinedHere,
                );

                // Add labels for each step in the cycle
                for (i, (from_struct_id, field_name, field_span, to_struct_id)) in
                    cycle_path.iter().enumerate()
                {
                    let is_last = i == cycle_path.len() - 1;

                    if is_last {
                        // This is the field that completes the cycle back to start
                        error = error.label(
                            *field_span,
                            crate::reporting::DiagnosticMessage::FieldCreatesCycle {
                                field_name: field_name.clone(),
                            },
                        );
                    } else {
                        // Intermediate step in the cycle - show the field and target
                        let target_def = registry.get(*to_struct_id);
                        error = error.label(
                            *field_span,
                            crate::reporting::DiagnosticMessage::FieldInCyclePath {
                                field_name: field_name.clone(),
                                target_struct: target_def.name.clone(),
                            },
                        );
                    }

                    // Mark all structs in the cycle as reported
                    reported.insert(*from_struct_id);
                }

                error.emit(diagnostics);
                reported.insert(cycle_start_id);
            }
            DfsEvent::Finish(_node, _) => {
                path.pop();
            }
            _ => {}
        }
    });
}
