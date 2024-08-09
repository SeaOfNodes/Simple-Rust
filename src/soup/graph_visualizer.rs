use fmt::Write;
use std::collections::HashSet;
use std::fmt;

use crate::soup::nodes::{Node, NodeId, Nodes};
use crate::soup::soup::Soup;

pub fn generate_dot_output(soup: &Soup) -> Result<String, fmt::Error> {
    let all = find_all(soup);

    let mut sb = String::new();
    writeln!(sb, "digraph chapter03 {{")?;
    // TODO write /* file.ro */
    writeln!(sb, "\trankdir=BT;")?;
    writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    nodes(&mut sb, &soup.nodes, &all)?;
    scopes(&mut sb, &soup.nodes, soup.scope)?;
    node_edges(&mut sb, &soup.nodes, &all)?;
    scope_edges(&mut sb, &soup.nodes, soup.scope)?;
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tsubgraph cluster_Nodes {{")?; // Magic "cluster_" in the subgraph name
    for n in all.iter().map(|n| &nodes[*n]) {
        if matches!(n, Node::Scope(_)) {
            continue;
        }
        write!(sb, "\t\t{} [ ", n.unique_name())?;
        if n.is_cfg() {
            write!(sb, "shape=box style=filled fillcolor=yellow ")?;
        }
        write!(sb, "label=\"{}\" ", n.glabel())?;
        writeln!(sb, "];")?;
    }
    writeln!(sb, "\t}}")
}

fn scopes(sb: &mut String, nodes: &Nodes, scope_node: NodeId) -> fmt::Result {
    let Node::Scope(scope) = &nodes[scope_node] else {
        unreachable!();
    };

    writeln!(sb, "\tnode [shape=plaintext];")?;

    for (level, s) in scope.scopes.iter().enumerate() {
        let scope_name = make_scope_name(&nodes[scope_node], level);

        writeln!(sb, "\tsubgraph cluster_{scope_name} {{")?;
        writeln!(sb, "\t\t{scope_name} [label=<")?;
        writeln!(
            sb,
            "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
        )?;
        writeln!(sb, "\t\t\t<TR><TD BGCOLOR=\"cyan\">{level}</TD>")?;
        for name in s.keys() {
            let port_name = make_port_name(&scope_name, name);
            writeln!(sb, "<TD PORT=\"{port_name}\">{name}</TD>")?;
        }
        writeln!(sb, "</TR>")?;
        writeln!(sb, "\t\t\t</TABLE>>];")?;
    }

    for _ in 0..scope.scopes.len() {
        writeln!(sb, "\t}}")?;
    }

    Ok(())
}

fn make_scope_name(scope: &Node, level: usize) -> String {
    format!("{}_{level}", scope.unique_name())
}

fn make_port_name(scope_name: &str, var_name: &str) -> String {
    format!("{scope_name}_{var_name}")
}

fn node_edges(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tedge [ fontname=Helvetica, fontsize=8 ];")?;
    for n in all.iter().map(|n| &nodes[*n]) {
        if matches!(n, Node::Scope(_)) {
            continue;
        }
        for (i, def) in n.base().inputs.iter().enumerate() {
            let def = if let Some(def) = def {
                &nodes[*def]
            } else {
                continue;
            };
            write!(sb, "\t{} -> {}", n.unique_name(), def.unique_name())?;

            write!(sb, "[taillabel={i}")?;

            if matches!(n, Node::Constant(_)) && matches!(def, Node::Start(_)) {
                write!(sb, " style=dotted")?;
            } else if def.is_cfg() {
                write!(sb, " color=red")?;
            }

            writeln!(sb, "];")?;
        }
    }
    Ok(())
}

fn scope_edges(sb: &mut String, nodes: &Nodes, scope_node: NodeId) -> fmt::Result {
    writeln!(sb, "\tedge [style=dashed color=cornflowerblue];")?;
    let Node::Scope(scope) = &nodes[scope_node] else {
        unreachable!();
    };
    for (level, s) in scope.scopes.iter().enumerate() {
        let scope_name = make_scope_name(&nodes[scope_node], level);

        for (name, index) in s {
            if let Some(def) = scope.base.inputs[*index] {
                let port_name = make_port_name(&scope_name, name);
                let def_name = nodes[def].unique_name();
                writeln!(sb, "\t{scope_name}:\"{port_name}\" -> {def_name};")?;
            }
        }
    }
    Ok(())
}

fn find_all(soup: &Soup) -> HashSet<NodeId> {
    let start = soup.start;
    let mut all = HashSet::new();
    for output in &soup.nodes[start].base().outputs {
        walk(&soup.nodes, &mut all, *output);
    }

    let Node::Scope(scope) = &soup.nodes[soup.scope] else {
        unreachable!();
    };
    for s in &scope.scopes {
        for index in s.values() {
            if let Some(input) = scope.base.inputs[*index] {
                walk(&soup.nodes, &mut all, input)
            }
        }
    }

    all
}

fn walk(nodes: &Nodes, all: &mut HashSet<NodeId>, node: NodeId) {
    if all.contains(&node) {
        return;
    }

    all.insert(node);

    for n in nodes[node].base().inputs.iter().flatten() {
        walk(nodes, all, *n);
    }

    for n in &nodes[node].base().outputs {
        walk(nodes, all, *n);
    }
}
