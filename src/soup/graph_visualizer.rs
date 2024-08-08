use fmt::Write;
use std::collections::HashSet;
use std::fmt;

use crate::soup::nodes::{Node, NodeId, Nodes};
use crate::soup::soup::Soup;

pub fn generate_dot_output(soup: &Soup) -> Result<String, fmt::Error> {
    let all = find_all(soup);

    let mut sb = String::new();
    writeln!(sb, "digraph chapter02 {{")?;
    // TODO write /* file.ro */
    writeln!(sb, "\trankdir=BT;")?;
    writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    nodes(&mut sb, &soup.nodes, &all)?;
    node_edges(&mut sb, &soup.nodes, &all)?;
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tsubgraph cluster_Nodes {{")?; // Magic "cluster_" in the subgraph name
    for n in all.iter().map(|n| &nodes[*n]) {
        write!(sb, "\t\t{} [ ", n.unique_name())?;
        if n.is_cfg() {
            write!(sb, "shape=box style=filled fillcolor=yellow ")?;
        }
        write!(sb, "label=\"{}\" ", n.glabel())?;
        writeln!(sb, "];")?;
    }
    writeln!(sb, "\t}}")
}

fn node_edges(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tedge [ fontname=Helvetica, fontsize=8 ];")?;
    for n in all.iter().map(|n| &nodes[*n]) {
        for (i, def) in n.base().inputs.iter().enumerate() {
            let def = if let Some(def) = def {
                &nodes[*def]
            } else {
                continue;
            };
            write!(sb, "\t{} -> {}", n.unique_name(), def.unique_name())?;

            write!(sb, "[taillabel={i}")?;

            if matches!(n, Node::ConstantNode(_)) && matches!(def, Node::StartNode(_)) {
                write!(sb, " style=dotted")?;
            } else if def.is_cfg() {
                write!(sb, " color=red")?;
            }

            writeln!(sb, "];")?;
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
