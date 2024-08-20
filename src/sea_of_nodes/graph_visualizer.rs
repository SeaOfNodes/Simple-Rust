use fmt::Write;
use std::collections::HashSet;
use std::fmt;
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::parser::Parser;

pub fn run_graphviz_and_chromium(input: String) {
    let child = Command::new(&"bash")
        .args(["-c", "dot -Tsvg | base64"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    use std::io::Write;
    child
        .stdin
        .as_ref()
        .unwrap()
        .write_all(input.as_ref())
        .unwrap();

    let output = child.wait_with_output().unwrap();

    // assert_eq!(String::from_utf8(output.stderr).unwrap(), "");
    println!(
        "GRAPHVIZ BEGIN:\n{}GRAPHVIZ END",
        std::str::from_utf8(&output.stderr).unwrap()
    );
    assert_eq!(output.status.code(), Some(0));

    let url = format!(
        "data:image/svg+xml;base64,{}",
        std::str::from_utf8(&output.stdout).unwrap()
    );

    static LOCK: Mutex<()> = Mutex::new(());
    let _guard = LOCK.lock().unwrap();

    // using chromium becasue firefox only displays it after manually selecting the address hitting enter
    Command::new(&"chromium")
        .arg(url)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();

    std::thread::sleep(Duration::from_millis(1000)); // give it some time...
}

pub fn generate_dot_output(
    parser: &Parser,
    separate_control_cluster: bool,
) -> Result<String, fmt::Error> {
    let all = find_all(parser);

    let mut sb = String::new();
    writeln!(sb, "digraph \"{}\" {{", parser.src().replace("\"", "\\\""))?;

    // TODO write /* file.ro */
    writeln!(sb, "\trankdir=BT;")?;
    writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    nodes(&mut sb, &parser.nodes, &all, separate_control_cluster)?;
    for s in &parser.x_scopes {
        scope(&mut sb, &parser.nodes, *s)?;
    }
    node_edges(&mut sb, &parser.nodes, &all)?;
    for scope in &parser.x_scopes {
        scope_edges(&mut sb, &parser.nodes, *scope)?;
    }
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes_by_cluster(
    sb: &mut String,
    do_ctrl: bool,
    nodes: &Nodes,
    all: &HashSet<NodeId>,
    separate_control_cluster: bool,
) -> fmt::Result {
    if !separate_control_cluster && do_ctrl {
        return Ok(());
    }
    if do_ctrl {
        writeln!(sb, "\tsubgraph cluster_Controls {{")?;
    } else {
        writeln!(sb, "\tsubgraph cluster_Nodes {{")?; // Magic "cluster_" in the subgraph name
    }

    for &n in all.iter() {
        if matches!(&nodes[n], Node::Proj(_) | Node::Scope(_)) {
            continue;
        }
        if separate_control_cluster && do_ctrl && !nodes.is_cfg(n) {
            continue;
        }
        if separate_control_cluster && !do_ctrl && nodes.is_cfg(n) {
            continue;
        }

        write!(sb, "\t\t{} [ ", nodes.unique_name(n))?;
        let lab = nodes[n].glabel();
        if nodes[n].is_multi_node() {
            writeln!(sb, "shape=plaintext label=<")?;
            writeln!(
                sb,
                "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
            )?;
            writeln!(sb, "\t\t\t<TR><TD BGCOLOR=\"yellow\">{lab}</TD></TR>")?;
            write!(sb, "\t\t\t<TR>")?;

            let mut do_proj_table = false;
            for use_ in &nodes.outputs[n] {
                if let proj @ Node::Proj(p) = &nodes[*use_] {
                    if !do_proj_table {
                        do_proj_table = true;
                        writeln!(sb, "<TD>")?;
                        writeln!(
                            sb,
                            "\t\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
                        )?;
                        write!(sb, "\t\t\t\t<TR>")?;
                    }
                    write!(sb, "<TD PORT=\"p{}\"", p.index)?;
                    if nodes.is_cfg(*use_) {
                        write!(sb, " BGCOLOR=\"yellow\"")?;
                    };
                    write!(sb, ">{}</TD>", proj.glabel())?;
                }
            }
            if do_proj_table {
                writeln!(sb, "</TR>")?;
                writeln!(sb, "\t\t\t\t</TABLE>")?;
                write!(sb, "\t\t\t</TD>")?;
            }
            writeln!(sb, "</TR>")?;
            write!(sb, "\t\t\t</TABLE>>\n\t\t")?;
        } else {
            if nodes.is_cfg(n) {
                write!(sb, "shape=box style=filled fillcolor=yellow ")?;
            } else if matches!(&nodes[n], Node::Phi(_)) {
                write!(sb, "style=filled fillcolor=lightyellow ")?;
            }
            write!(sb, "label=\"{lab}\" ")?;
        }
        writeln!(sb, "];")?;
    }

    if !separate_control_cluster {
        // Force Region & Phis to line up
        for &n in all {
            if let Node::Region { .. } | Node::Loop = &nodes[n] {
                write!(sb, "\t\t{{ rank=same; {};", nodes.print(Some(n)))?;
                for &phi in &nodes.outputs[n] {
                    if let Node::Phi(_) = &nodes[phi] {
                        write!(sb, "{};", nodes.unique_name(phi))?;
                    }
                }
                writeln!(sb, "}}")?;
            }
        }
    }

    writeln!(sb, "\t}}")
}

fn nodes(
    sb: &mut String,
    nodes: &Nodes,
    all: &HashSet<NodeId>,
    separate_control_cluster: bool,
) -> fmt::Result {
    nodes_by_cluster(sb, true, nodes, all, separate_control_cluster)?;
    nodes_by_cluster(sb, false, nodes, all, separate_control_cluster)
}

fn scope(sb: &mut String, nodes: &Nodes, scope_node: NodeId) -> fmt::Result {
    let Node::Scope(scope) = &nodes[scope_node] else {
        unreachable!();
    };

    writeln!(sb, "\tnode [shape=plaintext];")?;

    for (level, s) in scope.scopes.iter().rev().enumerate() {
        let scope_name = make_scope_name(nodes, scope_node, level + 1);

        writeln!(sb, "\tsubgraph cluster_{scope_name} {{")?;
        writeln!(sb, "\t\t{scope_name} [label=<")?;
        writeln!(
            sb,
            "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
        )?;
        let scope_level = scope.scopes.len() - level - 1;
        write!(sb, "\t\t\t<TR><TD BGCOLOR=\"cyan\">{scope_level}</TD>")?;
        for name in s.keys() {
            let port_name = make_port_name(&scope_name, name);
            write!(sb, "<TD PORT=\"{port_name}\">{name}</TD>")?;
        }
        writeln!(sb, "</TR>")?;
        writeln!(sb, "\t\t\t</TABLE>>];")?;
    }

    for _ in 0..scope.scopes.len() {
        writeln!(sb, "\t}}")?;
    }

    Ok(())
}

fn make_scope_name(nodes: &Nodes, scope: NodeId, level: usize) -> String {
    format!("{}_{level}", nodes.unique_name(scope))
}

fn make_port_name(scope_name: &str, var_name: &str) -> String {
    format!("{scope_name}_{var_name}")
}

fn node_edges(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tedge [ fontname=Helvetica, fontsize=8 ];")?;
    for &n in all.iter() {
        if matches!(
            &nodes[n],
            Node::Constant(_) | Node::Proj(_) | Node::Scope(_)
        ) {
            continue;
        }
        for (i, def) in nodes.inputs[n].iter().enumerate() {
            let Some(def) = *def else { continue };

            if let (Node::Phi(_), Node::Region { .. } | Node::Loop) = (&nodes[n], &nodes[def]) {
                writeln!(
                    sb,
                    "\t{} -> {} [style=dotted taillabel={i}];",
                    nodes.unique_name(n),
                    nodes.unique_name(def)
                )?;
                continue;
            }

            write!(sb, "\t{} -> {}", nodes.unique_name(n), def_name(nodes, def))?;

            write!(sb, "[taillabel={i}")?;

            if matches!(nodes[n], Node::Constant(_)) && matches!(nodes[def], Node::Start { .. }) {
                write!(sb, " style=dotted")?;
            } else if nodes.is_cfg(def) {
                write!(sb, " color=red")?;
            }

            if i == 2 && matches!(&nodes[n], Node::Phi(_) | Node::Loop) {
                write!(sb, " constraint=false")?;
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
    for (level, s) in scope.scopes.iter().rev().enumerate() {
        let scope_name = make_scope_name(nodes, scope_node, level + 1);

        for (name, index) in s {
            let mut def = nodes.inputs[scope_node][*index];
            while def.is_some() && matches!(&nodes[def.unwrap()], Node::Scope(_)) {
                def = nodes.inputs[def.unwrap()][*index]; // lazy
            }
            if let Some(def) = def {
                let port_name = make_port_name(&scope_name, name);
                let def_name = def_name(&nodes, def);
                writeln!(sb, "\t{scope_name}:\"{port_name}\" -> {def_name};")?;
            }
        }
    }
    Ok(())
}

fn def_name(nodes: &Nodes, def: NodeId) -> String {
    match &nodes[def] {
        Node::Proj(p) => {
            let mname = nodes.unique_name(nodes.inputs[def][0].unwrap());
            format!("{mname}:p{}", p.index)
        }
        _ => nodes.unique_name(def),
    }
}

fn find_all(parser: &Parser) -> HashSet<NodeId> {
    let mut all = HashSet::new();
    for output in &parser.nodes.outputs[parser.nodes.start] {
        walk(&parser.nodes, &mut all, Some(*output));
    }
    for node in &parser.nodes.inputs[parser.scope] {
        walk(&parser.nodes, &mut all, *node);
    }
    all
}

fn walk(nodes: &Nodes, all: &mut HashSet<NodeId>, node: Option<NodeId>) {
    let Some(node) = node else {
        return;
    };
    if node == NodeId::DUMMY {
        return;
    }

    if all.contains(&node) {
        return;
    }

    all.insert(node);

    for n in &nodes.inputs[node] {
        walk(nodes, all, *n);
    }

    for n in &nodes.outputs[node] {
        walk(nodes, all, Some(*n));
    }
}
