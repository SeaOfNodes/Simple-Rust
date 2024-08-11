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

    assert_eq!(String::from_utf8(output.stderr).unwrap(), "");
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

pub fn generate_dot_output(parser: &Parser) -> Result<String, fmt::Error> {
    let all = find_all(parser);

    let mut sb = String::new();
    writeln!(sb, "digraph chapter05 {{")?;
    writeln!(sb, "/*")?;
    writeln!(sb, "{}", parser.src())?;
    writeln!(sb, "*/")?;

    // TODO write /* file.ro */
    writeln!(sb, "\trankdir=BT;")?;
    writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    nodes(&mut sb, &parser.nodes, &all)?;
    for scope in &parser.x_scopes {
        scopes(&mut sb, &parser.nodes, *scope)?;
    }
    node_edges(&mut sb, &parser.nodes, &all)?;
    for scope in &parser.x_scopes {
        scope_edges(&mut sb, &parser.nodes, *scope)?;
    }
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes(sb: &mut String, nodes: &Nodes, all: &HashSet<NodeId>) -> fmt::Result {
    writeln!(sb, "\tsubgraph cluster_Nodes {{")?; // Magic "cluster_" in the subgraph name
    for &n in all.iter() {
        if matches!(&nodes[n], Node::Proj(_) | Node::Scope(_)) {
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
            }
            if matches!(&nodes[n], Node::Phi(_)) {
                write!(sb, "style=filled fillcolor=lightyellow ")?;
            }
            write!(sb, "label=\"{lab}\" ")?;
        }
        writeln!(sb, "];")?;
    }

    for &n in all {
        if let Node::Region = &nodes[n] {
            write!(sb, "\t\t{{ rank=same; {};", nodes.print(Some(n)))?;
            for &phi in &nodes.outputs[n] {
                if let Node::Phi(_) = &nodes[phi] {
                    write!(sb, "{};", nodes.unique_name(phi))?;
                }
            }
            writeln!(sb, "}}")?;
        }
    }

    writeln!(sb, "\t}}")
}

fn scopes(sb: &mut String, nodes: &Nodes, scope_node: NodeId) -> fmt::Result {
    let Node::Scope(scope) = &nodes[scope_node] else {
        unreachable!();
    };

    writeln!(sb, "\tnode [shape=plaintext];")?;

    for (level, s) in scope.scopes.iter().enumerate() {
        let scope_name = make_scope_name(nodes, scope_node, level);

        writeln!(sb, "\tsubgraph cluster_{scope_name} {{")?;
        writeln!(sb, "\t\t{scope_name} [label=<")?;
        writeln!(
            sb,
            "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
        )?;
        write!(sb, "\t\t\t<TR><TD BGCOLOR=\"cyan\">{level}</TD>")?;
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

            if let (Node::Phi(_), Node::Region) = (&nodes[n], &nodes[def]) {
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
        let scope_name = make_scope_name(nodes, scope_node, level);

        for (name, index) in s {
            if let Some(def) = nodes.inputs[scope_node][*index] {
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
    let start = parser.nodes.start;
    let mut all = HashSet::new();
    for output in &parser.nodes.outputs[start] {
        walk(&parser.nodes, &mut all, *output);
    }

    let Node::Scope(scope) = &parser.nodes[parser.scope] else {
        unreachable!();
    };
    for s in &scope.scopes {
        for index in s.values() {
            if let Some(input) = parser.nodes.inputs[parser.scope][*index] {
                walk(&parser.nodes, &mut all, input)
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

    for n in nodes.inputs[node].iter().flatten() {
        walk(nodes, all, *n);
    }

    for n in &nodes.outputs[node] {
        walk(nodes, all, *n);
    }
}
