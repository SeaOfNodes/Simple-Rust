use fmt::Write;
use std::collections::HashSet;
use std::fmt;
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

use crate::sea_of_nodes::nodes::index::{Scope, Stop};
use crate::sea_of_nodes::nodes::{MemOpKind, Node, Nodes, Op};
use crate::sea_of_nodes::types::Type;

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
    nodes: &Nodes,
    stop: Stop,
    scope: Option<Scope>,
    x_scopes: &[Scope],
    source: &str,
    separate_control_cluster: bool,
) -> Result<String, fmt::Error> {
    let all = find_all(nodes, &[Some(*stop), scope.as_deref().copied()]);

    let mut sb = String::new();
    writeln!(sb, "digraph \"{}\" {{", source.replace("\"", "\\\""))?;

    writeln!(sb, "\trankdir=BT;")?;
    // writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    do_nodes(&mut sb, nodes, &all, separate_control_cluster)?;
    for &scope in x_scopes {
        if !nodes.is_dead(*scope) {
            do_scope(&mut sb, nodes, scope)?;
        }
    }
    node_edges(&mut sb, nodes, &all)?;
    for &scope in x_scopes {
        if !nodes.is_dead(*scope) {
            scope_edges(&mut sb, nodes, scope)?;
        }
    }
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes_by_cluster(
    sb: &mut String,
    do_ctrl: bool,
    nodes: &Nodes,
    all: &HashSet<Node>,
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
        if matches!(&nodes[n], Op::Proj(_) | Op::Scope(_)) {
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
                if let proj @ Op::Proj(p) = &nodes[*use_] {
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
            } else if matches!(&nodes[n], Op::Phi(_)) {
                write!(sb, "style=filled fillcolor=lightyellow ")?;
            }
            write!(sb, "label=\"{lab}\" ")?;
        }
        writeln!(sb, "];")?;
    }

    if !separate_control_cluster {
        // Force Region & Phis to line up
        for &n in all {
            if let Op::Region { .. } | Op::Loop = &nodes[n] {
                write!(sb, "\t\t{{ rank=same; {};", nodes.print(Some(n)))?;
                for &phi in &nodes.outputs[n] {
                    if let Op::Phi(_) = &nodes[phi] {
                        write!(sb, "{};", nodes.unique_name(phi))?;
                    }
                }
                writeln!(sb, "}}")?;
            }
        }
    }

    writeln!(sb, "\t}}")
}

fn do_nodes(
    sb: &mut String,
    nodes: &Nodes,
    all: &HashSet<Node>,
    separate_control_cluster: bool,
) -> fmt::Result {
    nodes_by_cluster(sb, true, nodes, all, separate_control_cluster)?;
    nodes_by_cluster(sb, false, nodes, all, separate_control_cluster)
}

fn do_scope(sb: &mut String, nodes: &Nodes, scope: Scope) -> fmt::Result {
    let scopes = &nodes[scope].scopes;
    writeln!(sb, "\tnode [shape=plaintext];")?;

    for (level, s) in scopes.iter().rev().enumerate() {
        let scope_name = make_scope_name(nodes, scope, level + 1);

        writeln!(sb, "\tsubgraph cluster_{scope_name} {{")?;
        writeln!(sb, "\t\t{scope_name} [label=<")?;
        writeln!(
            sb,
            "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
        )?;
        let scope_level = scopes.len() - level - 1;
        write!(sb, "\t\t\t<TR><TD BGCOLOR=\"cyan\">{scope_level}</TD>")?;
        for name in s.keys() {
            let port_name = make_port_name(&scope_name, name);
            write!(sb, "<TD PORT=\"{port_name}\">{name}</TD>")?;
        }
        writeln!(sb, "</TR>")?;
        writeln!(sb, "\t\t\t</TABLE>>];")?;
    }

    for _ in 0..scopes.len() {
        writeln!(sb, "\t}}")?;
    }

    Ok(())
}

fn make_scope_name(nodes: &Nodes, scope: Scope, level: usize) -> String {
    format!("{}_{level}", nodes.unique_name(*scope))
}

fn make_port_name(scope_name: &str, var_name: &str) -> String {
    format!("{scope_name}_{var_name}")
}

impl Node {
    fn is_mem(self, sea: &Nodes) -> bool {
        match &sea[self] {
            Op::Phi(p) => matches!(*p.ty, Type::Memory(_)),
            Op::Proj(_) => matches!(self.ty(sea).as_deref(), Some(Type::Memory(_))),
            Op::Mem(m) => matches!(m.kind, MemOpKind::Store),
            _ => false,
        }
    }
}

fn node_edges(sb: &mut String, nodes: &Nodes, all: &HashSet<Node>) -> fmt::Result {
    writeln!(sb, "\tedge [ fontname=Helvetica, fontsize=8 ];")?;
    for &n in all.iter() {
        if matches!(&nodes[n], Op::Constant(_) | Op::Proj(_) | Op::Scope(_)) {
            continue;
        }
        for (i, def) in nodes.inputs[n].iter().enumerate() {
            let Some(def) = *def else { continue };

            if let (Op::Phi(_), Op::Region { .. } | Op::Loop) = (&nodes[n], &nodes[def]) {
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

            if n.to_new(nodes).is_some() {
                write!(sb, " color=green")?;
            } else if nodes.is_cfg(def) {
                write!(sb, " color=red")?;
            } else if def.is_mem(nodes) {
                write!(sb, " color=blue")?;
            }

            if matches!(nodes[n], Op::Constant(_)) && matches!(nodes[def], Op::Start { .. }) {
                write!(sb, " style=dotted")?;
            } else if nodes.is_cfg(def) {
                write!(sb, " color=red")?;
            }

            if i == 2 && matches!(&nodes[n], Op::Phi(_) | Op::Loop) {
                write!(sb, " constraint=false")?;
            }

            writeln!(sb, "];")?;
        }
    }
    Ok(())
}

fn scope_edges(sb: &mut String, nodes: &Nodes, scope: Scope) -> fmt::Result {
    writeln!(sb, "\tedge [style=dashed color=cornflowerblue];")?;
    for (level, s) in nodes[scope].scopes.iter().rev().enumerate() {
        let scope_name = make_scope_name(nodes, scope, level + 1);

        for (name, (index, _ty)) in s {
            let mut def = nodes.inputs[scope][*index];
            while def.is_some() && matches!(&nodes[def.unwrap()], Op::Scope(_)) {
                def = nodes.inputs[def.unwrap()][*index]; // lazy
            }
            if let Some(def) = def {
                let port_name = make_port_name(&scope_name, name);
                let def_name = def_name(nodes, def);
                writeln!(sb, "\t{scope_name}:\"{port_name}\" -> {def_name};")?;
            }
        }
    }
    Ok(())
}

fn def_name(nodes: &Nodes, def: Node) -> String {
    match &nodes[def] {
        Op::Proj(p) => {
            let mname = nodes.unique_name(nodes.inputs[def][0].unwrap());
            format!("{mname}:p{}", p.index)
        }
        _ => nodes.unique_name(def),
    }
}

fn find_all(nodes: &Nodes, leaves: &[Option<Node>]) -> HashSet<Node> {
    let mut all = HashSet::new();
    for &node in leaves.iter().flatten() {
        for output in &nodes.outputs[node] {
            walk(nodes, &mut all, Some(*output));
        }
    }
    all
}

fn walk(nodes: &Nodes, all: &mut HashSet<Node>, node: Option<Node>) {
    let Some(node) = node else {
        return;
    };
    if node == Node::DUMMY {
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
