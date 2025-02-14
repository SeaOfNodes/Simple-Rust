use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::node::{Scope, Stop};
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::types::Ty;
use fmt::Write;
use std::collections::HashSet;
use std::fmt;
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

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
    sea: &Nodes,
    stop: Stop,
    scope: Option<Scope>,
    x_scopes: &[Scope],
    source: &str,
    separate_control_cluster: bool,
) -> Result<String, fmt::Error> {
    let all = find_all(sea, &[Some(**stop), scope.map(|s| s.to_node())]);
    let mut all = Vec::from_iter(all);
    all.sort_by_key(Node::index);

    let mut sb = String::new();
    writeln!(sb, "digraph \"{}\" {{", source.replace("\"", "\\\""))?;

    writeln!(sb, "\trankdir=BT;")?;
    // writeln!(sb, "\tordering=\"in\";")?;
    writeln!(sb, "\tconcentrate=\"true\";")?;
    writeln!(sb, "\tcompound=\"true\";")?;

    do_nodes(&mut sb, &all, separate_control_cluster, sea)?;
    for &scope in x_scopes {
        if !scope.is_dead(sea) {
            do_scope(&mut sb, sea, scope)?;
        }
    }
    node_edges(&mut sb, sea, &all)?;
    for &scope in x_scopes {
        if !scope.is_dead(sea) {
            scope_edges(&mut sb, sea, scope)?;
        }
    }
    writeln!(sb, "}}")?;
    Ok(sb)
}

fn nodes_by_cluster(
    sb: &mut String,
    do_ctrl: bool,
    all: &[Node],
    separate_control_cluster: bool,
    sea: &Nodes,
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
        if n.is_proj(sea) || n.is_cproj(sea) || n.is_scope_min(sea) || n == **sea.xctrl {
            continue;
        }
        if separate_control_cluster && do_ctrl && !n.is_cfg(sea) {
            continue;
        }
        if separate_control_cluster && !do_ctrl && n.is_cfg(sea) {
            continue;
        }

        write!(sb, "\t\t{} [ ", n.unique_name(sea))?;
        let lab = sea[n].glabel();
        if sea[n].is_multi_node() {
            writeln!(sb, "shape=plaintext label=<")?;
            writeln!(
                sb,
                "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">"
            )?;
            writeln!(sb, "\t\t\t<TR><TD BGCOLOR=\"yellow\">{lab}</TD></TR>")?;
            write!(sb, "\t\t\t<TR>")?;

            let mut do_proj_table = false;

            // differs from java: we cannot mutate the outputs, so we copy
            let mut os = sea.outputs[n].clone();
            os.sort_by(|x, y| {
                if let (Some(xp), Some(yp)) = (x.to_proj(sea), y.to_proj(sea)) {
                    sea[xp].index.cmp(&sea[yp].index)
                } else {
                    x.index().cmp(&y.index())
                }
            });

            // n._outputs.sort((x,y) -> x instanceof ProjNode xp && y instanceof ProjNode yp ? (xp._idx - yp._idx) : (x._nid - y._nid));
            for use_ in os {
                if let proj @ Op::Proj(p) = &sea[use_] {
                    if !do_proj_table {
                        do_proj_table = true;
                        writeln!(sb, "<TD>")?;
                        writeln!(
                            sb,
                            "\t\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
                        )?;
                        write!(sb, "\t\t\t\t<TR>")?;
                    }
                    write!(sb, "<TD PORT=\"p{}\">{}</TD>", p.index, proj.glabel())?;
                }
                if let proj @ Op::CProj(p) = &sea[use_] {
                    if !do_proj_table {
                        do_proj_table = true;
                        writeln!(sb, "<TD>")?;
                        writeln!(
                            sb,
                            "\t\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
                        )?;
                        write!(sb, "\t\t\t\t<TR>")?;
                    }
                    write!(
                        sb,
                        "<TD PORT=\"p{}\" BGCOLOR=\"yellow\">{}</TD>",
                        p.index,
                        proj.glabel()
                    )?;
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
            if n.is_cfg(sea) {
                write!(sb, "shape=box style=filled fillcolor=yellow ")?;
            } else if n.is_phi(sea) {
                write!(sb, "style=filled fillcolor=lightyellow ")?;
            }
            write!(sb, "label=\"{lab}\" ")?;
        }
        writeln!(sb, "];")?;
    }

    if !separate_control_cluster {
        // Force Region & Phis to line up
        for &n in all {
            if n.is_region(sea) {
                write!(sb, "\t\t{{ rank=same; {};", n.print(sea))?;
                for &phi in &sea.outputs[n] {
                    if let Op::Phi(_) = &sea[phi] {
                        write!(sb, "{};", phi.unique_name(sea))?;
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
    all: &[Node],
    separate_control_cluster: bool,
    sea: &Nodes,
) -> fmt::Result {
    nodes_by_cluster(sb, true, all, separate_control_cluster, sea)?;
    nodes_by_cluster(sb, false, all, separate_control_cluster, sea)
}

fn do_scope(sb: &mut String, sea: &Nodes, scope: Scope) -> fmt::Result {
    writeln!(sb, "\tnode [shape=plaintext];")?;

    let last = scope.inputs(sea).len();
    let max = sea[scope].lex_size.len();
    for i in 0..max {
        let level = max - i - 1;
        let scope_name = make_scope_name(sea, scope, level);

        writeln!(sb, "\tsubgraph cluster_{scope_name} {{")?;
        if level == 0 {
            let n = scope.mem(sea);
            writeln!(
                sb,
                "\t\t{} [label=\"{}\"];",
                n.unique_name(sea),
                sea[n].glabel()
            )?;
        }
        writeln!(sb, "\t\t{scope_name} [label=<")?;
        writeln!(
            sb,
            "\t\t\t<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
        )?;
        write!(sb, "\t\t\t<TR><TD BGCOLOR=\"cyan\">{level}</TD>")?;
        let lex_start = sea[scope].lex_size[level];
        for j in lex_start..last {
            let name = sea[scope].vars[j].name;
            let port_name = make_port_name(&scope_name, name);
            write!(sb, "<TD PORT=\"{port_name}\">{name}</TD>")?;
        }
        writeln!(sb, "</TR>")?;
        writeln!(sb, "\t\t\t</TABLE>>];")?;
    }

    for _ in 0..max {
        writeln!(sb, "\t}}")?;
    }

    Ok(())
}

fn make_scope_name(sea: &Nodes, scope: Scope, level: usize) -> String {
    format!("{}_{level}", scope.unique_name(sea))
}

fn make_port_name(scope_name: &str, var_name: &str) -> String {
    format!("{scope_name}_{var_name}")
}

impl Node {
    fn is_mem(self, sea: &Nodes) -> bool {
        match &sea[self] {
            Op::Phi(p) => p.ty.is_mem(),
            Op::Proj(_) => self.ty(sea).is_some_and(Ty::is_mem),
            Op::Store(_) => true,
            _ => false,
        }
    }
}

fn node_edges(sb: &mut String, sea: &Nodes, all: &[Node]) -> fmt::Result {
    writeln!(sb, "\tedge [ fontname=Helvetica, fontsize=8 ];")?;
    for &n in all.iter() {
        if n.is_constant(sea)
            || n.is_proj(sea)
            || n.is_cproj(sea)
            || n.is_scope(sea)
            || n == **sea.xctrl
        {
            continue;
        }
        for (i, def) in sea.inputs[n].iter().enumerate() {
            let Some(def) = *def else { continue };

            if n.is_phi(sea) && def.is_region(sea) {
                writeln!(
                    sb,
                    "\t{} -> {} [style=dotted taillabel={i}];",
                    n.unique_name(sea),
                    def.unique_name(sea)
                )?;
                continue;
            }

            write!(sb, "\t{} -> {}", n.unique_name(sea), def_name(sea, def))?;

            write!(sb, "[taillabel={i}")?;

            if n.is_new(sea) {
                write!(sb, " color=green")?;
            } else if def.is_cfg(sea) {
                write!(sb, " color=red")?;
            } else if def.is_mem(sea) {
                write!(sb, " color=blue")?;
            }

            if i == 2 && (n.is_phi(sea) || n.is_loop(sea)) {
                write!(sb, " constraint=false")?;
            }

            writeln!(sb, "];")?;
        }
    }
    Ok(())
}

fn scope_edges(sb: &mut String, sea: &Nodes, scope: Scope) -> fmt::Result {
    writeln!(sb, "\tedge [style=dashed color=cornflowerblue];")?;
    let mut level = 0;
    for v in &sea[scope].vars {
        let mut def = scope.inputs(sea)[v.index];
        while let Some(lazy) = def.and_then(|d| d.to_scope(sea)) {
            def = lazy.inputs(sea)[v.index];
        }
        let Some(def) = def else { continue };
        while level < sea[scope].lex_size.len() && v.index >= sea[scope].lex_size[level] {
            level += 1;
        }
        let scope_name = make_scope_name(sea, scope, level - 1);
        let port_name = make_port_name(&scope_name, v.name);
        let def_name = def_name(sea, def);
        writeln!(sb, "\t{scope_name}:\"{port_name}\" -> {def_name};")?;
    }
    Ok(())
}

fn def_name(sea: &Nodes, def: Node) -> String {
    match &sea[def] {
        Op::CProj(p) | Op::Proj(p) => {
            let mname = sea.inputs[def][0].unwrap().unique_name(sea);
            format!("{mname}:p{}", p.index)
        }
        _ => def.unique_name(sea),
    }
}

fn find_all(nodes: &Nodes, leaves: &[Option<Node>]) -> HashSet<Node> {
    let mut all = HashSet::new();
    for &node in leaves.iter().flatten() {
        walk(nodes, &mut all, Some(node));
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
