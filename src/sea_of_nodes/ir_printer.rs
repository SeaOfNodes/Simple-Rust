use std::borrow::Cow;
use std::fmt;
use std::fmt::Write;

use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::types::{Int, Ty, Type};

pub fn pretty_print_llvm(nodes: &Nodes, node: impl Into<Node>, depth: usize) -> String {
    pretty_print_(nodes, node.into(), depth, true)
}

/// Another bulk pretty-printer.  Makes more effort at basic-block grouping.
pub fn pretty_print(nodes: &Nodes, node: impl Into<Node>, depth: usize) -> String {
    pretty_print_(nodes, node.into(), depth, false)
}

fn pretty_print_(nodes: &Nodes, node: Node, depth: usize, llvm_format: bool) -> String {
    // First, a Breadth First Search at a fixed depth.
    let bfs = BFS::run(nodes, node, depth);

    // Convert just that set to a post-order
    let mut rpos = Vec::with_capacity(bfs._bfs.len());
    let mut visit = IdSet::zeros(nodes.len());

    for i in bfs._lim..bfs._bfs.len() {
        post_ord(bfs._bfs[i], &mut rpos, &mut visit, &bfs._bs, nodes);
    }

    // Reverse the post-order walk
    let mut sb = String::new();
    let mut gap = false;

    let mut iter = rpos.iter().rev().peekable();
    while let Some(&n) = iter.next() {
        if nodes.is_cfg(n) || nodes.is_multi_head(n) {
            if !gap {
                sb.push('\n'); // Blank before multihead
            };
            print_line(nodes, n, &mut sb, llvm_format).unwrap(); // Print head
            while let Some(&&t) = iter.peek() {
                if nodes.is_multi_tail(t) {
                    break;
                }
                iter.next();
                print_line(nodes, t, &mut sb, llvm_format).unwrap();
            }
            sb.push('\n'); // Blank after multitail
            gap = true;
        } else {
            print_line(nodes, n, &mut sb, llvm_format).unwrap();
            gap = false;
        }
    }
    sb
}

fn print_line(nodes: &Nodes, n: Node, sb: &mut String, llvm_format: bool) -> fmt::Result {
    if llvm_format {
        print_line_llvm(nodes, n, sb)
    } else {
        print_line_(nodes, n, sb)
    }
}

/// Print a node on 1 line, columnar aligned, as:
/// NNID NNAME DDEF DDEF  [[  UUSE UUSE  ]]  TYPE
/// 1234 sssss 1234 1234 1234 1234 1234 1234 tttttt
fn print_line_(sea: &Nodes, n: Node, sb: &mut String) -> fmt::Result {
    write!(sb, "{n:4} {: <7.7} ", sea[n].label())?;

    if n.is_dead(sea) {
        return writeln!(sb, "DEAD");
    }

    for def in &sea.inputs[n] {
        if let Some(def) = def {
            write!(sb, "{def:4} ")?;
        } else {
            write!(sb, "____ ")?;
        }
    }
    for _ in sea.inputs[n].len()..3 {
        sb.push_str("     ");
    }
    sb.push_str(" [[  ");

    for use_ in &sea.outputs[n] {
        if *use_ != Node::DUMMY {
            write!(sb, "{use_:4} ")?;
        } else {
            write!(sb, "____ ")?;
        }
    }

    let lim = 5 - sea.inputs[n].len().max(3);
    for _ in sea.outputs[n].len()..lim {
        sb.push_str("     ");
    }

    sb.push_str(" ]]  ");

    if let Some(ty) = sea.ty[n] {
        write!(sb, "{}", ty.str())?;
    }
    writeln!(sb)
}

fn node_id(sea: &Nodes, n: Node, sb: &mut String) -> fmt::Result {
    write!(sb, "%{n}")?;
    if let Some(p) = n.to_proj(sea) {
        write!(sb, ".{}", sea[p].index)?;
    }
    Ok(())
}

/// Display Type name in a format that's good for IR printer
fn type_name(ty: Ty) -> Cow<str> {
    use Cow::*;
    match *ty {
        Type::Int(i) => match i {
            Int::Bot => Borrowed("IntBot"),
            Int::Top => Borrowed("IntTop"),
            Int::Constant(_) => Borrowed("Int"),
        },
        Type::Tuple { types } => {
            let mut sb = "[".to_string();
            for &t in types {
                sb.push_str(type_name(t).as_ref());
                sb.push(',');
            }
            sb.pop();
            sb.push(']');
            Owned(sb)
        }
        _ => Owned(ty.to_string()),
    }
}

/// Print a node on 1 line, format is inspired by LLVM
/// %id: TYPE = NODE(inputs ....)
/// Nodes as referred to as %id
fn print_line_llvm(nodes: &Nodes, n: Node, sb: &mut String) -> fmt::Result {
    node_id(nodes, n, sb)?;
    write!(sb, ": ")?;
    if nodes.inputs[n].is_empty() {
        return writeln!(sb, "DEAD");
    }
    if let Some(ty) = nodes.ty[n] {
        write!(sb, "{}", type_name(ty))?;
    }

    write!(sb, " = {}(", nodes[n].label())?;
    for (i, &def) in nodes.inputs[n].iter().enumerate() {
        if i > 0 {
            write!(sb, ", ")?;
        }
        if let Some(def) = def {
            node_id(nodes, def, sb)?
        } else {
            write!(sb, "_")?;
        }
    }
    writeln!(sb, ")")
}

impl<'t> Nodes<'t> {
    pub fn is_multi_head(&self, node: Node) -> bool {
        match &self[node] {
            Op::If | Op::Region { .. } | Op::Loop | Op::Start { .. } => true,
            Op::Constant(_)
            | Op::Return
            | Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Minus
            | Op::Scope(_)
            | Op::Bool(_)
            | Op::Not
            | Op::Proj(_)
            | Op::Phi(_)
            | Op::Cast(_)
            | Op::New(_)
            | Op::Mem(_)
            | Op::Stop => false,
        }
    }

    pub fn is_multi_tail(&self, node: Node) -> bool {
        match &self[node] {
            Op::Constant(_) | Op::Phi(_) => true,
            Op::Proj(_) => {
                let ctrl = self.inputs[node][0].unwrap();
                self.is_multi_head(ctrl)
            }
            Op::Return
            | Op::Start { .. }
            | Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Minus
            | Op::Scope(_)
            | Op::Bool(_)
            | Op::Not
            | Op::If
            | Op::Region { .. }
            | Op::Loop
            | Op::Cast(_)
            | Op::New(_)
            | Op::Mem(_)
            | Op::Stop => false,
        }
    }
}

fn post_ord(
    n: Node,
    rpos: &mut Vec<Node>,
    visit: &mut IdSet<Node>,
    bfs: &IdSet<Node>,
    nodes: &Nodes,
) {
    if !bfs.get(n) {
        return; // Not in the BFS visit
    }
    if visit.get(n) {
        return; // Already post-order walked
    }
    visit.add(n);

    // First walk the CFG, then everything
    if nodes.is_cfg(n) {
        for &use_ in &nodes.outputs[n] {
            if use_ != Node::DUMMY
                && nodes.is_cfg(use_)
                && nodes.outputs[use_].len() >= 1
                && !matches!(nodes[nodes.outputs[use_][0]], Op::Loop)
            {
                post_ord(use_, rpos, visit, bfs, nodes);
            }
        }
        for &use_ in &nodes.outputs[n] {
            if use_ != Node::DUMMY && nodes.is_cfg(use_) {
                post_ord(use_, rpos, visit, bfs, nodes);
            }
        }
    }
    for &use_ in &nodes.outputs[n] {
        if use_ != Node::DUMMY {
            post_ord(use_, rpos, visit, bfs, nodes);
        }
    }
    rpos.push(n); // Post-order
}

/// Breadth-first search, broken out in a class to keep in more independent.
/// Maintains a root-set of Nodes at the limit (or past by 1 if MultiHead).
struct BFS {
    /// A breadth first search, plus MultiHeads for any MultiTails
    _bfs: Vec<Node>,
    /// Visited members
    _bs: IdSet<Node>,
    /// Depth limit
    _depth: usize,
    /// From here to _bfs._len can be roots for a reverse search
    _lim: usize,
}

impl BFS {
    fn run<'t>(nodes: &Nodes<'t>, base: Node, mut d: usize) -> Self {
        let mut bfs = Self {
            _bfs: vec![],
            _bs: IdSet::zeros(nodes.len()),
            _depth: d,
            _lim: 0,
        };

        bfs.add(base); // Prime the pump

        let mut idx = 0;
        let mut lim = 1; // Limit is where depth counter changes

        // Ran out of nodes below depth
        while idx < bfs._bfs.len() {
            let n = bfs._bfs[idx];
            idx += 1;

            for &def in nodes.inputs[n].iter().flatten() {
                if !bfs._bs.get(def) {
                    bfs.add(def);
                }
            }
            if idx == lim {
                // Depth counter changes at limit
                if d == 0 {
                    break; // Ran out of depth
                }
                d -= 1;
                lim = bfs._bfs.len(); // New depth limit
            }
        }

        // Toss things past the limit except multi-heads
        while idx < bfs._bfs.len() {
            let n = bfs._bfs[idx];
            if nodes.is_multi_head(n) {
                idx += 1;
            } else {
                bfs.del(idx);
            }
        }

        // Root set is any node with no inputs in the visited set
        lim = bfs._bfs.len();

        for i in (0..bfs._bfs.len()).rev() {
            if !bfs.any_visited(nodes, bfs._bfs[i]) {
                lim -= 1;
                bfs.swap(i, lim);
            }
        }
        bfs._lim = lim;

        bfs
    }

    fn swap(&mut self, x: usize, y: usize) {
        self._bfs.swap(x, y);
    }

    fn add(&mut self, n: Node) {
        self._bfs.push(n);
        self._bs.add(n);
    }

    fn del(&mut self, idx: usize) {
        let n = self._bfs.swap_remove(idx);
        self._bs.remove(n);
    }

    fn any_visited<'t>(&self, nodes: &Nodes<'t>, n: Node) -> bool {
        nodes.inputs[n]
            .iter()
            .flatten()
            .any(|def| self._bs.get(*def))
    }
}
