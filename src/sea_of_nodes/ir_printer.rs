use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;

use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes, Op};
use crate::sea_of_nodes::types::{Int, Ty, Type};

pub fn pretty_print_llvm(nodes: &Nodes, node: impl Into<Node>, depth: usize) -> String {
    pretty_print_(nodes, node.into(), depth, true)
}

/// Another bulk pretty-printer.  Makes more effort at basic-block grouping.
pub fn pretty_print(nodes: &Nodes, node: impl Into<Node>, depth: usize) -> String {
    if nodes.scheduled {
        pretty_print_scheduled(node.into(), depth, false, nodes).unwrap()
    } else {
        pretty_print_(nodes, node.into(), depth, false)
    }
}

fn pretty_print_(sea: &Nodes, node: Node, depth: usize, llvm_format: bool) -> String {
    // First, a Breadth First Search at a fixed depth.
    let bfs = BFS::run(sea, node, depth);

    // Convert just that set to a post-order
    let mut rpos = Vec::with_capacity(bfs._bfs.len());
    let mut visit = IdSet::zeros(sea.len());

    for i in bfs._lim..bfs._bfs.len() {
        post_ord(bfs._bfs[i], &mut rpos, &mut visit, &bfs._bs, sea);
    }

    // Reverse the post-order walk
    let mut sb = String::new();
    let mut gap = false;

    let mut iter = rpos.iter().rev().peekable();
    while let Some(&n) = iter.next() {
        if n.is_cfg(sea) || sea.is_multi_head(n) {
            if !gap {
                sb.push('\n'); // Blank before multihead
            };
            print_line(sea, n, &mut sb, llvm_format).unwrap(); // Print head
            while let Some(&&t) = iter.peek() {
                if sea.is_multi_tail(t) {
                    break;
                }
                iter.next();
                print_line(sea, t, &mut sb, llvm_format).unwrap();
            }
            sb.push('\n'); // Blank after multitail
            gap = true;
        } else {
            print_line(sea, n, &mut sb, llvm_format).unwrap();
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
    for _ in sea.inputs[n].len()..4 {
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

    let lim = 6 - sea.inputs[n].len().max(4);
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
            Op::If(_) | Op::Region { .. } | Op::Loop | Op::Start { .. } => true,
            Op::Constant(_)
            | Op::XCtrl
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
            | Op::CProj(_)
            | Op::Phi(_)
            | Op::Cast(_)
            | Op::New(_)
            | Op::Load(_)
            | Op::Store(_)
            | Op::Stop => false,
        }
    }

    pub fn is_multi_tail(&self, node: Node) -> bool {
        match &self[node] {
            Op::Constant(_) | Op::XCtrl | Op::Phi(_) => true,
            Op::Proj(_) | Op::CProj(_) => {
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
            | Op::If(_)
            | Op::Region { .. }
            | Op::Loop
            | Op::Cast(_)
            | Op::New(_)
            | Op::Load(_)
            | Op::Store(_)
            | Op::Stop => false,
        }
    }
}

fn post_ord(
    n: Node,
    rpos: &mut Vec<Node>,
    visit: &mut IdSet<Node>,
    bfs: &IdSet<Node>,
    sea: &Nodes,
) {
    if !bfs.get(n) {
        return; // Not in the BFS visit
    }
    if visit.get(n) {
        return; // Already post-order walked
    }
    visit.add(n);

    // First walk the CFG, then everything
    if n.is_cfg(sea) {
        for &use_ in &sea.outputs[n] {
            if use_ != Node::DUMMY
                && use_.is_cfg(sea)
                && sea.outputs[use_].len() >= 1
                && !matches!(sea[sea.outputs[use_][0]], Op::Loop)
            {
                post_ord(use_, rpos, visit, bfs, sea);
            }
        }
        for &use_ in &sea.outputs[n] {
            if use_ != Node::DUMMY && use_.is_cfg(sea) {
                post_ord(use_, rpos, visit, bfs, sea);
            }
        }
    }
    for &use_ in &sea.outputs[n] {
        if use_ != Node::DUMMY {
            post_ord(use_, rpos, visit, bfs, sea);
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

/// Bulk pretty printer, knowing scheduling information is available
fn pretty_print_scheduled(
    node: Node,
    depth: usize,
    llvm_format: bool,
    sea: &Nodes,
) -> Result<String, fmt::Error> {
    // Backwards DFS walk to depth.
    let mut ds = HashMap::new();
    walk_(&mut ds, node, depth, sea);
    // Print by block with least idepth
    let mut sb = String::new();
    let mut bns = vec![];

    while !ds.is_empty() {
        let mut blk: Option<Cfg> = None;
        for &n in ds.keys() {
            let mut cfg = n.to_cfg(&sea.ops);
            if cfg.is_none() || !cfg.unwrap().block_head(sea) {
                cfg = n.inputs(sea)[0].unwrap().to_cfg(&sea.ops);
            }
            let cfg = cfg.unwrap();
            if blk.is_none() || sea[cfg].idepth < sea[blk.unwrap()].idepth {
                blk = Some(cfg);
            }
        }
        let blk = blk.unwrap();
        ds.remove(&blk.node());

        // Print block header
        write!(sb, "{:<13.13}:                     [[  ", blk.label(sea))?;

        if blk.node().is_region(sea) || blk.node().is_loop(sea) || blk.node().is_stop(sea) {
            for i in if blk.node().is_stop(sea) { 0 } else { 1 }..blk.node().inputs(sea).len() {
                label(&mut sb, blk.cfg(i, sea).unwrap(), sea)?
            }
        } else if !blk.node().is_start(sea) {
            label(&mut sb, blk.cfg(0, sea).unwrap(), sea)?;
        }
        sb += " ]]  \n";

        // Collect block contents that are in the depth limit
        bns.clear();
        let mut xd = usize::MAX;
        for &use_ in &sea.outputs[blk.node()] {
            if let Some(i) = ds.get(&use_) {
                if !use_.to_cfg(&sea.ops).is_some_and(|cfg| cfg.block_head(sea)) {
                    bns.push(use_);
                    xd = xd.min(*i);
                }
            }
        }

        // Print Phis up front, if any
        {
            let mut i = 0;
            while i < bns.len() {
                if let Some(phi) = bns[i].to_phi(sea) {
                    print_line_2(*phi, &mut sb, llvm_format, &mut bns, i, &mut ds, sea)?;
                    i -= 1;
                }
                i += 1;
            }
        }

        // Print block contents in depth order, bumping depth until whole block printed
        while !bns.is_empty() {
            let mut i = 0;
            while i < bns.len() {
                if ds.get(&bns[i]) == Some(&xd) {
                    print_line_2(bns[i], &mut sb, llvm_format, &mut bns, i, &mut ds, sea)?;
                    i -= 1;
                }
                i += 1;
            }
            xd += 1;
        }

        sb += "\n";
    }
    Ok(sb)
}

fn walk_(ds: &mut HashMap<Node, usize>, node: Node, d: usize, sea: &Nodes) {
    let nd = ds.get(&node);
    if nd.is_some_and(|&nd| d <= nd) {
        return; // Been there, done that
    }
    ds.insert(node, d);
    if d == 0 {
        return; // Depth cutoff
    }
    for def in node.inputs(sea) {
        if let Some(def) = def {
            walk_(ds, *def, d - 1, sea);
        }
    }
}

impl Cfg {
    fn label(self, sea: &Nodes) -> Cow<'static, str> {
        if self.node().is_start(sea) {
            Cow::Borrowed("START")
        } else if self.node().is_loop(sea) {
            Cow::Borrowed("LOOP")
        } else {
            Cow::Owned(format!("L{}", self.node()))
        }
    }
}

fn label(sb: &mut String, mut blk: Cfg, sea: &Nodes) -> fmt::Result {
    if blk.block_head(sea) {
        blk = blk.cfg(0, sea).unwrap();
    }
    write!(sb, "{:<9.9} ", blk.label(sea))
}

fn print_line_2(
    n: Node,
    sb: &mut String,
    llvm_format: bool,
    bns: &mut Vec<Node>,
    i: usize,
    ds: &mut HashMap<Node, usize>,
    sea: &Nodes,
) -> fmt::Result {
    print_line(sea, n, sb, llvm_format)?;
    bns.swap_remove(i);
    ds.remove(&n);
    Ok(())
}
