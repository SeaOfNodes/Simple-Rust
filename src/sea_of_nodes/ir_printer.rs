use std::fmt;
use std::fmt::Write;

use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};

/// Another bulk pretty-printer.  Makes more effort at basic-block grouping.
pub fn pretty_print(nodes: &Nodes, node: NodeId, depth: usize) -> String {
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
            print_line(nodes, n, &mut sb).unwrap(); // Print head
            while let Some(&&t) = iter.peek() {
                if nodes.is_multi_tail(t) {
                    break;
                }
                iter.next();
                print_line(nodes, t, &mut sb).unwrap();
            }
            sb.push('\n'); // Blank after multitail
            gap = true;
        } else {
            print_line(nodes, n, &mut sb).unwrap();
            gap = false;
        }
    }
    sb
}

/// Print a node on 1 line, columnar aligned, as:
/// NNID NNAME DDEF DDEF  [[  UUSE UUSE  ]]  TYPE
/// 1234 sssss 1234 1234 1234 1234 1234 1234 tttttt
fn print_line(nodes: &Nodes, n: NodeId, sb: &mut String) -> fmt::Result {
    write!(sb, "{n:4} {: <7.7} ", nodes[n].label())?;

    if nodes.is_dead(n) {
        return writeln!(sb, "DEAD");
    }

    for def in &nodes.inputs[n] {
        if let Some(def) = def {
            write!(sb, "{def:4} ")?;
        } else {
            write!(sb, "____ ")?;
        }
    }
    for _ in nodes.inputs[n].len()..3 {
        sb.push_str("     ");
    }
    sb.push_str(" [[  ");

    for use_ in &nodes.outputs[n] {
        if *use_ != NodeId::DUMMY {
            write!(sb, "{use_:4} ")?;
        } else {
            write!(sb, "____ ")?;
        }
    }

    let lim = 5 - nodes.inputs[n].len().max(3);
    for _ in nodes.outputs[n].len()..lim {
        sb.push_str("     ");
    }

    sb.push_str(" ]]  ");

    if let Some(ty) = nodes.ty[n] {
        write!(sb, "{ty}")?;
    }
    writeln!(sb)
}

impl<'t> Nodes<'t> {
    pub fn is_multi_head(&self, node: NodeId) -> bool {
        match &self[node] {
            Node::If | Node::Region { .. } | Node::Loop | Node::Start { .. } => true,
            Node::Constant(_)
            | Node::Return
            | Node::Add
            | Node::Sub
            | Node::Mul
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Not
            | Node::Proj(_)
            | Node::Phi(_)
            | Node::Stop => false,
        }
    }

    pub fn is_multi_tail(&self, node: NodeId) -> bool {
        match &self[node] {
            Node::Constant(_) | Node::Phi(_) => true,
            Node::Proj(_) => {
                let ctrl = self.inputs[node][0].unwrap();
                self.is_multi_head(ctrl)
            }
            Node::Return
            | Node::Start { .. }
            | Node::Add
            | Node::Sub
            | Node::Mul
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Not
            | Node::If
            | Node::Region { .. }
            | Node::Loop
            | Node::Stop => false,
        }
    }
}

fn post_ord(
    n: NodeId,
    rpos: &mut Vec<NodeId>,
    visit: &mut IdSet<NodeId>,
    bfs: &IdSet<NodeId>,
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
            if use_ != NodeId::DUMMY
                && nodes.is_cfg(use_)
                && nodes.outputs[use_].len() >= 1
                && !matches!(nodes[nodes.outputs[use_][0]], Node::Loop)
            {
                post_ord(use_, rpos, visit, bfs, nodes);
            }
        }
        for &use_ in &nodes.outputs[n] {
            if use_ != NodeId::DUMMY && nodes.is_cfg(use_) {
                post_ord(use_, rpos, visit, bfs, nodes);
            }
        }
    }
    for &use_ in &nodes.outputs[n] {
        if use_ != NodeId::DUMMY {
            post_ord(use_, rpos, visit, bfs, nodes);
        }
    }
    rpos.push(n); // Post-order
}

/// Breadth-first search, broken out in a class to keep in more independent.
/// Maintains a root-set of Nodes at the limit (or past by 1 if MultiHead).
struct BFS {
    /// A breadth first search, plus MultiHeads for any MultiTails
    _bfs: Vec<NodeId>,
    /// Visited members
    _bs: IdSet<NodeId>,
    /// Depth limit
    _depth: usize,
    /// From here to _bfs._len can be roots for a reverse search
    _lim: usize,
}

impl BFS {
    fn run<'t>(nodes: &Nodes<'t>, base: NodeId, mut d: usize) -> Self {
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

    fn add(&mut self, n: NodeId) {
        self._bfs.push(n);
        self._bs.add(n);
    }

    fn del(&mut self, idx: usize) {
        let n = self._bfs.swap_remove(idx);
        self._bs.remove(n);
    }

    fn any_visited<'t>(&self, nodes: &Nodes<'t>, n: NodeId) -> bool {
        nodes.inputs[n]
            .iter()
            .flatten()
            .any(|def| self._bs.get(*def))
    }
}
