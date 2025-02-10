use crate::datastructures::id::Id;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{CProj, If, Load, Loop, Return, Stop};
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes, Op, Start};
use std::num::NonZeroU32;

/// Control Flow Graph Nodes
///
///  CFG nodes have a immediate dominator depth (idepth) and a loop nesting
///  depth(loop_depth).
pub struct CfgData {
    /// Immediate dominator tree depth, used to approximate a real IDOM during
    /// parsing where we do not have the whole program, and also peepholes
    /// change the CFG incrementally.
    ///
    /// See <a href="https://en.wikipedia.org/wiki/Dominator_(graph_theory)">Wikipedia: Dominator</a>
    pub idepth: u32,
    /// Anti-dependence field support
    pub anti: Option<Load>,
    ltree: Option<LoopTree>,
    pre: u32,
}
impl CfgData {
    pub fn new() -> Self {
        Self {
            idepth: 0,
            anti: None,
            ltree: None,
            pre: 0,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct LoopTree(NonZeroU32);
impl Id for LoopTree {
    fn index(&self) -> usize {
        self.0.get() as usize
    }
}

pub struct LoopTreeData {
    par: Option<LoopTree>,
    head: Loop,
    depth: u32,
}
impl LoopTreeData {
    pub const DUMMY: Self = Self {
        par: None,
        head: Loop::DUMMY,
        depth: 0,
    };
}

impl LoopTree {
    fn depth(self, data: &mut IdVec<LoopTree, LoopTreeData>) -> u32 {
        if data[self].depth == 0 {
            if let Some(par) = data[self].par {
                data[self].depth = par.depth(data) + 1;
            }
        }
        data[self].depth
    }
}

impl Cfg {
    pub fn cfg(self, idx: usize, sea: &Nodes) -> Option<Cfg> {
        self.inputs(sea)[idx].map(|n| n.to_cfg(sea).unwrap())
    }

    pub fn idepth(self, sea: &mut Nodes) -> u32 {
        if sea.cfg[self].idepth != 0 {
            return sea.cfg[self].idepth;
        }
        let d = match sea[*self] {
            Op::Start(_) => return 0, // uncached
            Op::Stop | Op::Region => (0..self.inputs(sea).len())
                .filter_map(|i| self.cfg(i, sea).map(|d| d.idepth(sea)))
                .max()
                .unwrap(),
            _ => self.idom(sea).unwrap().idepth(sea),
        } + 1;
        sea.cfg[self].idepth = d;
        d
    }

    pub fn idom(self, sea: &mut Nodes) -> Option<Cfg> {
        self.idom_dep(None, sea)
    }

    /// Return the immediate dominator of this Node and compute dom tree depth.
    pub fn idom_dep(self, dep: Option<Node>, sea: &mut Nodes) -> Option<Cfg> {
        match sea[*self] {
            Op::Loop => self.cfg(1, sea),
            Op::Region => {
                let mut lca = None;
                // Walk the LHS & RHS idom trees in parallel until they match, or either fails.
                // Because this does not cache, it can be linear in the size of the program.
                for i in 1..self.inputs(sea).len() {
                    lca = Some(self.cfg(i, sea).unwrap().idom_2(lca, dep, sea));
                }
                lca
            }
            Op::Start(_) | Op::Stop => None,
            _ => self.cfg(0, sea),
        }
    }

    /// Return the LCA of two idoms
    pub fn idom_2(self, rhs: Option<Cfg>, dep: Option<Node>, sea: &mut Nodes) -> Cfg {
        let Some(mut rhs) = rhs else {
            return self;
        };
        let mut lhs = self;
        while lhs != rhs {
            let comp = lhs.idepth(sea) as i64 - rhs.idepth(sea) as i64;
            if comp >= 0 {
                if let Some(dep) = dep {
                    lhs.add_dep(dep, sea);
                }
                lhs = lhs.idom(sea).unwrap()
            }
            if comp <= 0 {
                if let Some(dep) = dep {
                    rhs.add_dep(dep, sea);
                }
                rhs = rhs.idom(sea).unwrap()
            }
        }
        lhs
    }

    pub fn loop_depth(self, sea: &mut Nodes) -> u32 {
        sea.cfg[self]
            .ltree
            .map(|l| l.depth(&mut sea.loop_tree))
            .unwrap_or(0)
    }
}

impl Start {
    /// ------------------------------------------------------------------------
    /// Tag all CFG Nodes with their containing LoopNode; LoopNodes themselves
    /// also refer to *their* containing LoopNode, as well as have their depth.
    /// Start is a LoopNode which contains all at depth 1.
    pub fn build_loop_tree(self, stop: Stop, sea: &mut Nodes) {
        let l = LoopTree(NonZeroU32::try_from(sea.loop_tree.len() as u32).unwrap());
        sea.loop_tree.push(LoopTreeData {
            par: None,
            head: *self,
            depth: 0,
        });

        sea.cfg[***self].ltree = Some(l);
        sea.cfg[*stop].ltree = Some(l);
        sea.cfg[*sea.xctrl].ltree = Some(l);

        let mut post = IdVec::new(vec![false; sea.len()]);
        self.blt_walk(2, stop, &mut post, sea);
    }
}
impl Cfg {
    fn blt_walk(
        self,
        mut pre: u32,
        stop: Stop,
        post: &mut IdVec<Cfg, bool>,
        sea: &mut Nodes,
    ) -> u32 {
        // Pre-walked?
        if sea.cfg[self].pre != 0 {
            return pre;
        }
        sea.cfg[self].pre = pre;
        pre += 1;

        // Pre-walk
        for use_ in 0..sea.outputs[self].len() {
            let use_ = sea.outputs[self][use_];
            if use_ != Node::DUMMY {
                if let Some(usecfg) = use_.to_cfg(sea) {
                    pre = usecfg.blt_walk(pre, stop, post, sea);
                }
            }
        }

        // Post-order work: find innermost loop
        let mut innner = None;

        for use_ in 0..sea.outputs[self].len() {
            let use_ = sea.outputs[self][use_];
            if use_ == Node::DUMMY {
                continue;
            }
            let Some(usecfg) = use_.to_cfg(sea) else {
                continue;
            };
            // Child visited but not post-visited?
            let mut ltree: LoopTree;
            if !post[usecfg] {
                // Must be a backedge to a LoopNode then
                ltree = LoopTree(NonZeroU32::try_from(sea.loop_tree.len() as u32).unwrap());
                sea.loop_tree.push(LoopTreeData {
                    par: None,
                    head: usecfg.to_loop(sea).unwrap(),
                    depth: 0,
                });
                sea.cfg[usecfg].ltree = Some(ltree);
            } else {
                // Take child's loop choice, which must exist
                ltree = sea.cfg[usecfg].ltree.unwrap();

                // If falling into a loop, use the target loop's parent instead
                if **sea.loop_tree[ltree].head == usecfg {
                    if sea.loop_tree[ltree].par.is_none() {
                        // This loop never had an If test choose to take its
                        // exit, i.e. it is a no-exit infinite loop.
                        sea.loop_tree[ltree].head.force_exit(stop, sea);
                        sea.loop_tree[ltree].par = sea.cfg[*stop].ltree;
                    }
                    ltree = sea.loop_tree[ltree].par.unwrap();
                }
            }
            // Sort inner loops.  The decision point is some branch far removed
            // from either loop head OR either backedge so requires pre-order
            // numbers to figure out innermost.
            let Some(i) = innner else {
                innner = Some(ltree);
                continue;
            };
            if i == ltree {
                continue; // No change
            }
            let (outer, i2) = if sea.cfg[**sea.loop_tree[ltree].head].pre
                > sea.cfg[**sea.loop_tree[i].head].pre
            {
                (i, ltree)
            } else {
                (ltree, i)
            };
            innner = Some(i2);
            sea.loop_tree[i2].par = Some(outer);
        }

        // Set selected loop
        if innner.is_some() {
            sea.cfg[self].ltree = innner;
        }

        // Tag as post-walked
        post[self] = true;
        pre
    }
}

impl Loop {
    /// If this is an unreachable loop, it may not have an exit.  If it does not
    /// (i.e., infinite loop), force an exit to make it reachable.
    fn force_exit(self, stop: Stop, sea: &mut Nodes) {
        // Walk the backedge, then immediate dominator tree util we hit this
        // Loop again.  If we ever hit a CProj from an If (as opposed to
        // directly on the If) we found our exit.
        let mut x = self.back(sea);
        while x != self.to_cfg() {
            if let Some(exit) = x.to_cproj(sea) {
                if let Some(iff) = exit.inputs(sea)[0].unwrap().to_if(sea) {
                    let other = iff.cproj(1 - sea[exit].index, sea).unwrap();
                    if other.loop_depth(sea) < self.loop_depth(sea) {
                        return;
                    }
                }
            }
            x = x.idom(sea).unwrap()
        }

        // Found a no-exit loop.  Insert an exit
        let iff = If::new(*self.back(sea), None, sea);
        for i in 0..sea.outputs[self].len() {
            let use_ = sea.outputs[self][i];
            if use_ != Node::DUMMY {
                if let Some(phi) = use_.to_phi(sea) {
                    iff.add_def(*phi, sea);
                }
            }
        }

        let t = CProj::new(**iff, 0, "True", sea);
        let f = CProj::new(**iff, 1, "False", sea);
        self.set_def(2, f.to_node(), sea);
        stop.add_def(**Return::new(**t, *sea.zero, None, sea), sea);
    }
}
