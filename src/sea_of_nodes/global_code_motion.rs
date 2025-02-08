use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::{IfOp, Load, Stop, TypedNode};
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes, WorkList};

impl Stop {
    pub fn gcm(self, sea: &mut Nodes) {
        sea.start.build_loop_tree(self, sea);
        // TODO show here?
        build_cfg(self, sea);
    }
}

/// Arrange that the existing isCFG() Nodes form a valid CFG.  The
/// Node.use(0) is always a block tail (either IfNode or head of the
/// following block).  There are no unreachable infinite loops.
pub fn build_cfg(stop: Stop, sea: &mut Nodes) {
    sched_early(sea);
    sea.scheduled = true;
    sched_late(stop, sea);
}

/// Visit all nodes in CFG Reverse Post-Order, essentially defs before uses
/// (except at loops).  Since defs are visited first - and hoisted as early
/// as possible, when we come to a use we place it just after its deepest
/// input.
fn sched_early(sea: &mut Nodes) {
    let mut rpo = vec![];
    let mut visit = IdSet::zeros(sea.len());
    _rpo_cfg(sea.start.to_node(), &mut visit, &mut rpo, sea);

    // Reverse Post-Order on CFG
    for cfg in rpo.into_iter().rev() {
        cfg.loop_depth(sea);
        for i in 0..cfg.inputs(sea).len() {
            let n = cfg.inputs(sea)[i];
            _sched_early(n, &mut visit, sea);
        }
        if cfg.is_region(sea) {
            for i in 0..sea.outputs[cfg].len() {
                let phi = sea.outputs[cfg][i];
                if phi != Node::DUMMY && phi.is_phi(sea) {
                    _sched_early(Some(phi), &mut visit, sea);
                }
            }
        }
    }
}

/// Post-Order of CFG
fn _rpo_cfg(n: Node, visit: &mut IdSet<Node>, rpo: &mut Vec<Cfg>, sea: &Nodes) {
    let Some(cfg) = n.to_cfg(sea) else {
        return;
    };
    if visit.get(n) {
        return; // Been there, done that
    }
    visit.add(n);
    for &use_ in &sea.outputs[n] {
        if use_ != Node::DUMMY {
            _rpo_cfg(use_, visit, rpo, sea);
        }
    }
    rpo.push(cfg);
}

impl Node {
    // Pinned in the schedule
    fn is_pinned(self, sea: &Nodes) -> bool {
        match self.downcast(&sea.ops) {
            TypedNode::Constant(_) => self == *sea.zero,
            TypedNode::Cast(_) | TypedNode::Phi(_) | TypedNode::Proj(_) => true,
            _ => self.is_cfg(sea),
        }
    }
}

fn _sched_early(n: Option<Node>, visit: &mut IdSet<Node>, sea: &mut Nodes) {
    let Some(n) = n else { return };
    if visit.get(n) {
        return; // Been there, done that
    }
    debug_assert!(!n.is_cfg(sea));
    visit.add(n);

    // Schedule not-pinned not-CFG inputs before self.  Since skipping
    // Pinned, this never walks the backedge of Phis (and thus spins around
    // a data-only loop, eventually attempting relying on some pre-visited-
    // not-post-visited data op with no scheduled control.
    for i in 0..n.inputs(sea).len() {
        if let Some(def) = n.inputs(sea)[i] {
            if !def.is_phi(sea) {
                _sched_early(Some(def), visit, sea);
            }
        }
    }

    // If not-pinned (e.g. constants, projections, phi) and not-CFG
    if !n.is_pinned(sea) {
        // Schedule at deepest input
        let mut early = sea.start.to_cfg(); // Maximally early, lowest idepth
        if let Some(cfg) = n.inputs(sea)[0].and_then(|n| n.to_cfg(sea)) {
            early = cfg;
        }
        for i in 1..n.inputs(sea).len() {
            let cfg0 = n.inputs(sea)[i].unwrap().cfg0(sea);
            if sea.cfg[cfg0].idepth > sea.cfg[early].idepth {
                early = cfg0; // Latest/deepest input
            }
        }
        n.set_def(0, *early, sea); // First place this can go
    }
}

fn sched_late(stop: Stop, sea: &mut Nodes) {
    let mut late = vec![None; sea.len() + 1];
    let mut ns = vec![None; sea.len() + 1];
    // Breadth-first scheduling
    breadth(stop, &mut ns, &mut late, sea);

    // Copy the best placement choice into the control slot
    for i in 0..late.len() {
        if let Some(n) = &ns[i] {
            if !n.is_proj(sea) {
                n.set_def(0, late[i].map(|n| *n), sea);
            }
        }
    }
}

impl Cfg {
    pub(crate) fn block_head(self, sea: &Nodes) -> bool {
        matches!(
            self.downcast(&sea.ops),
            TypedNode::Start(_) | TypedNode::CProj(_) | TypedNode::Region(_) | TypedNode::Stop(_)
        )
    }
}

fn breadth(stop: Stop, ns: &mut Vec<Option<Node>>, late: &mut Vec<Option<Cfg>>, sea: &mut Nodes) {
    // Things on the worklist have some (but perhaps not all) uses done.
    let mut work = WorkList::with_seed(123);
    work.push(**stop);

    'outer: while let Some(n) = work.pop() {
        debug_assert_eq!(late[n.index()], None, "No double visit");
        // These I know the late schedule of, and need to set early for loops
        if let Some(cfg) = n.to_cfg(sea) {
            late[n.index()] = if cfg.block_head(sea) {
                Some(cfg)
            } else {
                cfg.cfg(0, sea)
            };
        } else if let Some(phi) = n.to_phi(sea) {
            late[n.index()] = Some(phi.region(sea));
        } else if n.is_proj(sea) && n.inputs(sea)[0].unwrap().is_cfg(sea) {
            late[n.index()] = n.inputs(sea)[0].unwrap().to_cfg(sea);
        } else {
            // All uses done?
            for &use_ in &sea.outputs[n] {
                if use_ != Node::DUMMY && late[use_.index()].is_none() {
                    continue 'outer; // Nope, await all uses done
                }
            }
            // Loads need their memory inputs' uses also done
            if let Some(ld) = n.to_load(sea) {
                for &memuse in &sea.outputs[ld.mem(sea).unwrap()] {
                    if late[memuse.index()].is_none() {
                        if memuse.ty(sea).unwrap().is_mem() {
                            continue 'outer; // Load-use directly defines memory
                        }
                        if let Some(tt) = memuse.ty(sea).unwrap().to_tuple() {
                            if tt.data()[sea[ld].alias as usize].is_mem() {
                                continue 'outer; // Load-use indirectly defines memory
                            }
                        }
                    }
                }
            }

            // All uses done, schedule
            do_sched_late(n, ns, late, sea);
        }

        // Walk all inputs and put on worklist, as their last-use might now be done
        for &def in n.inputs(sea).iter().flatten() {
            if late[def.index()].is_none() {
                work.push(def);
                // if the def has a load use, maybe the load can fire
                for &ld in &sea.outputs[def] {
                    if ld != Node::DUMMY && ld.is_load(sea) && late[ld.index()].is_none() {
                        work.push(ld);
                    }
                }
            }
        }
    }
}

fn do_sched_late(
    n: Node,
    ns: &mut Vec<Option<Node>>,
    late: &mut Vec<Option<Cfg>>,
    sea: &mut Nodes,
) {
    // Walk uses, gathering the LCA (Least Common Ancestor) of uses
    let early = {
        let i = n.inputs(sea)[0].unwrap();
        i.to_cfg(sea).unwrap_or_else(|| i.cfg0(sea))
    };
    let mut lca = None;
    for i in 0..sea.outputs[n].len() {
        let use_ = sea.outputs[n][i];
        if use_ != Node::DUMMY {
            lca = Some(use_block(n, use_, late, sea).idom_2(lca, None, sea));
        }
    }

    // Loads may need anti-dependencies, raising their LCA
    if let Some(load) = n.to_load(sea) {
        lca = Some(find_anti_dep(lca.unwrap(), load, early, late, sea));
    }

    // Walk up from the LCA to the early, looking for best place.  This is
    // the lowest execution frequency, approximated by least loop depth and
    // deepest control flow.
    let mut best = lca.unwrap();
    lca = best.idom(sea); // Already found best for starting LCA

    while lca != early.idom(sea) {
        if better(lca.unwrap(), best, sea) {
            best = lca.unwrap();
        }
        lca = lca.unwrap().idom(sea);
    }

    assert!(!best.is_if(sea));
    ns[n.index()] = Some(n);
    late[n.index()] = Some(best);
}

/// Block of use.  Normally from late[] schedule, except for Phis, which go
/// to the matching Region input.
fn use_block(n: Node, use_: Node, late: &[Option<Cfg>], sea: &Nodes) -> Cfg {
    let Some(phi) = use_.to_phi(sea) else {
        return late[use_.index()].unwrap();
    };
    let mut found = None;
    for i in 1..phi.inputs(sea).len() {
        if phi.inputs(sea)[i] == Some(n) {
            if found.is_none() {
                found = phi.region(sea).cfg(i, sea);
            } else {
                todo!("Can be more than once")
            }
        }
    }
    found.unwrap()
}

/// Least loop depth first, then largest idepth
fn better(lca: Cfg, best: Cfg, sea: &mut Nodes) -> bool {
    lca.loop_depth(sea) < best.loop_depth(sea)
        || lca.idepth(sea) > best.idepth(sea)
        || best.is_if(sea)
}

impl Node {
    fn cfg0(self, sea: &Nodes) -> Cfg {
        self.inputs(sea)[0].unwrap().to_cfg(sea).unwrap()
    }
}

fn find_anti_dep(
    mut lca: Cfg,
    load: Load,
    early: Cfg,
    late: &[Option<Cfg>],
    sea: &mut Nodes,
) -> Cfg {
    // We could skip final-field loads here.
    // Walk LCA->early, flagging Load's block location choices
    {
        let early = Some(early);
        let mut cfg = Some(lca);
        while early.is_some() && cfg != early.unwrap().idom(sea) {
            sea.cfg[cfg.unwrap()].anti = Some(load);
            cfg = cfg.unwrap().idom(sea);
        }
    }

    // Walk load->mem uses, looking for Stores causing an anti-dep
    for i in 0..sea.outputs[load.mem(sea).unwrap()].len() {
        let mem = sea.outputs[load.mem(sea).unwrap()][i];
        if mem == Node::DUMMY {
            continue;
        }
        match mem.downcast(&sea.ops) {
            TypedNode::Store(st) => {
                lca = anti_dep(
                    load,
                    late[st.index()].unwrap(),
                    st.cfg0(sea),
                    lca,
                    Some(*st),
                    sea,
                );
            }
            TypedNode::New(st) => {
                lca = anti_dep(
                    load,
                    late[st.index()].unwrap(),
                    st.cfg0(sea),
                    lca,
                    Some(*st),
                    sea,
                );
            }
            TypedNode::Phi(phi) => {
                // Repeat anti-dep for matching Phi inputs.
                // No anti-dep edges but may raise the LCA.
                for i in 1..phi.inputs(sea).len() {
                    if phi.inputs(sea)[i] == load.mem(sea) {
                        lca = anti_dep(
                            load,
                            phi.region(sea).cfg(i, sea).unwrap(),
                            load.mem(sea).unwrap().cfg0(sea),
                            lca,
                            None,
                            sea,
                        );
                    }
                }
            }
            TypedNode::Load(_) => {
                // Loads do not cause anti-deps on other loads
            }
            TypedNode::Return(_) => {
                // Load must already be ahead of Return
            }
            TypedNode::ScopeMin(_) => {
                // Mem uses now on ScopeMin
            }
            TypedNode::If(i) if sea[i] == IfOp::Never => {}
            x => todo!("unexpected node type {x:?}"),
        }
    }

    lca
}

fn anti_dep(
    load: Load,
    mut stblk: Cfg,
    defblk: Cfg,
    mut lca: Cfg,
    st: Option<Node>,
    sea: &mut Nodes,
) -> Cfg {
    // Walk store blocks "reach" from its scheduled location to its earliest
    while Some(stblk) != defblk.idom(sea) {
        // Store and Load overlap, need anti-dependence
        if sea.cfg[stblk].anti == Some(load) {
            lca = stblk.idom_2(Some(lca), None, sea); // Raise Loads LCA
            if let Some(st) = st {
                if lca == stblk && !st.inputs(sea).iter().any(|n| *n == Some(*load)) {
                    // And if something moved,
                    st.add_def(*load, sea); // Add anti-dep as well
                }
            }
            return lca; // Cap this stores' anti-dep to here
        }
        stblk = stblk.idom(sea).unwrap()
    }
    lca
}
