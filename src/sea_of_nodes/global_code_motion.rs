use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::{CProj, If, Load, Loop, Return, Start, Stop, TypedNode};
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes};
use crate::sea_of_nodes::types::Type;
use std::collections::HashSet;

/// Arrange that the existing isCFG() Nodes form a valid CFG.  The
/// Node.use(0) is always a block tail (either IfNode or head of the
/// following block).  There are no unreachable infinite loops.
pub fn build_cfg(stop: Stop, sea: &mut Nodes) {
    fix_loops(stop, sea);
    sched_early(sea);
    sea.scheduled = true;
    sched_late(sea.start, sea);
}

/// Backwards walk on the CFG only, looking for unreachable code - which has
/// to be an infinite loop.  Insert a bogus never-taken exit to Stop, so the
/// loop becomes reachable.  Also, set loop nesting depth
fn fix_loops(stop: Stop, sea: &mut Nodes) {
    // Backwards walk from Stop, looking for unreachable code
    let mut visit = IdSet::zeros(sea.len());
    let mut unreach = HashSet::with_capacity(sea.len());

    unreach.insert(sea.start.to_cfg());
    for ret in 0..stop.inputs(sea).len() {
        let ret = stop.inputs(sea)[ret];
        ret.unwrap()
            .to_return(sea)
            .unwrap()
            .walk_unreach(&mut visit, &mut unreach, sea);
    }
    if unreach.is_empty() {
        return;
    }

    // Forwards walk from unreachable, looking for loops with no exit test.
    visit.clear();
    for &cfg in &unreach {
        walk_infinite(cfg, &mut visit, stop, sea);
    }
    // Set loop depth on remaining graph
    unreach.clear();
    visit.clear();
    for ret in 0..stop.inputs(sea).len() {
        let ret = stop.inputs(sea)[ret];
        ret.unwrap()
            .to_return(sea)
            .unwrap()
            .walk_unreach(&mut visit, &mut unreach, sea);
    }
    assert!(unreach.is_empty());
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
            if x.is_cproj(sea) {
                return; // Found an exit, not an infinite loop
            }
            x = x.idom(sea).unwrap()
        }

        // Found a no-exit loop.  Insert an exit
        let iff = If::new(*self.back(sea), None, sea);
        for i in 0..sea.outputs[self].len() {
            let use_ = sea.outputs[self][i];
            if use_ != Node::DUMMY {
                if let Some(phi) = use_.to_phi(sea) {
                    iff.add_def(Some(*phi), sea);
                }
            }
        }

        let t = CProj::new(**iff, 0, "True", sea);
        let f = CProj::new(**iff, 1, "False", sea);
        self.set_def(2, Some(**f), sea);
        stop.add_def(Some(**Return::new(**t, *sea.zero, None, sea)), sea);
    }
}

/// Forwards walk over previously unreachable, looking for loops with no
/// exit test.
fn walk_infinite(n: Cfg, visit: &mut IdSet<Node>, stop: Stop, sea: &mut Nodes) {
    let n = *n;
    if visit.get(n) {
        return; // Been there, done that
    }
    visit.add(n);
    if let Some(n) = n.to_loop(sea) {
        n.force_exit(stop, sea);
    }
    for i in 0..sea.outputs[n].len() {
        let use_ = sea.outputs[n][i];
        if use_ != Node::DUMMY {
            if let Some(use_) = use_.to_cfg(sea) {
                walk_infinite(use_, visit, stop, sea);
            }
        }
    }
}

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

        // Strictly for dead infinite loops, we can have entire code blocks
        // not reachable from below - so we reach down, from above, one
        // step.  Since _schedEarly modifies the output arrays, the normal
        // region._outputs ArrayList iterator throws CME.  The extra edges
        // are always *added* after any Phis, so just walk the Phi prefix.
        if cfg.is_region(sea) {
            let len = sea.outputs[cfg].len();
            for i in 0..len {
                if sea.outputs[cfg][i] != Node::DUMMY {
                    if let Some(phi) = sea.outputs[cfg][i].to_phi(sea) {
                        _sched_early(Some(*phi), &mut visit, sea);
                    }
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
    visit.add(n);

    // Schedule not-pinned not-CFG inputs before self.  Since skipping
    // Pinned, this never walks the backedge of Phis (and thus spins around
    // a data-only loop, eventually attempting relying on some pre-visited-
    // not-post-visited data op with no scheduled control.
    for i in 0..n.inputs(sea).len() {
        if let Some(def) = n.inputs(sea)[i] {
            if !def.is_pinned(sea) {
                _sched_early(Some(def), visit, sea);
            }
        }
    }

    // If not-pinned (e.g. constants, projections, phi) and not-CFG
    if !n.is_pinned(sea) {
        // Schedule at deepest input
        let mut early = sea.start.to_cfg(); // Maximally early, lowest idepth
        for i in 1..n.inputs(sea).len() {
            let cfg0 = n.inputs(sea)[i].unwrap().cfg0(sea);
            if sea.cfg[cfg0].idepth > sea.cfg[early].idepth {
                early = cfg0; // Latest/deepest input
            }
        }
        n.set_def(0, Some(*early), sea); // First place this can go
    }
}

fn sched_late(start: Start, sea: &mut Nodes) {
    let mut late = vec![None; sea.len()];
    let mut ns = vec![None; sea.len()];
    _sched_late(start.to_node(), &mut ns, &mut late, sea);
    for i in 0..late.len() {
        if let Some(n) = &ns[i] {
            n.set_def(0, late[i].map(|n| *n), sea);
        }
    }
}

impl Cfg {
    pub(crate) fn block_head(self, sea: &Nodes) -> bool {
        match self.downcast(&sea.ops) {
            TypedNode::Start(_)
            | TypedNode::CProj(_)
            | TypedNode::Region(_)
            | TypedNode::Stop(_) => true,
            _ => false,
        }
    }
}

/// Forwards post-order pass.  Schedule all outputs first, then draw an
/// idom-tree line from the LCA of uses to the early schedule.  Schedule is
/// legal anywhere on this line; pick the most control-dependent (largest
/// idepth) in the shallowest loop nest.
fn _sched_late(n: Node, ns: &mut Vec<Option<Node>>, late: &mut Vec<Option<Cfg>>, sea: &mut Nodes) {
    if late[n.index()].is_some() {
        return; // Been there, done that
    }

    // These0 I know the late schedule of, and need to set early for loops
    if let Some(cfg) = n.to_cfg(sea) {
        late[n.index()] = if cfg.block_head(sea) {
            Some(cfg)
        } else {
            cfg.cfg(0, sea)
        }
    }
    if let Some(phi) = n.to_phi(sea) {
        late[n.index()] = Some(phi.region(sea));
    }

    // Walk Stores before Loads, so we can get the anti-deps right
    for i in 0..sea.outputs[n].len() {
        let use_ = sea.outputs[n][i];
        if use_ != Node::DUMMY {
            if is_forwards_edge(Some(use_), Some(n), sea)
                && use_.ty(sea).is_some_and(|t| matches!(&*t, Type::Memory(_)))
            {
                _sched_late(use_, ns, late, sea);
            }
        }
    }

    // Walk everybody now
    for i in 0..sea.outputs[n].len() {
        let use_ = sea.outputs[n][i];
        if use_ != Node::DUMMY {
            if is_forwards_edge(Some(use_), Some(n), sea) {
                _sched_late(use_, ns, late, sea);
            }
        }
    }

    // Already implicitly scheduled
    if n.is_pinned(sea) {
        return;
    }

    // Need to schedule n

    // Walk uses, gathering the LCA (Least Common Ancestor) of uses
    let early = n.cfg0(sea);
    let mut lca = None;
    for i in 0..sea.outputs[n].len() {
        let use_ = sea.outputs[n][i];
        if use_ != Node::DUMMY {
            lca = Some(use_block(n, use_, late, sea).idom_2(lca, sea));
        }
    }

    // Loads may need anti-dependencies, raising their LCA
    if let Some(load) = n.to_load(sea) {
        lca = Some(find_anti_dep(lca.unwrap(), load, early, late, sea));
    }

    // Walk up from the LCA to the early, looking for best place.  This is the
    // lowest execution frequency, approximated by least loop depth and
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
fn better(lca: Cfg, best: Cfg, sea: &Nodes) -> bool {
    sea.cfg[lca].loop_depth < sea.cfg[best].loop_depth
        || (sea.cfg[lca].idepth > sea.cfg[best].idepth || best.is_if(sea))
}

/// Skip iteration if a backedge
fn is_forwards_edge(use_: Option<Node>, def: Option<Node>, sea: &Nodes) -> bool {
    let Some(use_) = use_ else { return false };
    let Some(def) = def else { return false };

    let uin = use_.inputs(sea);
    !(uin.len() > 2
        && uin[2] == Some(def)
        && (use_.is_loop(sea)
            || (use_
                .to_phi(sea)
                .is_some_and(|phi| phi.region(sea).is_loop(sea)))))
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
            lca = stblk.idom_2(Some(lca), sea); // Raise Loads LCA
            if let Some(st) = st {
                if lca == stblk && !st.inputs(sea).iter().any(|n| *n == Some(*load)) {
                    // And if something moved,
                    st.add_def(Some(*load), sea); // Add anti-dep as well
                }
            }
            return lca; // Cap this stores' anti-dep to here
        }
        stblk = stblk.idom(sea).unwrap()
    }
    lca
}
