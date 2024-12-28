use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::index::{Load, Start, Stop, TypedNode};
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

    unreach.insert(sea.start.to_cfg(&sea.ops).unwrap());
    for ret in stop.inputs(sea) {
        ret.unwrap()
            .to_return(sea)
            .unwrap()
            .to_cfg(&sea.ops)
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
    for ret in stop.inputs(sea) {
        ret.unwrap()
            .to_return(sea)
            .unwrap()
            .to_cfg(&sea.ops)
            .unwrap()
            .walk_unreach(&mut visit, &mut unreach, sea);
    }
    assert!(unreach.is_empty());
}

/// Forwards walk over previously unreachable, looking for loops with no
/// exit test.
fn walk_infinite(n: Cfg, visit: &mut IdSet<Node>, stop: Stop, sea: &Nodes) {
    let n = n.node();
    if visit.get(n) {
        return; // Been there, done that
    }
    visit.add(n);
    if let Some(n) = n.to_loop(sea) {
        n.force_exit(stop);
    }
    for use_ in sea.outputs[n] {
        if use_ != Node::DUMMY {
            if let Some(use_) = use_.to_cfg(&sea.ops) {
                walk_infinite(use_, visit, stop, sea);
            }
        }
    }
}

fn sched_early(sea: &mut Nodes) {
    let mut rpo = vec![];
    let mut visit = IdSet::zeros(sea.len());
    _rpo_cfg(*sea.start, &mut visit, &mut rpo, sea);

    // Reverse Post-Order on CFG
    for cfg in rpo.into_iter().rev() {
        cfg.loop_depth();
        for &n in cfg.node().inputs(sea) {
            _sched_early(n, &mut visit, sea);
        }

        // Strictly for dead infinite loops, we can have entire code blocks
        // not reachable from below - so we reach down, from above, one
        // step.  Since _schedEarly modifies the output arrays, the normal
        // region._outputs ArrayList iterator throws CME.  The extra edges
        // are always *added* after any Phis, so just walk the Phi prefix.
        if let Some(region) = cfg.node().to_region(sea) {
            let len = sea.outputs[region].len();
            for i in 0..len {
                if sea.outputs[region][i] != Node::DUMMY {
                    if let Some(phi) = sea.outputs[region][i].to_phi(sea) {
                        _sched_early(Some(*phi), &mut visit, sea);
                    }
                }
            }
        }
    }
}

/// Post-Order of CFG
fn _rpo_cfg(n: Node, visit: &mut IdSet<Node>, rpo: &mut Vec<Cfg>, sea: &Nodes) {
    let Some(cfg) = n.to_cfg(&sea.ops) else {
        return;
    };
    if visit.get(n) {
        return; // Been there, done that
    }
    visit.add(n);
    for &use_ in &sea.outputs[n] {
        _rpo_cfg(use_, visit, rpo, sea);
    }
    rpo.push(cfg);
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
    for &def in n.inputs(sea).iter().flatten() {
        if !def.is_pinned(sea) {
            _sched_early(Some(def), visit, sea);
        }
    }

    // If not-pinned (e.g. constants, projections, phi) and not-CFG
    if !n.is_pinned(sea) {
        // Schedule at deepest input
        let mut early = sea.start.to_cfg(&sea.ops).unwrap(); // Maximally early, lowest idepth
        for i in 1..n.inputs(sea).len() {
            let cfg0 = n.inputs(sea)[i].unwrap().inputs(sea)[0]
                .unwrap()
                .to_cfg(&sea.ops)
                .unwrap();
            if sea[cfg0].idepth > sea[early].idepth {
                early = cfg0; // Latest/deepest input
            }
        }
        n.set_def(0, Some(early.node()), sea); // First place this can go
    }
}

fn sched_late(start: Start, sea: &mut Nodes) {
    let mut late = vec![None; sea.len()];
    let mut ns = vec![None; sea.len()];
    _sched_late(*start, &mut ns, &mut late, sea);
    for i in 0..late.len() {
        if let Some(n) = &ns[i] {
            n.set_def(0, late[i].map(|n| n.node()), sea);
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
    if let Some(cfg) = n.to_cfg(&sea.ops) {
        late[n.index()] = if cfg.block_head() {
            Some(cfg)
        } else {
            cfg.cfg(0, sea)
        }
    }
    if let Some(phi) = n.to_phi(sea) {
        late[n.index()] = Some(phi.region(sea));
    }

    // Walk Stores before Loads, so we can get the anti-deps right
    for &use_ in &sea.outputs[n] {
        if is_forwards_edge(Some(use_), Some(n), sea)
            && matches!(&*use_.ty(sea).unwrap(), Type::Memory(_))
        {
            _sched_late(use_, ns, late, sea);
        }
    }

    // Walk everybody now
    for &use_ in &sea.outputs[n] {
        if is_forwards_edge(Some(use_), Some(n), sea) {
            _sched_late(use_, ns, late, sea);
        }
    }

    // Already implicitly scheduled
    if n.is_pinned() {
        return;
    }

    // Need to schedule n

    // Walk uses, gathering the LCA (Least Common Ancestor) of uses
    let early = n.inputs(sea)[0].unwrap().to_cfg(&sea.ops).unwrap();
    let mut lca = None;
    for use_ in sea.outputs[n] {
        lca = use_block(n, use_, late, sea).idom_2(lca, sea);
    }

    // Loads may need anti-dependencies, raising their LCA
    if let Some(load) = n.to_load(sea) {
        lca = find_anti_dep(lca, load, early, late);
    }

    // Walk up from the LCA to the early, looking for best place.  This is the
    // lowest execution frequency, approximated by least loop depth and
    // deepest control flow.
    let mut best = lca.unwrap();
    lca = best.idom(sea); // Already found best for starting LCA

    while lca != early.idom(sea) {
        if better(lca, best, sea) {
            best = lca.unwrap();
        }
        lca = lca.unwrap().idom(sea);
    }

    assert!(best.node().to_if(sea).is_none());
    ns[n.index()] = Some(n);
    late[n.index()] = Some(best);
}

/// Block of use.  Normally from late[] schedule, except for Phis, which go
/// to the matching Region input.
fn use_block(n: Node, use_: Node, late: &[Cfg], sea: &Nodes) -> Cfg {
    let Some(phi) = use_.to_phi(sea) else {
        return late[n.index()];
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
    sea[lca].loop_depth < sea[best].loop_depth
        || (sea[lca].idepth > sea[best].idepth || best.node().to_if(sea).is_some())
}

/// Skip iteration if a backedge
fn is_forwards_edge(use_: Option<Node>, def: Option<Node>, sea: &Nodes) -> bool {
    let Some(use_) = use_ else { return false };
    let Some(def) = def else { return false };

    let uin = use_.inputs(sea);
    !(uin.len() > 2
        && uin[2] == Some(def)
        && (use_.to_loop(sea).is_some()
            || (use_
                .to_phi(sea)
                .is_some_and(|phi| phi.region(sea).node().to_loop(sea).is_some()))))
}

fn find_anti_dep(lca: Cfg, load: Load, early: Cfg, late: &[Cfg], sea: &mut Nodes) -> Cfg {
    // We could skip final-field loads here.
    // Walk LCA->early, flagging Load's block location choices
    {
        let mut early = Some(early);
        let mut cfg = Some(lca);
        while early.is_some() && cfg != early.unwrap().idom(sea) {
            sea[cfg.unwrap()].anti = Some(load);
            cfg = cfg.unwrap().idom(sea);
        }
    }

    // Walk load->mem uses, looking for Stores causing an anti-dep
    for &mem in &sea.outputs[load.mem(sea).unwrap()] {
        match mem.downcast(&sea.ops) {
            TypedNode::Store(st) => {
                lca = anti_dep(load, late[st.index()], st.cfg0(), lca, st, sea);
            }
            TypedNode::Phi(phi) => {
                // Repeat anti-dep for matching Phi inputs.
                // No anti-dep edges but may raise the LCA.
                for i in 1..phi.inputs(sea).len() {
                    if phi.inputs(sea)[i] == load.mem(sea) {
                        lca = anti_dep(
                            load,
                            phi.region(sea).cfg(i),
                            load.mem(sea).cfg0(),
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
    while stblk != defblk.idom(sea) {
        // Store and Load overlap, need anti-dependence
        if sea[stblk].anti == Some(load) {
            lca = stblk.idom(lca, sea).unwrap(); // Raise Loads LCA
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
