use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::index::{Load, Start, Stop};
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes};
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
            .walk_unreach(&mut visit, &mut unreach);
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
            .walk_unreach(&mut visit, &mut unreach);
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
    //         CFGNode[] late = new CFGNode[Node.UID()];
    //         Node[] ns = new Node[Node.UID()];
    //         _schedLate(start,ns,late);
    //         for( int i=0; i<late.length; i++ )
    //             if( ns[i] != null )
    //                 ns[i].setDef(0,late[i]);
}

/// Forwards post-order pass.  Schedule all outputs first, then draw an
/// idom-tree line from the LCA of uses to the early schedule.  Schedule is
/// legal anywhere on this line; pick the most control-dependent (largest
/// idepth) in the shallowest loop nest.
fn _sched_late(n: Node, ns: &[Node], late: &[Cfg], sea: &Nodes) {
    //         if( late[n._nid]!=null ) return; // Been there, done that
    //         // These I know the late schedule of, and need to set early for loops
    //         if( n instanceof CFGNode cfg ) late[n._nid] = cfg.blockHead() ? cfg : cfg.cfg(0);
    //         if( n instanceof PhiNode phi ) late[n._nid] = phi.region();
    //
    //         // Walk Stores before Loads, so we can get the anti-deps right
    //         for( Node use : n._outputs )
    //             if( isForwardsEdge(use,n) &&
    //                 use._type instanceof TypeMem )
    //                 _schedLate(use,ns,late);
    //         // Walk everybody now
    //         for( Node use : n._outputs )
    //             if( isForwardsEdge(use,n) )
    //                 _schedLate(use,ns,late);
    //         // Already implicitly scheduled
    //         if( n.isPinned() ) return;
    //         // Need to schedule n
    //
    //         // Walk uses, gathering the LCA (Least Common Ancestor) of uses
    //         CFGNode early = (CFGNode)n.in(0);
    //         assert early != null;
    //         CFGNode lca = null;
    //         for( Node use : n._outputs )
    //             lca = use_block(n,use, late).idom(lca);
    //
    //         // Loads may need anti-dependencies, raising their LCA
    //         if( n instanceof LoadNode load )
    //             lca = find_anti_dep(lca,load,early,late);
    //
    //         // Walk up from the LCA to the early, looking for best place.  This is the
    //         // lowest execution frequency, approximated by least loop depth and
    //         // deepest control flow.
    //         CFGNode best = lca;
    //         lca = lca.idom();       // Already found best for starting LCA
    //         for( ; lca != early.idom(); lca = lca.idom() )
    //             if( better(lca,best) )
    //                 best = lca;
    //         assert !(best instanceof IfNode);
    //         ns  [n._nid] = n;
    //         late[n._nid] = best;
    //     }
}

/// Block of use.  Normally from late[] schedule, except for Phis, which go
/// to the matching Region input.
fn use_block(n: Node, use_: Node, late: &[Cfg], sea: &Nodes) -> Cfg {
    //     private static CFGNode use_block(Node n, Node use, CFGNode[] late) {
    //         if( !(use instanceof PhiNode phi) )
    //             return late[use._nid];
    //         CFGNode found=null;
    //         for( int i=1; i<phi.nIns(); i++ )
    //             if( phi.in(i)==n )
    //                 if( found==null ) found = phi.region().cfg(i);
    //                 else Utils.TODO(); // Can be more than once
    //         assert found!=null;
    //         return found;
    //     }
}

/// Least loop depth first, then largest idepth
fn better(lca: Cfg, best: Cfg, sea: &Nodes) -> bool {
    sea[lca].loop_depth < sea[best].loop_depth
        || (sea[lca].idepth > sea[best].idepth || best.node().to_if(sea).is_some())
}

/// Skip iteration if a backedge
fn is_forwards_edge(use_: Node, def: Node, sea: &Nodes) {
    //         return use != null && def != null &&
    //             !(use.nIns()>2 && use.in(2)==def && (use instanceof LoopNode || (use instanceof PhiNode phi && phi.region() instanceof LoopNode)));
}

fn find_anti_dep(lca: Cfg, load: Load, early: Cfg, late: &[Cfg]) -> Cfg {
    //         // We could skip final-field loads here.
    //         // Walk LCA->early, flagging Load's block location choices
    //         for( CFGNode cfg=lca; early!=null && cfg!=early.idom(); cfg = cfg.idom() )
    //             cfg._anti = load._nid;
    //         // Walk load->mem uses, looking for Stores causing an anti-dep
    //         for( Node mem : load.mem()._outputs ) {
    //             switch( mem ) {
    //             case StoreNode st:
    //                 lca = anti_dep(load,late[st._nid],st.cfg0(),lca,st);
    //                 break;
    //             case PhiNode phi:
    //                 // Repeat anti-dep for matching Phi inputs.
    //                 // No anti-dep edges but may raise the LCA.
    //                 for( int i=1; i<phi.nIns(); i++ )
    //                     if( phi.in(i)==load.mem() )
    //                         lca = anti_dep(load,phi.region().cfg(i),load.mem().cfg0(),lca,null);
    //                 break;
    //             case LoadNode ld: break; // Loads do not cause anti-deps on other loads
    //             case ReturnNode ret: break; // Load must already be ahead of Return
    //             default: throw Utils.TODO();
    //             }
    //         }
    //         return lca;
}

fn anti_dep(load: Load, stblk: Cfg, defblk: Cfg, lca: Cfg, st: Node, sea: &Nodes) {
    //         // Walk store blocks "reach" from its scheduled location to its earliest
    //         for( ; stblk != defblk.idom(); stblk = stblk.idom() ) {
    //             // Store and Load overlap, need anti-dependence
    //             if( stblk._anti==load._nid ) {
    //                 lca = stblk.idom(lca); // Raise Loads LCA
    //                 if( lca == stblk && st != null && Utils.find(st._inputs,load) == -1 ) // And if something moved,
    //                     st.addDef(load);   // Add anti-dep as well
    //                 return lca;            // Cap this stores' anti-dep to here
    //             }
    //         }
    //         return lca;
}
