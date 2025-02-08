//! The IterPeeps runs after parsing. It iterates the peepholes to a fixed point
//! so that no more peepholes apply.  This should be linear because peepholes rarely
//! (never?)  increase code size.  The graph should monotonically reduce in some
//! dimension, which is usually size.  It might also reduce in e.g. number of
//! MulNodes or Load/Store nodes, swapping out more "expensive" Nodes for cheaper
//! ones.
//!
//! The theoretical overall worklist is mindless just grabbing the next thing and
//! doing it.  If the graph changes, put the neighbors on the worklist.
//!
//! Lather, Rinse, Repeat until the worklist runs dry.
//!
//! The main issues we have to deal with:
//!
//! <ul>
//! <li>Nodes have uses; replacing some set of Nodes with another requires more graph
//!   reworking.  Not rocket science, but it can be fiddly.  Its helpful to have a
//!   small set of graph munging utilities, and the strong invariant that the graph
//!   is stable and correct between peepholes.  In our case `Node.subsume` does
//!   most of the munging, building on our prior stable Node utilities.</li>
//!
//! <li>Changing a Node also changes the graph "neighborhood".  The neighbors need to
//!   be checked to see if THEY can also peephole, and so on.  After any peephole
//!   or graph update we put a Nodes uses and defs on the worklist.</li>
//!
//! <li>Our strong invariant is that for all Nodes, either they are on the worklist
//!   OR no peephole applies.  This invariant is easy to check, although expensive.
//!   Basically the normal "iterate peepholes to a fixed point" is linear, and this
//!   check is linear at each peephole step... so quadratic overall.  Its a useful
//!   assert, but one we can disable once the overall algorithm is stable - and
//!   then turn it back on again when some new set of peepholes is misbehaving.
//!   The code for this is turned on in `IterPeeps.iterate` as `assert
//!   progressOnList(stop);`</li>
//! </ul>

use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::datastructures::random::Random;
use crate::sea_of_nodes::nodes::node::Stop;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};

pub struct IterPeeps {
    work: WorkList,
    pub(crate) mid_assert: bool,
}

impl IterPeeps {
    pub fn new() -> Self {
        Self {
            work: WorkList::with_seed(123),
            mid_assert: false,
        }
    }
    pub fn add(&mut self, node: Node) {
        if node != Node::DUMMY {
            self.work.push(node);
        }
    }
}

impl Node {
    /// Add a node to the list of dependencies.  Only add it if its not an input
    /// or output of this node, that is, it is at least one step away.  The node
    /// being added must benefit from this node being peepholed.
    pub fn add_dep(self, dep: impl Into<Node>, sea: &mut Nodes) -> Node {
        let dep = dep.into();
        // Running peepholes during the big assert cannot have side effects
        // like adding dependencies.
        if sea.iter_peeps.mid_assert {
            return self;
        }
        if !sea.deps[self].contains(&dep)
            && !self.inputs(sea).contains(&Some(dep))
            && !sea.outputs[self].contains(&dep)
        {
            // Not on list and not an immediate neighbor
            sea.deps[self].push(dep);
        }
        self
    }

    /// Move the dependents onto a worklist, and clear for future dependents.
    pub fn move_deps_to_worklist(self, sea: &mut Nodes) {
        for dep in sea.deps[self].drain(..) {
            sea.iter_peeps.add(dep);
        }
    }
}

impl<'t> Nodes<'t> {
    /// Iterate peepholes to a fixed point
    pub fn iterate(&mut self, stop: Stop) {
        debug_assert!(self.progress_on_list(**stop));
        while let Some(n) = self.iter_peeps.work.pop() {
            if n.is_dead(self) {
                continue;
            }

            if let Some(x) = n.peephole_opt(self) {
                if x.is_dead(self) {
                    continue;
                }

                // peepholeOpt can return brand-new nodes, needing an initial type set
                if self.ty[x].is_none() {
                    let ty = x.compute(self);
                    x.set_type(ty, self);
                }

                // Changes require neighbors onto the worklist
                if x != n || !(matches!(&self[x], Op::Constant(_))) {
                    // All outputs of n (changing node) not x (prior existing node).
                    for &z in &self.outputs[n] {
                        self.iter_peeps.add(z);
                    }

                    // Everybody gets a free "go again" in case they didn't get
                    // made in their final form.
                    self.iter_peeps.add(x);

                    // If the result is not self, revisit all inputs (because
                    // there's a new user), and replace in the graph.
                    if x != n {
                        for &z in self.inputs[n].iter().flatten() {
                            self.iter_peeps.add(z);
                        }
                        n.subsume(x, self);
                    }
                }

                // If there are distant neighbors, move to worklist
                n.move_deps_to_worklist(self);
                debug_assert!(self.progress_on_list(**stop)); // Very expensive assert
            }
            if n.is_unused(self) && !n.is_stop(self) {
                n.kill(self); // just plain dead
            }
        }
    }

    pub fn type_check(&mut self, stop: Stop) -> Result<(), String> {
        match self.walk_non_reentrant(**stop, |sea, n| n.err(sea)) {
            Some(err) => Err(err),
            None => Ok(()),
        }
    }

    /// Visit ALL nodes and confirm the invariant:
    ///   Either you are on the WORK worklist OR running `iter()` makes no progress.
    ///
    /// This invariant ensures that no progress is missed, i.e., when the
    /// worklist is empty we have indeed done all that can be done.  To help
    /// with debugging, the {@code assert} is broken out in a place where it is easy to
    /// stop if a change is found.
    ///
    /// Also, the normal usage of `iter()` may attempt peepholes with distance
    /// neighbors and these should fail, but will then try to add dependencies
    /// {@link #Node.addDep} which is a side effect in an assert.  The {@link
    /// #midAssert} is used to stop this side effect.
    fn progress_on_list(&mut self, stop: Node) -> bool {
        self.iter_peeps.mid_assert = true;
        let (old_cnt, old_nop) = (self.iter_cnt, self.iter_nop_cnt);

        let changed = self.walk_non_reentrant(stop, |sea: &mut Self, n| {
            let mut m = n;
            // Types must be forwards, even if on worklist
            if n.compute(sea).isa(n.ty(sea).unwrap(), sea.types)
                && (!n.is_keep(sea) || n.index() <= 6)
            {
                if sea.iter_peeps.work.on(n) {
                    return None;
                }
                m = n.peephole_opt(sea)?;
            }
            println!("BREAK HERE FOR BUG");
            Some(m)
        });

        self.iter_cnt = old_cnt;
        self.iter_nop_cnt = old_nop;

        self.iter_peeps.mid_assert = false;
        changed.is_none()
    }
}

/// Classic WorkList, with a fast add/remove, dup removal, random pull.
/// The Node's nid is used to check membership in the worklist.
pub struct WorkList {
    es: Vec<Node>,
    /// Bit set if node is on WorkList
    on: IdSet<Node>,
    /// For randomizing pull from the WorkList
    r: Random,
    /// Useful stat - how many nodes are processed in the post parse iterative opt
    total_work: usize,
}

impl WorkList {
    pub fn with_seed(seed: u64) -> Self {
        Self {
            es: vec![],
            on: IdSet::zeros(0),
            r: Random::with_seed(seed),
            total_work: 0,
        }
    }

    /// push node if not present
    pub fn push(&mut self, node: Node) {
        if !self.on.get(node) {
            self.on.add(node);
            self.es.push(node);
            self.total_work += 1;
        }
    }

    /// True if Node is on the WorkList
    pub fn on(&self, node: Node) -> bool {
        self.on.get(node)
    }

    /// Removes a random Node from the WorkList
    pub fn pop(&mut self) -> Option<Node> {
        if self.es.is_empty() {
            return None;
        }
        let idx = self.r.next_int(self.es.len() as i32);
        let node = self.es.swap_remove(idx);
        self.on.remove(node);
        Some(node)
    }
}
