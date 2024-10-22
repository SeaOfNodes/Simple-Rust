//!
//! A late scheduler for test cases.
//! <br>
//! <br>
//! This scheduler is used for tests and is not the final scheduler.
//! <br>
//! <br>
//! The scheduler schedules not as late as possible. Since it is just for test cases this is fine.
//! A better scheduler will try to place nodes outside of loops. Due to the schedule late strategy
//! this scheduler might place initialization code in loops.
//! <br>
//! We schedule late here since memory edges have some implicit information. There can only be one
//! memory edge (for a field) at a time. However, before <code>if</code> statements this might split.
//! When scheduling early a store might be moved out of the loop but then there are two edges alive.
//! The one through the store node and the adjacent one. This is an invalid schedule and instead of
//! handling this case a late scheduler does not have this problem as memory edges are combined
//! through phi nodes.
//! <br>
//! <br>
//! A bit about this scheduler. It first discovers all alive nodes and allocates side data for them.
//! Then the control flow graph is build. Then, all phis are placed and finally all the remaining nodes
//! in an order where a node is placed when all alive uses are placed. Finally, the output is produced.

use crate::sea_of_nodes::nodes::index::{Phi, Start, TypedNode};
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use std::collections::HashMap;

/// A basic block with a schedule of the containing nodes.
pub struct Block {
    /// The nodes in the final schedule of this block.
    pub nodes: Vec<Node>,
    /// The exit node of this block.
    pub exit: Option<Node>,
    /// In case the next block is a region this gives the entry id into the region.
    pub exit_id: Option<usize>,
    /// The next blocks.
    pub next: Vec<BlockId>,
}


pub type BlockId = usize;
type BasicBlockId = usize;

/// Definition of a block in the building process.
struct BasicBlock {
    /// Dominator of this block
    dom: Option<BasicBlockId>,

    /// Id of this block where `a.depth < b.depth` means that `a` is before `b` or
    /// both are in different branches.
    depth: BasicBlockId,

    /// Previous blocks
    prev: Vec<BasicBlockId>,

    /// Entry to this block
    entry: Node,

    /// Schedules node in reverse order.
    reverse_schedule: Vec<Node>,
}
impl BasicBlock {
    //          * Initialize the block.
    //          * - entry The node that started this block
    //          * - depth The depth of this node. All previous nodes need to have a lower value.
    //          * - prev The previous blocks.
    fn create(entry: Node, prev: Vec<BasicBlockId>, scheduler: &mut Scheduler) -> BasicBlockId {
        let mut dom = None;
        let depth = scheduler.blocks.len();
        for &bb in &prev {
            debug_assert!(depth > scheduler.blocks[bb].depth);
            if let Some(d) = dom {
                dom = Some(scheduler.dom(d, bb));
            } else {
                dom = Some(bb);
            }
        }
        scheduler.blocks.push(Self {
            dom,
            depth,
            prev,
            entry,
            reverse_schedule: vec![],
        });
        depth
    }
}

/// Ancillary data for nodes used during the scheduling process.
struct NodeData {
    /// The node this ancillary data is used for.
    node: Node,
    /// Number of alive users not yet scheduled.
    users: usize,
    /// Block into which the node should be placed so far.
    /// This will be updated with every placed use.
    block: Option<BasicBlockId>,
}

impl NodeData {
    /// Initialize ancillary data for a node
    ///
    /// - `node` The node this ancillary data is associated with.
    pub fn new(node: Node) -> Self {
        Self {
            node,
            users: 1,
            block: None,
        }
    }
}

struct Scheduler {
    blocks: Vec<BasicBlock>,

    /// Ancillary data for nodes.
    data: HashMap<Node, NodeData>,
    /// List of nodes which can be placed since all users are placed.
    schedule_stack: Vec<Node>,
}

impl Scheduler {
    /// Checks a node for validity
    /// - `data` The node to check.
    /// - returns: true if all placed inputs are before this node.
    fn is_valid(&self, node: Node, sea: &Nodes) -> bool {
        let data = &self.data[&node];
        for &i in data.node.inputs(sea) {
            if let Some(i) = i {
                let d = &self.data[&i];
                if d.users == 0 && d.block.is_some_and(|d| self.dom(d, data.block.unwrap()) != d) {
                    return false;
                }
            }
        }
        true
    }

    /// Checks that node is not XCtrl.
    /// - `node` The node to check.
    /// - returns: true if the node is not XCtrl.
    fn is_not_xctrl(node: Node, sea: &Nodes) -> bool {
        node.to_constant(sea).is_some_and(|c| sea[c] == sea.types.ty_xctrl)
    }

    // Return the dominator of <code>a</code> and <code>b</code>
    // - `a` Block a
    // - `b` Block b
    // - returns: The dominator of <code>a</code> and <code>b</code>
    fn dom(&self, mut a: BasicBlockId, mut b: BasicBlockId) -> BasicBlockId {
        while a != b {
            let a_ = &self.blocks[a];
            let b_ = &self.blocks[b];

            if a_.depth >= b_.depth { a = a_.dom.unwrap() }
            if b_.depth > a_.depth { b = b_.dom.unwrap() }
        }
        a
    }

    /// Return if a is before b.
    /// - `a` Block a
    /// - `b` Block b
    /// - returns: true is a is before b.
    fn is_before(&self, a: BasicBlockId, mut b: BasicBlockId) -> bool {
        while self.blocks[b].depth > self.blocks[a].depth {
            while self.blocks[self.blocks[b].dom.unwrap()].depth > self.blocks[a].depth {
                b = self.blocks[b].dom.unwrap();
            }
            let mut iter = self.blocks[b].prev.iter();
            b = *iter.next_back().unwrap();
            for &p in iter {
                if self.is_before(a, p) {
                    return true;
                }
            }
        }
        a == b
    }

    // Is node placed during the control flow graph build.
    // - `data` Node to check.
    // - returns: true if node is placed during the control flow graph build.
    fn is_pinned_node(data: &NodeData, sea: &Nodes) -> bool {
        data.node.is_cfg(sea) || data.node.to_phi(sea).is_some()
    }

    // Refine placement of a node.
    // - `data` The node to refine.
    // - block The block before the node should be scheduled.
    fn refine_placement(&mut self, node: Node, block: BasicBlockId, sea: &Nodes) {
        let data = self.data.get(&node).unwrap();
        debug_assert!(!Self::is_pinned_node(data, sea));
        let new = Some(data.block.map(|b| self.dom(b, block)).unwrap_or(block));
        self.data.get_mut(&node).unwrap().block = new;
        debug_assert!(self.is_valid(node, sea));
    }

    // Refine placement if before is happening before data
    // - `data` The node for which the placement should be refined.
    // - before The node before which the data node should be placed.
    fn optional_refine_placement(&mut self, node: Node, before: Node, sea: &Nodes) {
        if let Some(b) = self.data.get(&before).map(|d| d.block) {
            // before might be in a different branch. So check this case with isBefore
            // and only refine the placement if it is not in a different branch.
            if self.is_before(b.unwrap(), self.data[&node].block.unwrap()) {
                self.refine_placement(node, b.unwrap(), sea);
            }
        }
    }

    /// Schedule all nodes not yet scheduled.
    fn do_schedule(&mut self, sea: &Nodes) {
        while let Some(next) = self.schedule_stack.pop() {
            let data = &self.data[&next];
            debug_assert!(Self::is_pinned_node(data, sea));

            debug_assert_ne!(data.block, None);
            debug_assert_eq!(data.users, 0);

            if next.is_load(sea) {
                debug_assert!(self.is_valid(next, sea));

                // Handle anti-deps of load nodes.
                // At this point all anti-dep nodes are scheduled,
                // but they did not refine the placement
                // so do that now.
                let mut mem = next.inputs(sea)[1].unwrap();
                for out in &sea.outputs[mem] {
                    if let Some(p) = out.to_phi(sea) {
                        let r = p.inputs(sea)[0].unwrap();
                        for i in 0..p.inputs(sea).len() {
                            if p.inputs(sea)[i] == Some(mem) {
                                self.optional_refine_placement(next, r.inputs(sea)[i].unwrap(), sea);
                            }
                        }
                    } else if !out.is_load(sea) {
                        self.optional_refine_placement(next, *out, sea);
                    }
                }
            }

            debug_assert!(self.is_valid(next, sea));

            self.blocks[self.data[&next].block.unwrap()].reverse_schedule.push(next);

            for &i in next.inputs(sea).iter().flatten() {
                self.update(i, self.data[&next].block.unwrap(), sea);
            }

            if next.is_store(sea) {
                // Store nodes have anti-deps to load nodes.
                // So decrease the uses of these loads when the store is placed.
                for &o in &sea.outputs[next.inputs(sea)[1].unwrap()] {
                    if o.is_load(sea) && self.data.contains_key(&o) {
                        self.dec_users(o, sea);
                    }
                }
            }
        }

        // Now all nodes should be placed and have a block assigned
        debug_assert!(self.data.values().into_iter().all(|d| d.block.is_some()));
    }

    /// Decrement the not-yet-placed users of a block.
    /// - data The node for which the users should be decremented.
    fn dec_users(&mut self, node: Node, sea: &Nodes) {
        let data = self.data.get_mut(&node).unwrap();
        debug_assert!(data.users > 0);
        data.users -= 1;
        if data.users == 0 {
            debug_assert_ne!(data.block, None);
            debug_assert!(self.is_valid(node, sea));

            // When all users are gone this node can be scheduled.
            self.schedule_stack.push(node);
        }
    }

    /// Update the placement of a node
    /// - `data` The node to update
    /// - block The block before which the node should happen.
    fn update(&mut self, node: Node, block: BasicBlockId, sea: &Nodes) {
        let data = self.data.get_mut(&node).unwrap();
        if data.users == 0 {
            debug_assert!(Self::is_pinned_node(data, sea));
            return;
        }
        self.refine_placement(node, block, sea);
        self.dec_users(node, sea);
    }

    /// Checks if a CFG node is ready to be placed. This is used for regions joining branches.
    /// - `node` The CFG node to check
    /// - returns: true if all parents of a CFG node are placed.
    fn is_cfg_node_ready(&self, node: Node, sea: &Nodes) -> bool {
        if node.is_load(sea) {
            debug_assert_ne!(self.data[&node].block, None)
        } else if let Some(r) = node.to_region(sea) {
            for i in r.inputs(sea).iter().skip(1).flatten() {
                if self.data.get(i).is_some_and(|d| d.block.is_none()) {
                    return false;
                }
            }
        } else {
            debug_assert_ne!(self.data[&node.inputs(sea)[0].unwrap()].block, None)
        }
        true
    }

    /// Schedule all phi nodes.
    /// - `phi_queue` List of all the phi nodes to schedule.
    fn schedule_phis(&mut self, mut phi_queue: Vec<Phi>, sea: &Nodes) {
        while let Some(phi) = phi_queue.pop() {
            let r = phi.inputs(sea)[0].unwrap();
            self.data.get_mut(&phi).unwrap().block = self.data.get(&r).unwrap().block;
            for (phi_in, r_in) in phi.inputs(sea).iter().zip(r.inputs(sea).iter()).skip(1) {
                if let Some(r_in) = r_in {
                    if let Some(d) = self.data.get(r_in) {
                        self.update(phi_in.unwrap(), d.block.unwrap(), sea);
                    }
                }
            }
        }
    }

    /// Build the control flow graph.
    /// - `start` The start node.
    fn do_build_ctf(&mut self, start: Start, sea: &Nodes) {
        let mut queue = vec![*start];
        let mut phi_queue = vec![];

        while let Some(node) = queue.pop() {
            debug_assert!(node.is_cfg(sea));

            let block = match &sea[node] {
                Op::Start(_) => BasicBlock::create(node, vec![], self),
                Op::Loop => BasicBlock::create(node, vec![self.data[&node.inputs(sea)[1].unwrap()].block.unwrap()], self),
                Op::Region => {
                    let prev = node.inputs(sea).iter().skip(1).flatten().map(|n| self.data[n].block.unwrap()).collect();
                    BasicBlock::create(node, prev, self)
                }
                Op::If | Op::Return => self.data[&node.inputs(sea)[0].unwrap()].block.unwrap(),
                _ => {
                    let b = self.data[&node.inputs(sea)[0].unwrap()].block.unwrap();
                    BasicBlock::create(node, vec![b], self)
                }
            };

            self.data.get_mut(&node).unwrap().block = Some(block);

            if let Some(r) = node.to_region(sea) {
                // Regions might have phis which need to be scheduled.
                // Put them on a list for later scheduling.
                for p in sea.outputs[r].iter().flat_map(|o| o.to_phi(sea)) {
                    if let Some(d) = self.data.get_mut(&p) {
                        d.users = 0;
                        phi_queue.push(p);
                    }
                }
            }

            if node.to_return(sea).is_none() {
                for &n in &sea.outputs[node] {
                    if n.is_cfg(sea) && self.is_cfg_node_ready(n, sea) && self.data[&n].block.is_none() {
                        queue.push(n);
                    }
                }
            }

            for in_ in node.inputs(sea).iter().flatten() {
                if let Some(d) = self.data.get(in_) {
                    self.update(*in_, block, sea)
                }
            }
        }

        self.schedule_phis(phi_queue, sea);
    }

    /// Mark a node alive and create ancillary data for it.
    /// - `queue` List into which this node should be placed if it needs to be visited.
    /// - `node` Node which should be marked alive.
    /// - `cfg` If this node is a CFG node.
    fn mark_alive(&mut self, queue: &mut Vec<Node>, node: Node, cfg: bool, sea: &Nodes) {
        if let Some(nd) = self.data.get_mut(&node) {
            // Node was already visited, just increase the users.
            if nd.users > 0 {
                nd.users += 1;
            }
            return;
        }
        debug_assert_eq!(node.is_cfg(sea), cfg);
        debug_assert!(Self::is_not_xctrl(node, sea));

        let mut nd = NodeData::new(node);
        if cfg {
            nd.users = 0;
        }
        self.data.insert(node, nd);
        queue.push(node);
    }

    /// Mark all alive nodes.
    /// First all CFG nodes are marked, then all other nodes.
    /// - `node` THe start node.
    fn do_mark_alive(&mut self, node: Node, sea: &Nodes) {
        let mut cfg_queue = vec![];
        let mut data_queue = vec![];
        let mut mem = vec![];

        self.mark_alive(&mut cfg_queue, node, true, sea);

        // Mark all CFG nodes.
        while let Some(node) = cfg_queue.pop() {
            debug_assert!(node.is_cfg(sea));
            if node.to_return(sea).is_none() {
                for &out in &sea.outputs[node] {
                    if out.is_cfg(sea) && Self::is_not_xctrl(out, sea) {
                        self.mark_alive(&mut cfg_queue, out, true, sea);
                    }
                }
            }
            for &i in node.inputs(sea).iter().flatten() {
                if !i.is_cfg(sea) && Self::is_not_xctrl(i, sea) {
                    self.mark_alive(&mut data_queue, i, false, sea);
                }
            }
        }

        // Mark all other nodes.
        while let Some(node) = data_queue.pop() {
            debug_assert!(!node.is_cfg(sea));
            if let Some(phi) = node.to_phi(sea) {
                let r = phi.inputs(sea)[0].unwrap();
                for (i, in_) in phi.inputs(sea).iter().enumerate().skip(1) {
                    if r.inputs(sea)[i].is_some_and(|ri| self.data.contains_key(&ri)) {
                        self.mark_alive(&mut data_queue, in_.unwrap(), false, sea);
                    }
                }
            } else {
                for &i in node.inputs(sea).iter().flatten() {
                    if !i.is_cfg(sea) {
                        self.mark_alive(&mut data_queue, i, false, sea);
                    }
                }
            }
            if node.is_store(sea) {
                mem.push(node);
            }
        }

        // Handle store nodes and increase load with an anti-dep to the store.
        while let Some(node) = mem.pop() {
            for &out in &sea.outputs[node] {
                if out.is_load(sea) {
                    if let Some(d) = self.data.get_mut(&out) {
                        d.users += 1;
                    }
                }
            }
        }
    }

    /// Helper function to append the nodes of a block to an array of nodes
    /// - `arr` The array the nodes of a block should be appended to.
    /// - `node` A potentially separator node
    /// - `block` The block from which the nodes should be appended.
    /// - returns: The combined new array.
    fn append_nodes(&self, arr: &mut Vec<Node>, node: Option<Node>, block: BasicBlockId) {
        if let Some(node) = node {
            arr.push(node);
        }
        arr.extend(self.blocks[block].reverse_schedule.iter().rev());
    }

    /// Find the CFG output of a node
    /// - `node` The node
    /// - returns: THe CFG output of the node.
    fn find_single_cfg_out(node: Node, sea: &Nodes) -> Option<Node> {
        if node.to_start(sea).is_some() {
            return sea.outputs[node].iter().copied().find(|n| n.to_proj(sea).is_some_and(|p| sea[p].index == 0));
        }
        let mut iter = sea.outputs[node].iter().filter(|n| n.is_cfg(sea));
        let result = iter.next().copied();
        debug_assert_eq!(iter.next(), None);
        result
    }

    /// Build the final data structure after all the scheduling happened.
    /// - `start` The start node
    /// - returns: The final schedule.
    fn build(&mut self, start: Node, sea: &Nodes) -> (Vec<Block>, BlockId) {
        let mut block_data = vec![];
        let mut blocks = HashMap::<Node, BlockId>::new();
        let mut queue = vec![];
        queue.push(start);

        // Visit all CFG nodes and create blocks for them.
        // This can combine blocks.
        while let Some(first) = queue.pop() {
            let mut last = Some(first);
            let mut arr = vec![];
            if first.to_region(sea).is_some() {
                arr.extend(sea.outputs[first].iter().filter(|o| o.to_phi(sea).is_some_and(|p| self.data.contains_key(&*p))));
            }

            self.append_nodes(&mut arr, None, self.data[&first].block.unwrap());
            debug_assert!(first.to_if(sea).is_none());
            let mut prev;
            loop {
                prev = last;
                last = Self::find_single_cfg_out(last.unwrap(), sea);
                let Some(last) = last else { break };

                if last.to_region(sea).is_some() {
                    //                     if (blocks.get(last) == null && last != first)
                    //                         queue.push(d(last));
                    //                     break;
                }
                if last.to_return(sea).is_some() {
                    break;
                }
                if last.to_if(sea).is_some() {
                    queue.extend(sea.outputs[last].iter().filter(|o| o.is_cfg(sea) && !blocks.contains_key(*o)));
                    break;
                }
                self.append_nodes(&mut arr, Some(last), self.data[&last].block.unwrap());
            }
            let (exit_id, next) = match last.map(|l| l.downcast(&sea.ops)) {
                None => (None, vec![]),
                Some(TypedNode::If(i)) => (None, vec![0; 2]),
                Some(TypedNode::Region(r)) => (r.inputs(sea).iter().position(|p| *p == prev), vec![0; 1]),
                Some(TypedNode::Return(r)) => (None, vec![]),
                Some(n) => unreachable!("Unexpected block exit node {n:?}")
            };
            blocks.insert(first, block_data.len());
            block_data.push(Block { nodes: arr, exit: last, exit_id, next });
        }

        // Update the next pointer of all blocks.
        for block in block_data.iter_mut() {
            match block.exit {
                None => {}
                Some(n) if n.to_if(sea).is_some() => {
                    for p in sea.outputs[n].iter().filter_map(|n| n.to_proj(sea)) {
                        block.next[sea[p].index] = blocks[&*p];
                    }
                }
                Some(n) if n.to_region(sea).is_some() => {
                    block.next[0] = blocks[&n];
                }
                _ => debug_assert!(block.exit.unwrap().to_return(sea).is_some()),
            }
        }
        // And return the block for the start node.
        (block_data, blocks[&start])
    }

}

/// Create a schedule for the program reachable from start.
/// - `start` The start node
/// - returns: The final schedule.
pub fn schedule(start: Start, sea: &Nodes) -> (Vec<Block>, BlockId) {
    let mut scheduler = Scheduler {
        blocks: vec![],
        data: Default::default(),
        schedule_stack: vec![],
    };
    scheduler.do_mark_alive(*start, sea);
    scheduler.do_build_ctf(start, sea);
    scheduler.do_schedule(sea);
    scheduler.build(*start, sea)
}
