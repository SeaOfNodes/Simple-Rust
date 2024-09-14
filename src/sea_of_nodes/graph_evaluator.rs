use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::{NodeId, Nodes, Op, ProjOp};
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq)]
pub enum EResult {
    Value(i64),
    Fallthrough,
    Timeout,
}

pub fn evaluate(
    nodes: &Nodes,
    graph: impl Into<NodeId>,
    parameter: Option<i64>,
    loops: Option<usize>,
) -> i64 {
    let parameter = parameter.unwrap_or(0);
    let loops = loops.unwrap_or(1000);

    let res = evaluate_with_result(nodes, graph.into(), parameter, loops);

    match res {
        EResult::Value(v) => v,
        EResult::Fallthrough => 0,
        EResult::Timeout => panic!("timeout"),
    }
}

pub fn evaluate_with_result(nodes: &Nodes, graph: NodeId, parameter: i64, loops: usize) -> EResult {
    let mut visited = IdSet::zeros(nodes.len());
    let Some(start) = nodes.find_start(&mut visited, Some(graph)) else {
        return EResult::Timeout;
    };
    let mut evaluator = GraphEvaluator::new(nodes);
    evaluator.evaluate(start, parameter, loops)
}

/// Find the start node from some node in the graph or null if there is no start node
impl<'t> Nodes<'t> {
    fn find_start(&self, visit: &mut IdSet<NodeId>, node: Option<NodeId>) -> Option<NodeId> {
        let node = node?;
        if let Op::Start { .. } = &self[node] {
            return Some(node);
        }
        if visit.get(node) {
            return None;
        }
        visit.add(node);
        for &def in &self.inputs[node] {
            if let res @ Some(_) = self.find_start(visit, def) {
                return res;
            }
        }
        for &use_ in &self.outputs[node] {
            if let res @ Some(_) = self.find_start(visit, Some(use_)) {
                return res;
            }
        }
        None
    }

    /// Find the control output from a control node
    fn find_control(&self, control: NodeId) -> Option<NodeId> {
        self.outputs[control]
            .iter()
            .find(|&&use_| self.is_cfg(use_))
            .copied()
    }

    /// Find the projection for a node
    fn find_projection(&self, node: NodeId, idx: usize) -> Option<NodeId> {
        self.outputs[node]
            .iter()
            .find(|&&use_| matches!(&self[use_], Op::Proj(ProjOp {index, .. }) if *index == idx))
            .copied()
    }
}

struct GraphEvaluator<'a, 't> {
    nodes: &'a Nodes<'t>,

    /// Cache values for phi and parameter projection nodes.
    cache_values: HashMap<NodeId, i64>,
    /// Cache for loop phis as they can depend on itself or other loop phis
    loop_phi_cache: Vec<i64>,
}

impl<'a, 't> GraphEvaluator<'a, 't> {
    pub fn new(nodes: &'a Nodes<'t>) -> Self {
        Self {
            nodes,
            cache_values: HashMap::new(),
            loop_phi_cache: Vec::with_capacity(16),
        }
    }

    fn div(&mut self, div: NodeId) -> i64 {
        let in2 = self.get_value(self.nodes.inputs[div][2]);
        if in2 == 0 {
            0
        } else {
            self.get_value(self.nodes.inputs[div][1]) / in2
        }
    }

    fn binary<F: FnOnce(i64, i64) -> i64>(&mut self, node: NodeId, op: F) -> i64 {
        let a = self.get_value(self.nodes.inputs[node][1]);
        let b = self.get_value(self.nodes.inputs[node][2]);
        op(a, b)
    }

    /// Calculate the value of a node
    fn get_value(&mut self, node: Option<NodeId>) -> i64 {
        let Some(node) = node else {
            panic!("cannot evaluate None")
        };
        if let Some(&cache) = self.cache_values.get(&node) {
            return cache;
        }
        match &self.nodes[node] {
            Op::Constant(c) => c.unwrap_int(),
            Op::Add => self.binary(node, i64::wrapping_add),
            Op::Sub => self.binary(node, i64::wrapping_sub),
            Op::Mul => self.binary(node, i64::wrapping_mul),
            Op::Div => self.div(node),
            Op::Minus => self.get_value(self.nodes.inputs[node][1]).wrapping_neg(),
            Op::Bool(op) => self.binary(node, |a, b| op.compute(a, b) as i64),
            Op::Not => {
                if self.get_value(self.nodes.inputs[node][1]) == 0 {
                    1
                } else {
                    0
                }
            }
            n => todo!("unexpected node type {}", n.label()),
        }
    }

    /// Special case of latchPhis when phis can depend on phis of the same region.
    fn latch_loop_phis(&mut self, region: NodeId, prev: NodeId) {
        let idx = self.nodes.inputs[region]
            .iter()
            .position(|n| *n == Some(prev))
            .unwrap();
        debug_assert!(idx > 0);
        self.loop_phi_cache.clear();
        for &use_ in &self.nodes.outputs[region] {
            if let Op::Phi(_) = &self.nodes[use_] {
                let value = self.get_value(self.nodes.inputs[use_][idx]);
                self.loop_phi_cache.push(value);
            }
        }
        let mut d = self.loop_phi_cache.drain(..);
        for &use_ in &self.nodes.outputs[region] {
            if let Op::Phi(_) = &self.nodes[use_] {
                self.cache_values.insert(use_, d.next().unwrap());
            }
        }
        debug_assert_eq!(d.next(), None);
    }

    /// Calculate the values of phis of the region and caches the values. The phis are not allowed to depend on other phis of the region.
    fn latch_phis(&mut self, region: NodeId, prev: NodeId) {
        let idx = self.nodes.inputs[region]
            .iter()
            .position(|n| *n == Some(prev))
            .unwrap();
        debug_assert!(idx > 0);
        for &use_ in &self.nodes.outputs[region] {
            if let Op::Phi(_) = &self.nodes[use_] {
                let value = self.get_value(self.nodes.inputs[use_][idx]);
                self.cache_values.insert(use_, value);
            }
        }
    }

    /// Run the graph until either a return is found or the number of loop iterations are done.
    fn evaluate(&mut self, start: NodeId, parameter: i64, mut loops: usize) -> EResult {
        assert!(matches!(&self.nodes[start], Op::Start { .. }));

        if let Some(parameter1) = self.nodes.find_projection(start, 1) {
            self.cache_values.insert(parameter1, parameter);
        }

        let mut control = self.nodes.find_projection(start, 0);
        let mut prev = start;
        while let Some(c) = control {
            let next = match &self.nodes[c] {
                Op::Region { .. } | Op::Loop => {
                    if matches!(&self.nodes[c], Op::Loop) && self.nodes.inputs[c][1] != Some(prev) {
                        if loops == 0 {
                            return EResult::Timeout;
                        }
                        loops -= 1;
                        self.latch_loop_phis(c, prev);
                    } else {
                        self.latch_phis(c, prev)
                    }
                    self.nodes.find_control(c)
                }
                Op::If => self.nodes.find_projection(
                    c,
                    if self.get_value(self.nodes.inputs[c][1]) != 0 {
                        0
                    } else {
                        1
                    },
                ),
                Op::Return => return EResult::Value(self.get_value(self.nodes.inputs[c][1])),
                Op::Proj(_) => self.nodes.find_control(c),
                n => todo!("unexpected control node {}", n.label()),
            };
            prev = c;
            control = next;
        }
        EResult::Fallthrough
    }
}
