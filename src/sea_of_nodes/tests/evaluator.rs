use crate::datastructures::id_set::IdSet;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::index::{Constant, Div, Mem, New, Start, TypedNode};
use crate::sea_of_nodes::nodes::{BoolOp, MemOpKind, Node, Nodes};
use crate::sea_of_nodes::tests::scheduler;
use crate::sea_of_nodes::tests::scheduler::{Block, BlockId};
use crate::sea_of_nodes::types::{Int, TyStruct, Type};
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Object {
    Long(i64),
    Null,
    Memory,
    Obj(usize),
}

#[derive(Debug)]
pub struct Obj<'t> {
    pub ty: TyStruct<'t>,
    pub fields: Vec<Object>,
}

#[derive(Debug)]
pub struct Heap<'t> {
    pub objs: Vec<Obj<'t>>,
}

pub struct PrintableObject<'o, 't> {
    pub object: Object,
    pub heap: &'o Heap<'t>,
}

impl<'a, 't> Display for PrintableObject<'a, 't> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.object {
            Object::Long(l) => l.fmt(f),
            Object::Null => "null".fmt(f),
            Object::Memory => "memory".fmt(f),
            Object::Obj(obj) => {
                let obj = &self.heap.objs[obj];
                writeln!(f, "Obj<{}> {{", obj.ty.name())?;
                for ((name, _ty), &val) in obj.ty.fields().iter().zip(&obj.fields) {
                    writeln!(
                        f,
                        "  {name}={}",
                        Self {
                            object: val,
                            ..*self
                        }
                    )?;
                }
                write!(f, "}}")
            }
        }
    }
}

fn get_field_index(s: TyStruct, memop: Mem, sea: &Nodes) -> usize {
    s.fields()
        .iter()
        .position(|f| f.0 == sea[memop].name)
        .unwrap_or_else(|| {
            unreachable!("Field {} not found in struct {}", sea[memop].name, s.name())
        })
}

#[derive(Debug, Eq, PartialEq)]
pub enum EResult {
    Timeout,
    Fallthrough,
    Value(Object),
}

/// Find the start node from some node in the graph or null if there is no start node
impl<'t> Nodes<'t> {
    fn find_start(&self, visit: &mut IdSet<Node>, node: Option<Node>) -> Option<Start> {
        let node = node?;
        if visit.get(node) {
            return None;
        }
        visit.add(node);
        if let s @ Some(_) = node.to_start(self) {
            return s;
        }
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
}

pub struct Evaluator<'a, 't> {
    sea: &'a Nodes<'t>,
    blocks: Vec<Block>,
    pub heap: Heap<'t>,

    values: IdVec<Node, Object>,
    phi_cache: Vec<Object>,
    start: Start,
    start_block: BlockId,
}

impl<'a, 't> Evaluator<'a, 't> {
    pub fn new(graph: Node, sea: &'a Nodes<'t>) -> Self {
        let mut visited = IdSet::zeros(sea.len());
        let start = sea.find_start(&mut visited, Some(graph)).unwrap();
        let (blocks, start_block) = scheduler::schedule(start, sea);
        Self {
            sea,
            blocks,
            heap: Heap { objs: vec![] },
            values: IdVec::new(vec![Object::Null; sea.len()]),
            phi_cache: Vec::with_capacity(16),
            start,
            start_block,
        }
    }

    fn div(&self, div: Div) -> i64 {
        let in2 = self.vall(self.sea.inputs[div][2].unwrap());
        if in2 == 0 {
            0
        } else {
            self.vall(self.sea.inputs[div][1].unwrap()) / in2
        }
    }

    fn alloc(&mut self, alloc: New) -> Object {
        let Type::Pointer(p) = self.sea[alloc].inner() else {
            unreachable!()
        };
        let ty = p.to.try_into().unwrap();

        let object = Object::Obj(self.heap.objs.len());
        self.heap.objs.push(Obj {
            ty,
            fields: vec![Object::Null; ty.fields().len()],
        });
        object
    }

    fn load(&self, load: Mem) -> Object {
        let from = self.valo(load.inputs(&self.sea)[2].unwrap());
        let idx = get_field_index(from.ty, load, &self.sea);
        from.fields[idx]
    }

    fn store(&mut self, store: Mem) -> Object {
        let to = store.inputs(&self.sea)[2].unwrap();
        let val = self.val(store.inputs(&self.sea)[3].unwrap());
        let idx = get_field_index(self.valo(to).ty, store, &self.sea);
        self.valo_mut(to).fields[idx] = val;
        Object::Null
    }

    fn is_true(&self, obj: Object) -> bool {
        match obj {
            Object::Long(n) => n != 0,
            Object::Null => false,
            Object::Obj(_) => true,
            Object::Memory => unreachable!(),
        }
    }

    fn val(&self, node: Node) -> Object {
        self.values[node]
    }

    fn vall(&self, node: Node) -> i64 {
        match self.val(node) {
            Object::Long(l) => l,
            v => unreachable!("Not a long {v:?}"),
        }
    }

    fn valo(&self, node: Node) -> &Obj {
        match self.val(node) {
            Object::Obj(o) => &self.heap.objs[o],
            v => unreachable!("Not a long {v:?}"),
        }
    }

    fn valo_mut(&mut self, node: Node) -> &mut Obj<'t> {
        match self.val(node) {
            Object::Obj(o) => &mut self.heap.objs[o],
            v => unreachable!("Not a long {v:?}"),
        }
    }

    fn cons(&self, cons: Constant) -> Object {
        match self.sea[cons].inner() {
            Type::Int(Int::Constant(i)) => Object::Long(*i),
            Type::Pointer(_) => Object::Null,
            _ => unreachable!(),
        }
    }

    fn binary<F: FnOnce(i64, i64) -> i64>(&self, node: Node, op: F) -> Object {
        let a = self.vall(self.sea.inputs[node][1].unwrap());
        let b = self.vall(self.sea.inputs[node][2].unwrap());
        Object::Long(op(a, b))
    }

    fn exec(&mut self, node: Node) -> Object {
        match node.downcast(&self.sea.ops) {
            TypedNode::Constant(n) => self.cons(n),
            TypedNode::Add(_) => self.binary(node, i64::wrapping_add),
            TypedNode::Bool(b) => match self.sea[b] {
                BoolOp::EQ => {
                    let a = self.val(self.sea.inputs[node][1].unwrap());
                    let b = self.val(self.sea.inputs[node][2].unwrap());
                    Object::Long(if a == b { 1 } else { 0 })
                }
                op => self.binary(node, |a, b| op.compute(a, b) as i64),
            },
            TypedNode::Div(n) => Object::Long(self.div(n)),
            TypedNode::Minus(_) => {
                Object::Long(self.vall(node.inputs(&self.sea)[1].unwrap()).wrapping_neg())
            }
            TypedNode::Mul(_) => self.binary(node, i64::wrapping_mul),
            TypedNode::Not(_) => Object::Long(
                if self.is_true(self.val(node.inputs(&self.sea)[1].unwrap())) {
                    0
                } else {
                    1
                },
            ),
            TypedNode::Sub(_) => self.binary(node, i64::wrapping_sub),
            TypedNode::Cast(_) => self.val(node.inputs(&self.sea)[1].unwrap()),
            TypedNode::Mem(n) => match self.sea[n].kind {
                MemOpKind::Load { .. } => self.load(n),
                MemOpKind::Store => self.store(n),
            },
            TypedNode::New(n) => self.alloc(n),
            TypedNode::Proj(n) => {
                self.valo(n.inputs(&self.sea)[0].unwrap()).fields[self.sea[n].index]
            }
            n => unreachable!("Unexpected node {n:?}"),
        }
    }

    /// Run the graph until either a return is found or the number of loop iterations are done.
    pub fn evaluate(&mut self, parameter: i64, mut loops: usize) -> EResult {
        self.values[self.start] = Object::Obj(self.heap.objs.len());
        self.heap.objs.push(Obj {
            ty: self.sea.types.ty_struct_bot.try_into().unwrap(), // dummy
            fields: {
                let Type::Tuple { types } = &*self.sea[self.start].args else {
                    unreachable!();
                };
                let mut f = vec![Object::Memory; types.len()];
                f[0] = Object::Null;
                f[1] = Object::Long(parameter);
                f
            },
        });

        let mut i = 0;
        let mut block = self.start_block;
        loop {
            while i < self.blocks[block].nodes.len() {
                let n = self.blocks[block].nodes[i];
                self.values[n] = self.exec(n);
                i += 1;
            }
            i = 0;

            let Some(exit) = self.blocks[block].exit else {
                return EResult::Fallthrough;
            };

            match exit.downcast(&self.sea.ops) {
                TypedNode::Return(n) => {
                    return EResult::Value(self.val(n.inputs(&self.sea)[1].unwrap()))
                }
                TypedNode::If(n) => {
                    let condition = self.is_true(self.val(n.inputs(&self.sea)[1].unwrap()));
                    block = self.blocks[block].next[if condition { 0 } else { 1 }];
                    // if (block == null) return Status.FALLTHROUGH;
                }
                TypedNode::Region(region) => {
                    if loops == 0 {
                        return EResult::Timeout;
                    }
                    loops -= 1;

                    let exit = self.blocks[block].exit_id.unwrap();
                    debug_assert!(exit > 0 && region.inputs(&self.sea).len() > exit);
                    block = self.blocks[block].next[0];
                    // assert block != null;
                    while i < self.blocks[block].nodes.len() {
                        if let Some(phi) = self.blocks[block].nodes[i].to_phi(&self.sea) {
                            let exit_node = phi.inputs(&self.sea)[exit].unwrap();
                            self.phi_cache.push(self.val(exit_node))
                        } else {
                            break;
                        }
                        i += 1;
                    }

                    i = 0;
                    while i < self.phi_cache.len() {
                        self.values[self.blocks[block].nodes[i]] = self.phi_cache[i];
                        i += 1;
                    }
                    self.phi_cache.clear();
                }
                n => todo!("Unexpected control node {n:?}"),
            }
        }
    }
}

pub fn evaluate<'t>(
    nodes: &Nodes<'t>,
    graph: impl Into<Node>,
    parameter: Option<i64>,
    loops: Option<usize>,
) -> (Heap<'t>, Object) {
    let parameter = parameter.unwrap_or(0);
    let loops = loops.unwrap_or(1000);

    let (heap, res) = evaluate_with_result(nodes, graph.into(), parameter, loops);

    match res {
        EResult::Value(v) => (heap, v),
        EResult::Fallthrough => panic!("fallthrough"),
        EResult::Timeout => panic!("timeout"),
    }
}

pub fn evaluate_with_result<'t>(
    nodes: &Nodes<'t>,
    graph: Node,
    parameter: i64,
    loops: usize,
) -> (Heap<'t>, EResult) {
    let mut evaluator = Evaluator::new(graph, nodes);
    let result = evaluator.evaluate(parameter, loops);
    (evaluator.heap, result)
}
