use crate::datastructures::id_set::IdSet;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{Constant, Div, DivF, Load, New, Start, Store, TypedNode};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes};
use crate::sea_of_nodes::tests::scheduler;
use crate::sea_of_nodes::tests::scheduler::{Block, BlockId};
use crate::sea_of_nodes::types::{TyStruct, Type};
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Neg, Shl, Shr, Sub};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Object {
    Long(i64),
    Null,
    Memory,
    Double(f64),
    Obj(usize),
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Long(value)
    }
}
impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Object::Double(value)
    }
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

#[derive(Debug)]
pub struct ResultObject<'t> {
    pub heap: Heap<'t>,
    pub object: Object,
}

impl<'t> Obj<'t> {
    pub fn new(ty: TyStruct<'t>, fields: Vec<Object>, heap: &mut Heap<'t>) -> Object {
        let object = Object::Obj(heap.objs.len());
        heap.objs.push(Obj { ty, fields });
        object
    }
}

impl<'t> Display for ResultObject<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        PrintableObject {
            object: self.object,
            heap: &self.heap,
        }
        .fmt(f)
    }
}

pub struct PrintableObject<'o, 't> {
    pub object: Object,
    pub heap: &'o Heap<'t>,
}

impl<'a, 't> Display for PrintableObject<'a, 't> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.object {
            Object::Long(l) => l.fmt(f),
            Object::Double(d) => d.fmt(f),
            Object::Null => "null".fmt(f),
            Object::Memory => "memory".fmt(f),
            Object::Obj(obj) => {
                let obj = &self.heap.objs[obj];
                writeln!(f, "Obj<{}> {{", obj.ty.name())?;
                for (field, &val) in obj.ty.fields().iter().zip(&obj.fields) {
                    writeln!(
                        f,
                        "  {}={}",
                        field.fname,
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

fn get_field_index(s: TyStruct, name: &str, _off: i64) -> usize {
    s.fields()
        .iter()
        .position(|f| f.fname == name)
        .unwrap_or_else(|| unreachable!("Field {name} not found in struct {}", s.name()))
}

#[derive(Debug, PartialEq)]
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
    fn divf(&self, div: DivF) -> f64 {
        let in2 = self.vald(self.sea.inputs[div][2].unwrap());
        if in2 == 0.0 {
            0.0
        } else {
            self.vald(self.sea.inputs[div][1].unwrap()) / in2
        }
    }

    fn off_to_idx(&self, mut off: i64, t: TyStruct<'t>) -> i64 {
        off -= t.ary_base(self.sea.types);
        let scale = t.ary_scale(self.sea.types);
        let mask = (1 << scale) - 1;
        assert_eq!(off & mask, 0);
        off >> scale
    }

    fn alloc(&mut self, alloc: New) -> Object {
        let ty = self.sea[alloc].data().to;
        let mut body;
        if ty.is_ary() {
            let Object::Long(sz) = self.val(alloc.inputs(self.sea)[1].unwrap()) else {
                unreachable!()
            };
            let n = self.off_to_idx(sz, ty);
            if n < 0 {
                panic!("NegativeArraySizeException({n})")
            }
            let elem = ty.fields()[1].ty;
            let init = if elem.is_int() {
                Object::Long(0)
            } else if elem.is_float() {
                Object::Double(0.0)
            } else {
                assert!(elem.is_mem_ptr());
                Object::Null
            };
            body = vec![init; n as usize + 1]; // Array body

            // Length value
            body[0] = self
                .vall(alloc.inputs(self.sea)[2 + 2].unwrap())
                .try_into()
                .unwrap();
        } else {
            let num = ty.fields().len();
            body = (0..num)
                .map(|i| self.val(alloc.inputs(self.sea)[2 + i + num].unwrap()))
                .collect();
        }
        let mut mems = vec![Object::Null; ty.fields().len() + 2];
        // mems[0] is control
        mems[1] = Obj::new(ty, body, &mut self.heap); // the ref
                                                      // mems[2+...] are memory aliases

        // unlike java we create an obj
        Obj::new(self.sea.types.struct_top, mems, &mut self.heap)
    }

    fn load(&self, load: Load) -> Object {
        let from = self.valo(load.ptr(&self.sea).unwrap());
        let off = self.vall(load.off(&self.sea).unwrap());
        let idx = get_field_index(from.ty, load.to_mem_name(self.sea).unwrap(), off);
        if idx == from.ty.fields().len() - 1 && from.ty.is_ary() {
            let len = from.fields.len() - from.ty.fields().len() + 1;
            let i = self.off_to_idx(off, from.ty);
            if i < 0 || i as usize >= len {
                panic!("Array index out of bounds {i} < {len}")
            }
            from.fields[i as usize + from.ty.fields().len() - 1]
        } else {
            from.fields[idx]
        }
    }

    fn store(&mut self, store: Store) -> Object {
        let ptr = store.ptr(&self.sea).unwrap();
        let to = self.valo(ptr);
        let off = self.vall(store.off(&self.sea).unwrap());
        let val = self.val(store.val(&self.sea).unwrap());
        let idx = get_field_index(to.ty, store.to_mem_name(self.sea).unwrap(), off);

        if idx == to.ty.fields().len() - 1 && to.ty.is_ary() {
            let len = to.fields.len() - to.ty.fields().len() + 1;
            let i = self.off_to_idx(off, to.ty);
            if i < 0 || i as usize >= len {
                panic!("Array index out of bounds {i} < {len}")
            }
            let to = self.valo_mut(ptr);
            to.fields[i as usize + to.ty.fields().len() - 1] = val;
        } else {
            let to = self.valo_mut(ptr);
            to.fields[idx] = val;
        }
        Object::Null
    }

    fn is_true(&self, obj: Object) -> bool {
        match obj {
            Object::Long(n) => n != 0,
            Object::Double(n) => n.abs() != 0.0,
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
            Object::Double(l) => l as i64,
            v => unreachable!("Not a long {v:?}"),
        }
    }

    fn vald(&self, node: Node) -> f64 {
        match self.val(node) {
            Object::Long(l) => l as f64,
            Object::Double(l) => l,
            v => unreachable!("Not a double {v:?}"),
        }
    }

    fn valo(&self, node: Node) -> &Obj<'t> {
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
        match self.sea[cons].data() {
            Type::Int(i) => Object::Long(i.min),
            Type::Float(f) => Object::Double(f.con()),
            Type::MemPtr(_) => Object::Null,
            _ => unreachable!(),
        }
    }

    fn binary<T: Into<Object>, F: FnOnce(Node, Node) -> T>(&self, node: Node, op: F) -> Object {
        let a = self.sea.inputs[node][1].unwrap();
        let b = self.sea.inputs[node][2].unwrap();
        op(a, b).into()
    }
    fn binaryl<T: Into<Object>, F: FnOnce(i64, i64) -> T>(&self, node: Node, op: F) -> Object {
        self.binary(node, |a, b| op(self.vall(a), self.vall(b)))
    }
    fn binaryd<T: Into<Object>, F: FnOnce(f64, f64) -> T>(&self, node: Node, op: F) -> Object {
        self.binary(node, |a, b| op(self.vald(a), self.vald(b)))
    }

    fn exec(&mut self, node: Node) -> Object {
        match node.downcast(&self.sea.ops) {
            TypedNode::Constant(n) => self.cons(n),
            TypedNode::Add(_) => self.binaryl(node, i64::wrapping_add),
            TypedNode::AddF(_) => self.binaryd(node, f64::add),
            TypedNode::Bool(b) => match self.sea[b] {
                BoolOp::EQ => self.binary(node, |a, b| (a == b) as i64),
                BoolOp::LE => self.binaryl(node, |a, b| (a <= b) as i64),
                BoolOp::LT => self.binaryl(node, |a, b| (a < b) as i64),
                BoolOp::EQF => self.binaryd(node, |a, b| (a == b) as i64),
                BoolOp::LEF => self.binaryd(node, |a, b| (a <= b) as i64),
                BoolOp::LTF => self.binaryd(node, |a, b| (a < b) as i64),
            },
            TypedNode::Div(n) => self.div(n).into(),
            TypedNode::DivF(n) => self.divf(n).into(),
            TypedNode::Minus(_) => self
                .vall(node.inputs(&self.sea)[1].unwrap())
                .wrapping_neg()
                .into(),
            TypedNode::MinusF(_) => self.vald(node.inputs(&self.sea)[1].unwrap()).neg().into(),
            TypedNode::Mul(_) => self.binaryl(node, i64::wrapping_mul),
            TypedNode::MulF(_) => self.binaryd(node, f64::mul),
            TypedNode::Not(_) => Object::Long(
                if self.is_true(self.val(node.inputs(&self.sea)[1].unwrap())) {
                    0
                } else {
                    1
                },
            ),
            TypedNode::Sub(_) => self.binaryl(node, i64::wrapping_sub),
            TypedNode::SubF(_) => self.binaryd(node, f64::sub),
            TypedNode::Shl(_) => self.binaryl(node, i64::shl),
            TypedNode::Shr(_) => self.binaryl(node, |a, b| (a as u64 >> b as u64) as i64),
            TypedNode::Sar(_) => self.binaryl(node, i64::shr),
            TypedNode::And(_) => self.binaryl(node, i64::bitand),
            TypedNode::Or(_) => self.binaryl(node, i64::bitor),
            TypedNode::Xor(_) => self.binaryl(node, i64::bitxor),
            TypedNode::Cast(_) => self.val(node.inputs(&self.sea)[1].unwrap()),
            TypedNode::ToFloat(_) => (self.vall(node.inputs(&self.sea)[1].unwrap()) as f64).into(),
            TypedNode::Load(n) => self.load(n),
            TypedNode::Store(n) => self.store(n),
            TypedNode::New(n) => self.alloc(n),
            TypedNode::CProj(n) => {
                self.valo(n.inputs(&self.sea)[0].unwrap()).fields[self.sea[n].index]
            }
            TypedNode::Proj(n) => {
                self.valo(n.inputs(&self.sea)[0].unwrap()).fields[self.sea[n].index]
            }
            TypedNode::ScopeMin(_) => Object::Null,
            TypedNode::ReadOnly(n) => self.val(n.inputs(&self.sea)[1].unwrap()),
            n => unreachable!("Unexpected node {n:?}"),
        }
    }

    /// Run the graph until either a return is found or the number of loop iterations are done.
    pub fn evaluate(&mut self, parameter: i64, mut loops: usize) -> EResult {
        self.values[self.start] = Obj::new(
            self.sea.types.struct_bot,
            {
                let types = self.sea[self.start].args;
                let mut f = vec![Object::Null; types.data().len()];
                f[1] = Object::Memory;
                f[2] = Object::Long(parameter);
                f
            },
            &mut self.heap,
        );

        let mut i = 0;
        let mut block = self.start_block;
        loop {
            while i < self.blocks[block].nodes.len() {
                let n = self.blocks[block].nodes[i];
                self.values[n] = self.exec(n);
                i += 1;
            }
            i = 0;

            let Some(exit_node) = self.blocks[block].exit else {
                return EResult::Fallthrough;
            };

            match exit_node.downcast(&self.sea.ops) {
                TypedNode::Return(n) => {
                    return EResult::Value(self.val(n.inputs(&self.sea)[1].unwrap()))
                }
                TypedNode::If(n) => {
                    let condition = self.is_true(self.val(n.inputs(&self.sea)[1].unwrap()));
                    block = self.blocks[block].next[if condition { 0 } else { 1 }];
                    // if (block == null) return Status.FALLTHROUGH;
                }
                TypedNode::Region(_) | TypedNode::Loop(_) => {
                    if loops == 0 {
                        return EResult::Timeout;
                    }
                    loops -= 1;

                    let exit = self.blocks[block].exit_id.unwrap();
                    debug_assert!(exit > 0 && exit_node.inputs(&self.sea).len() > exit);
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
) -> ResultObject<'t> {
    let parameter = parameter.unwrap_or(0);
    let loops = loops.unwrap_or(1000);

    let (heap, res) = evaluate_with_result(nodes, graph.into(), parameter, loops);

    match res {
        EResult::Value(object) => ResultObject { heap, object },
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
