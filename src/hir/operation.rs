use std::rc::Rc;

use crate::hir::id::Id;

#[derive(Debug, Clone)]
pub enum Operation {
    Root {},
    Start {},
    Return {
        value: Option<Id>,
        io: Id,
        memory: Id,
        frame_pointer: Id,
        return_address: Id,
    },
    Parameter {
        name: String,
    },
    Constant {
        value: ConstantValue,
    },
    Call {
        function: Id,
        arguments: Vec<Id>,
    },
    Region {
        inputs: Vec<Id>,
    },
    Phi {
        inputs: Vec<Id>,
    },
    If {
        predicate: Id,
    },
    Projection {
        projection: Projection,
    },
    Local {},
    Global {},
    Load {
        pointer: Id,
    },
    Store {
        pointer: Id,
        value: Id,
    },
    MergeMemory {
        memories: Vec<Id>,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum ConstantValue {
    Integer(i64),
}

#[derive(Clone, Debug)]
pub enum Projection {
    Index(u64),
    Field(Rc<String>),
    Control(bool),
    ReturnAdr,
    FramePtr,
    Memory,
    IO,
}

impl Operation {
    pub fn for_each_dependency<F: FnMut(Id)>(&self, mut f: F) {
        let mut f = |x: &Id| f(*x);
        match self {
            Operation::Root {} => {}
            Operation::Start {} => {}
            Operation::Return {
                value,
                io,
                memory,
                frame_pointer,
                return_address,
            } => {
                value.iter().for_each(&mut f);
                f(io);
                f(memory);
                f(frame_pointer);
                f(return_address);
            }
            Operation::Parameter { name } => {}
            Operation::Constant { value } => {}
            Operation::Call {
                function,
                arguments,
            } => {
                f(function);
                arguments.iter().for_each(f);
            }
            Operation::Region { inputs } => {
                inputs.iter().for_each(f);
            }
            Operation::Phi { inputs } => {
                inputs.iter().for_each(f);
            }
            Operation::If { predicate } => {
                f(predicate);
            }
            Operation::Projection { projection } => {}
            Operation::Local {} => {}
            Operation::Global {} => {}
            Operation::Load { pointer } => {
                f(pointer);
            }
            Operation::Store { pointer, value } => {
                f(pointer);
                f(value);
            }
            Operation::MergeMemory { memories } => {
                memories.iter().for_each(f);
            }
        }
    }
}
