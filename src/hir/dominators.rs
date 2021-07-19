use std::collections::HashSet;

use crate::hir::Hir;
use crate::hir::id::Id;

impl<'t> Hir<'t> {
    /// TODO: use more efficient algorithm for dominators or at least a more efficient representation like enum DominatorSet { All, Some(HashSet<Id>), One(Id) }
    pub fn dominators(&self) -> Vec<HashSet<Id>> {
        //  // dominator of the start node is the start itself
        //  Dom(n0) = {n0}
        //  // for all other nodes, set all nodes as the dominators
        //  for each n in N - {n0}
        //      Dom(n) = N;
        let mut dominators = Vec::with_capacity(self.nodes.len());
        for n in (0..self.nodes.len()).map(Id::from) {
            if n == self.root {
                dominators.push(HashSet::from([self.root]));
            } else {
                dominators.push(HashSet::from_iter((0..self.nodes.len()).map(Id::from)));
            }
        }

        //  // iteratively eliminate nodes that are not dominators
        //  while changes in any Dom(n)
        //      for each n in N - {n0}:
        //          Dom(n) = {n} union with intersection over Dom(p) for all p in pred(n)
        let mut change = true;
        while change {
            change = false;

            for n in 0..self.nodes.len() {
                let size = dominators[n].len();

                let mut no_dep = true;
                self.nodes[n].for_each_dependency(|p| {
                    no_dep = false;
                    let p_index = p.index();
                    if p_index != n {
                        let [dn, dp] = dominators.get_many_mut([n, p_index]).unwrap();
                        dn.retain(|it| dp.contains(it));
                    }
                });
                if no_dep {
                    dominators[n].clear();
                }

                dominators[n].insert(Id::from(n));

                if dominators[n].len() != size {
                    change = true;
                }
            }
        }

        dominators
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::arena::Arena;
    use crate::hir::Hir;
    use crate::hir::id::Id;
    use crate::hir::operation::Operation;
    use crate::hir::types::Types;

    /// https://en.wikipedia.org/wiki/Dominator_(graph_theory)
    #[test]
    fn example_from_wikipedia() {
        let arena = Arena::new();
        let types = Types::new(&arena);
        let mut hir = Hir::dummy();

        let n5_id = Id::from(4);

        let n1 = hir.add_node(None, Operation::Root {}, types.ty_unit, vec![]);
        let n2 = hir.add_node(
            None,
            Operation::Region {
                inputs: vec![n1, n5_id],
            },
            types.ty_unit,
            vec![],
        );
        let n3 = hir.add_node(
            None,
            Operation::Region { inputs: vec![n2] },
            types.ty_unit,
            vec![],
        );
        let n4 = hir.add_node(
            None,
            Operation::Region { inputs: vec![n2] },
            types.ty_unit,
            vec![],
        );
        let n5 = hir.add_node(
            None,
            Operation::Region {
                inputs: vec![n3, n4],
            },
            types.ty_unit,
            vec![],
        );
        assert_eq!(n5, n5_id);
        let n6 = hir.add_node(
            None,
            Operation::Region { inputs: vec![n2] },
            types.ty_unit,
            vec![],
        );

        let dominators = hir.dominators();

        assert_eq!(dominators[n1.index()], HashSet::from([n1]));
        assert_eq!(dominators[n2.index()], HashSet::from([n1, n2]));
        assert_eq!(dominators[n3.index()], HashSet::from([n1, n2, n3]));
        assert_eq!(dominators[n4.index()], HashSet::from([n1, n2, n4]));
        assert_eq!(dominators[n5.index()], HashSet::from([n1, n2, n5]));
        assert_eq!(dominators[n6.index()], HashSet::from([n1, n2, n6]));
    }
}
