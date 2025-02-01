use crate::sea_of_nodes::nodes::node::{
    CProj, Constant, If, Load, Loop, Minus, New, Not, Proj, Return, Scope, ScopeMin, Start, Stop,
    Store, Struct, XCtrl,
};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op};
use crate::sea_of_nodes::types::{Ty, TyStruct, Types};
use crate::sea_of_nodes::{graph_visualizer, types};
use std::collections::HashMap;

/// Converts a Simple source program to the Sea of Nodes intermediate representation in one pass.
pub struct Parser<'s, 't> {
    /// Tokenizes the source code
    lexer: Lexer<'s>,

    /// Manages interned types.
    types: &'t Types<'t>,

    /// Manages all nodes and implements operations such as peephole.
    /// Keeps track of the current start for constant node creation.
    pub nodes: Nodes<'t>,

    /// Next available memory alias number
    alias: usize,

    /// returned after parsing
    pub(crate) stop: Stop,

    /// The current scope changes as we parse.
    pub(crate) scope: Scope,

    /// stack of scopes for graph visualization
    pub(crate) x_scopes: Vec<Scope>,

    continue_scope: Option<Scope>,

    /// Merge all the while-breaks here
    break_scope: Option<Scope>,

    /// Mapping from a type name to a Type.  The string name matches
    /// `type.str()` call.  No TypeMemPtrs are in here, because Simple does not
    /// have C-style '*ptr' references.
    name_to_type: HashMap<&'t str, Ty<'t>>,

    /// Mapping from a type name to the constructor for a Type.
    inits: HashMap<&'t str, Struct>,

    pub disable_show_graph_println: bool,
}

type ParseErr = String;
type PResult<T> = Result<T, ParseErr>;

pub fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "bool"
            | "break"
            | "byte"
            | "continue"
            | "else"
            | "f32"
            | "f64"
            | "false"
            | "flt"
            | "i16"
            | "i32"
            | "i64"
            | "i8"
            | "if"
            | "int"
            | "new"
            | "null"
            | "return"
            | "struct"
            | "true"
            | "u1"
            | "u16"
            | "u32"
            | "u8"
            | "while"
    )
}

impl<'s, 't> Parser<'s, 't> {
    pub fn new(source: &'s str, types: &'t Types<'t>) -> Self {
        Self::new_with_arg(source, types, types.int_bot)
    }

    pub fn new_with_arg(source: &'s str, types: &'t Types<'t>, arg: Ty<'t>) -> Self {
        let mut nodes = Nodes::new(types);

        let scope = Scope::new(&mut nodes);
        nodes.ty[scope] = Some(types.bot); // in java this is done by the constructor

        nodes.start = Start::new(&[types.ctrl, arg], &mut nodes);

        let stop = Stop::new(&mut nodes);

        nodes.zero = Constant::new(types.int_zero, &mut nodes)
            .peephole(&mut nodes)
            .keep(&mut nodes)
            .to_constant(&nodes)
            .unwrap();
        nodes.xctrl = XCtrl::new(&mut nodes)
            .peephole(&mut nodes)
            .keep(&mut nodes)
            .to_xctrl(&nodes)
            .unwrap();

        Self {
            lexer: Lexer {
                source,
                remaining: source,
            },
            types,
            nodes,
            stop,
            scope,
            x_scopes: vec![],
            continue_scope: None,
            break_scope: None,
            alias: 2, // alias 0 for the control, 1 for memory
            name_to_type: Self::default_types(types),
            inits: HashMap::new(),
            disable_show_graph_println: false,
        }
    }

    fn default_types(types: &'t Types<'t>) -> HashMap<&'t str, Ty<'t>> {
        HashMap::from([
            ("bool", types.u1),
            ("byte", types.u8),
            ("f32", types.f32),
            ("f64", types.flt_bot),
            ("flt", types.flt_bot),
            ("i16", types.i16),
            ("i32", types.i32),
            ("i64", types.int_bot),
            ("i8", types.i8),
            ("int", types.int_bot),
            ("u1", types.u1),
            ("u16", types.u16),
            ("u32", types.u32),
            ("u8", types.u8),
            ("val", types.top), // Marker type, indicates type inference
            ("var", types.bot), // Marker type, indicates type inference
        ])
    }

    fn ctrl(&mut self) -> Node {
        self.nodes.inputs[self.scope][0].expect("has ctrl")
    }
    fn set_ctrl(&mut self, node: Node) {
        self.scope.set_def(0, node, &mut self.nodes);
    }

    pub fn parse(&mut self) -> PResult<Stop> {
        self.x_scopes.push(self.scope);

        // Enter a new scope for the initial control and arguments
        self.scope.push(false, &mut self.nodes);
        let mem = ScopeMin::new(&mut self.nodes);
        mem.add_def(None, &mut self.nodes);

        // project ctrl and arg0 from start
        let start = self.nodes.start;
        self.scope
            .define(
                Scope::CTRL,
                self.types.ctrl,
                false,
                CProj::new(start, 0, Scope::CTRL, &mut self.nodes).peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");
        let mem0 = Proj::new(start, 1, Scope::MEM0, &mut self.nodes).peephole(&mut self.nodes);
        mem.add_def(mem0, &mut self.nodes);
        self.scope
            .define(
                Scope::MEM0,
                self.types.mem_top,
                false,
                mem0.peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");
        self.scope
            .define(
                Scope::ARG0,
                self.types.int_bot,
                false,
                Proj::new(start, 2, Scope::ARG0, &mut self.nodes).peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");

        // Parse whole program
        self.parse_block(false)?;

        if self.ctrl().ty(&self.nodes) == Some(self.types.ctrl) {
            let ret = Return::new(
                self.ctrl(),
                *self.nodes.zero,
                Some(self.scope),
                &mut self.nodes,
            )
            .peephole(&mut self.nodes);
            self.stop.add_def(ret, &mut self.nodes);
        }

        self.scope.kill(&mut self.nodes);
        self.x_scopes.pop();

        for init in self.inits.values() {
            init.unkeep(&mut self.nodes).kill(&mut self.nodes);
        }
        self.inits.clear();

        if !self.lexer.is_eof() {
            Err(format!(
                "Syntax error, unexpected {}",
                self.lexer.get_any_next_token()
            ))
        } else {
            let p = self.stop.peephole(&mut self.nodes);
            assert_eq!(*self.stop, p);
            Ok(self.stop)
        }
    }

    pub fn iterate(&mut self, stop: Stop) {
        self.nodes.iterate(stop)
    }

    pub fn type_check(&mut self, stop: Stop) -> Result<(), String> {
        self.nodes.type_check(stop)
    }

    /// <pre>
    ///     '{' statements '}'
    /// </pre>
    ///
    /// Does not parse the opening or closing `{}`
    fn parse_block(&mut self, inCon: bool) -> PResult<()> {
        self.scope.push(inCon, &mut self.nodes);
        while !self.peek('}') && !self.lexer.is_eof() {
            self.parse_statement()?;
        }
        self.scope.pop(&mut self.nodes);
        Ok(())
    }

    /// <pre>
    ///     returnStatement | declStatement | blockStatement | ifStatement | expressionStatement
    /// </pre>
    fn parse_statement(&mut self) -> PResult<()> {
        if self.matchx("return") {
            self.parse_return()
        } else if self.match_("{") {
            self.parse_block(false)?;
            self.require("}")
        } else if self.matchx("if") {
            self.parse_if()
        } else if self.matchx("while") {
            self.parse_while()
        } else if self.matchx("for") {
            self.parse_for()
        } else if self.matchx("break") {
            self.parse_break()
        } else if self.matchx("continue") {
            self.parse_continue()
        } else if self.matchx("struct") {
            self.parse_struct()
        } else if self.matchx("#showGraph") {
            self.show_graph();
            self.require(";")
        } else if self.matchx(";") {
            Ok(()) // Empty statement
        } else {
            self.parse_declaration_statement() // Declaration or normal assignment/expression
        }
    }

    /// <pre>
    ///     while ( expression ) statement
    /// </pre>
    fn parse_while(&mut self) -> PResult<()> {
        self.require("(")?;
        self.parse_looping(false)
    }

    /// <pre>
    ///     for( var x=init; test; incr ) body
    /// </pre>
    fn parse_for(&mut self) -> PResult<()> {
        // {   var x=init,y=init,...;
        //     while( pred ) {
        //         body;
        //         next;
        //     }
        // }
        self.require("(")?;
        self.scope.push(false, &mut self.nodes); // Scope for the index variables
        if !self.match_(";") {
            // Can be empty init "for(;test;next) body"
            self.parse_declaration_statement(); // Non-empty init
        }
        let rez = self.parse_looping(true);
        self.scope.pop(&mut self.nodes); // Exit index variable scope
        rez
    }

    /// Shared by `for` and `while`
    fn parse_looping(&mut self, do_for: bool) -> PResult<()> {
        let saved_continue_scope = self.continue_scope;
        let saved_break_scope = self.break_scope;

        // Loop region has two control inputs, the first is the entry
        // point, and second is back edge that is set after loop is parsed
        // (see end_loop() call below).  Note that the absence of back edge is
        // used as an indicator to switch off peepholes of the region and
        // associated phis; see {@code inProgress()}.

        let ctrl = Loop::new(self.ctrl(), &mut self.nodes).peephole(&mut self.nodes); // Note we set back edge to null here
        self.set_ctrl(ctrl);

        // At loop head, we clone the current Scope (this includes all
        // names in every nesting level within the Scope).
        // We create phis eagerly for all the names we find, see dup().

        // Save the current scope as the loop head
        let head = self.scope;
        head.keep(&mut self.nodes);

        // Clone the head Scope to create a new Scope for the body.
        // Create phis eagerly as part of cloning
        self.scope = self.scope.dup(true, &mut self.nodes); // The true argument triggers creating phis
        self.x_scopes.push(self.scope);

        // Parse predicate
        let pred = if self.peek(';') {
            self.int_con(1)
        } else {
            self.parse_asgn()
        };
        self.require(if do_for { ";" } else { ")" })?;

        // IfNode takes current control and predicate
        let if_node = If::new(ctrl, Some(pred.keep(&mut self.nodes)), &mut self.nodes)
            .peephole(&mut self.nodes);

        // Setup projection nodes
        let if_true = CProj::new(if_node.keep(&mut self.nodes), 0, "True", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);
        let if_false = CProj::new(if_node.unkeep(&mut self.nodes), 1, "False", &mut self.nodes)
            .peephole(&mut self.nodes);

        // for( ;;next ) body
        let mut next_pos = "dummy_pos";
        let mut next_end = "dummy_pos";
        if do_for {
            // Skip the next expression and parse it later
            next_pos = self.pos();
            self.skip_asgn()?;
            next_end = self.pos();
            self.require(")")?;
        }

        // Clone the body Scope to create the break/exit Scope which accounts for any
        // side effects in the predicate.  The break/exit Scope will be the final
        // scope after the loop, and its control input is the False branch of
        // the loop predicate.  Note that body Scope is still our current scope.
        self.set_ctrl(if_false);
        let break_scope = self.scope.dup(false, &mut self.nodes);
        self.break_scope = Some(break_scope);
        self.x_scopes.push(break_scope);
        break_scope.add_guards(if_false, pred, true, &mut self.nodes); // Up-cast predicate

        // No continues yet
        self.continue_scope = None;

        // Parse the true side, which corresponds to loop body
        // Our current scope is the body Scope
        self.set_ctrl(if_true.unkeep(&mut self.nodes));
        self.scope.add_guards(
            if_true,
            pred.unkeep(&mut self.nodes),
            false,
            &mut self.nodes,
        ); // Up-cast predicate
        self.parse_statement()?; // Parse loop body
        self.scope.remove_guards(if_true, &mut self.nodes);

        // Merge the loop bottom into other continue statements
        if self.continue_scope.is_some() {
            self.continue_scope = Some(self.jump_to(self.continue_scope));
            self.scope.kill(&mut self.nodes);
            self.scope = self.continue_scope.unwrap();
        }

        // Now append the next code onto the body code
        if do_for {
            let old = self.pos();
            self.set_pos(next_pos);
            if !self.peek(')') {
                self.parse_asgn()?;
            }
            if self.pos() != next_end {
                return Err(self.error_syntax("Unexpected code after expression"));
            }
            self.set_pos(old);
        }

        // The true branch loops back, so whatever is current _scope.ctrl gets
        // added to head loop as input.  endLoop() updates the head scope, and
        // goes through all the phis that were created earlier.  For each phi,
        // it sets the second input to the corresponding input from the back
        // edge.  If the phi is redundant, it is replaced by its sole input.
        let exit = self.break_scope.unwrap();
        head.end_loop(self.scope, exit, &mut self.nodes);
        head.unkeep(&mut self.nodes);
        head.kill(&mut self.nodes);

        // Cleanup
        self.x_scopes.pop();
        self.x_scopes.pop();

        self.continue_scope = saved_continue_scope;
        self.break_scope = saved_break_scope;

        // At exit the false control is the current control, and
        // the scope is the exit scope after the exit test.
        *self.x_scopes.last_mut().unwrap() = exit;
        self.scope = exit;
        Ok(())
    }

    fn jump_to(&mut self, to_scope: Option<Scope>) -> Scope {
        let cur = self.scope.dup(false, &mut self.nodes);
        self.set_ctrl(**self.nodes.xctrl); // Kill current scope

        // Prune nested lexical scopes that have depth > than the loop head
        // We use _breakScope as a proxy for the loop head scope to obtain the depth
        let break_scopes_len = self.nodes[self.break_scope.unwrap()].scopes.len();
        while self.nodes[cur].scopes.len() > break_scopes_len {
            cur.pop(&mut self.nodes);
        }

        // If this is a continue then first time the target is null
        // So we just use the pruned current scope as the base for the
        // continue
        let Some(to_scope) = to_scope else {
            return cur;
        };

        // toScope is either the break scope, or a scope that was created here
        debug_assert!(self.nodes[to_scope].scopes.len() <= break_scopes_len);
        let region = to_scope.merge_scopes(cur, &mut self.nodes);
        to_scope.set_def(0, region, &mut self.nodes); // set ctrl
        to_scope
    }

    fn check_loop_active(&self) -> PResult<()> {
        if self.break_scope.is_none() {
            Err("No active loop for a break or continue".to_string())
        } else {
            Ok(())
        }
    }

    fn parse_break(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        self.break_scope = Some(self.jump_to(self.break_scope));
        self.require(";")
    }

    fn parse_continue(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        self.continue_scope = Some(self.jump_to(self.continue_scope));
        self.require(";")
    }

    /// <pre>
    ///     if ( expression ) statement [else statement]
    /// </pre>
    fn parse_if(&mut self) -> PResult<()> {
        self.require("(")?;

        // Parse predicate
        let pred = self.parse_expression()?;
        self.require(")")?;
        pred.keep(&mut self.nodes);

        // IfNode takes current control and predicate
        let if_node = If::new(self.ctrl(), Some(pred), &mut self.nodes).peephole(&mut self.nodes);

        // Setup projection nodes
        let if_true = CProj::new(if_node.keep(&mut self.nodes), 0, "True", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);
        let if_false = CProj::new(if_node.unkeep(&mut self.nodes), 1, "False", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.scope.dup(false, &mut self.nodes);
        self.x_scopes.push(false_scope);

        // Parse the true side
        if_true.unkeep(&mut self.nodes);
        self.set_ctrl(if_true);
        self.scope.upcast(if_true, pred, false, &mut self.nodes); // Up-cast predicate
        self.parse_statement()?;
        let true_scope = self.scope;

        // Parse the false side
        self.scope = false_scope;
        self.set_ctrl(if_false.unkeep(&mut self.nodes));
        if self.matchx("else") {
            self.scope.upcast(if_false, pred, true, &mut self.nodes); // Up-cast predicate
            self.parse_statement()?;
            false_scope = self.scope;
        }
        pred.unkeep(&mut self.nodes);

        if self.nodes.inputs[true_scope].len() != n_defs
            || self.nodes.inputs[false_scope].len() != n_defs
        {
            return Err("Cannot define a new name on one arm of an if".to_string());
        }

        // Merge results
        self.scope = true_scope;
        self.x_scopes.pop();

        let region = true_scope.merge_scopes(false_scope, &mut self.nodes);
        self.set_ctrl(region);
        Ok(())
    }

    /// <pre>
    ///     'return' expr ;
    /// </pre>
    ///
    /// Parses a return statement; "return" already parsed.
    /// The $ctrl edge is killed.
    fn parse_return(&mut self) -> PResult<()> {
        let expr = self.parse_expression()?;
        self.require(";")?;
        let ctrl = self.ctrl();
        let ret =
            Return::new(ctrl, expr, Some(self.scope), &mut self.nodes).peephole(&mut self.nodes);

        self.stop.add_def(ret, &mut self.nodes);
        self.set_ctrl(**self.nodes.xctrl);
        Ok(())
    }

    /// Dumps out the node graph
    pub fn show_graph(&mut self) {
        let dot = graph_visualizer::generate_dot_output(
            &self.nodes,
            self.stop,
            Some(self.scope),
            &self.x_scopes,
            self.lexer.source,
            false,
        )
        .unwrap();
        if !self.disable_show_graph_println {
            println!("{dot}");
            // uncomment to open in browser:
            // graph_visualizer::run_graphviz_and_chromium(dot);
        }
    }

    pub fn print(&mut self, node: impl Into<Node>) -> String {
        node.into().print(&self.nodes).to_string()
    }

    /// Parses an expression statement or a declaration statement where type is a struct
    ///
    /// <pre>
    ///      name;         // Error
    /// type name;         // Define name with default initial value
    /// type name = expr;  // Define name with given   initial value
    ///      name = expr;  // Reassign existing
    ///             expr   // Something else
    /// </pre>
    ////
    fn parse_expression_statement(&mut self) -> PResult<()> {
        let old = self.lexer.remaining;
        let t = self.parse_type();
        let name = self.require_id()?;

        let expr: Node;
        if self.match_(";") {
            // Assign a default value
            if let Some(t) = t {
                let init = self.types.make_init(t).unwrap();
                expr = Constant::new(init, &mut self.nodes).peephole(&mut self.nodes)
            } else {
                // No type and no expr is an error
                return Err("expression".to_string());
            }
        } else if self.match_("=") {
            // Assign "= expr;"
            expr = self.parse_expression()?;
            self.require(";")?;
        } else {
            // Neither, so just a normal expression parse
            self.lexer.remaining = old;
            self.parse_expression()?;
            return self.require(";");
        };

        // Defining a new variable vs updating an old one
        let name = self.types.get_str(name);
        let t = if let Some(t) = t {
            if self.scope.define(name, t, expr, &mut self.nodes).is_err() {
                return Err(format!("Redefining name '{name}'"));
            }
            t
        } else if let Some((_, t)) = self.nodes[self.scope].lookup(name).copied() {
            self.scope.update(name, expr, &mut self.nodes).unwrap();
            t
        } else {
            return Err(format!("Undefined name '{name}'"));
        };

        let expr_ty = self.nodes.ty[expr].unwrap();
        if !self.types.isa(expr_ty, t) {
            return Err(format!(
                "Type {} is not of declared type {}",
                expr_ty.str(),
                t.str()
            ));
        }
        Ok(())
    }

    fn parse_type(&mut self) -> Option<Ty<'t>> {
        let old = self.lexer.remaining;
        let tname = self.lexer.match_id()?;
        if tname == "int" {
            Some(self.types.int_bot)
        } else if let Some(&obj) = self.name_to_type.get(tname) {
            let nil = self.match_("?");
            Some(*self.types.get_mem_ptr(obj.to_struct().unwrap(), nil))
        } else {
            // Not a type; unwind the parse
            self.lexer.remaining = old;
            None
        }
    }
    /// <pre>
    ///     type name = expression ';'
    /// </pre>
    fn parse_decl(&mut self, t: Ty<'t>) -> PResult<()> {
        let name = self.require_id()?;
        let expr = if self.peek(';') {
            // Assign a null value
            let v = self.types.make_init(t).unwrap();
            Constant::new(v, &mut self.nodes).peephole(&mut self.nodes)
        } else {
            // Assign "= expr;"
            self.require("=")?;
            self.parse_expression()?
        };
        self.require(";")?;

        let expr_ty = self.nodes.ty[expr].unwrap();
        if !self.types.isa(expr_ty, t) {
            return Err(format!(
                "Type {} is not of declared type {}",
                expr_ty.str(),
                t.str()
            ));
        }

        let name = self.types.get_str(name);
        self.scope
            .define(name, t, expr, &mut self.nodes)
            .map_err(|()| format!("Redefining name '{name}'"))
    }

    /// <pre>
    ///     expr : compareExpr
    /// </pre>
    fn parse_expression(&mut self) -> PResult<Node> {
        self.parse_comparison()
    }

    /// <pre>
    ///     expr : additiveExpr op additiveExpr
    /// </pre>
    fn parse_comparison(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_addition()?;
        loop {
            let mut negate = false;
            let (op, idx) = if self.match_("==") {
                (BoolOp::EQ, 2)
            } else if self.match_("!=") {
                negate = true;
                (BoolOp::EQ, 2)
            } else if self.match_("<=") {
                (BoolOp::LE, 2)
            } else if self.match_("<") {
                (BoolOp::LT, 2)
            } else if self.match_(">=") {
                (BoolOp::LE, 1)
            } else if self.match_(">") {
                (BoolOp::LT, 1)
            } else {
                return Ok(lhs);
            };
            // do it before parsing rhs to get same ids as java...
            lhs = self.nodes.create((
                Op::Bool(op),
                if idx == 2 {
                    vec![None, Some(lhs), None]
                } else {
                    vec![None, None, Some(lhs)]
                },
            ));
            let rhs = self.parse_addition()?;
            lhs.set_def(idx, rhs, &mut self.nodes);
            lhs = lhs.peephole(&mut self.nodes);
            if negate {
                lhs = Not::new(lhs, &mut self.nodes).peephole(&mut self.nodes);
            }
        }
    }

    /// <pre>
    ///     additiveExpr : multiplicativeExpr (('+' | '-') multiplicativeExpr)*
    /// </pre>
    fn parse_addition(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_multiplication()?;
        loop {
            let op = if self.match_("+") {
                Op::Add
            } else if self.match_("-") {
                Op::Sub
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_multiplication()?;
            lhs.set_def(2, rhs, &mut self.nodes);
            lhs = lhs.peephole(&mut self.nodes);
        }
    }

    /// <pre>
    ///     multiplicativeExpr : unaryExpr (('*' | '/') unaryExpr)*
    /// </pre>
    fn parse_multiplication(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = if self.match_("*") {
                Op::Mul
            } else if self.match_("/") {
                Op::Div
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_unary()?;
            lhs.set_def(2, rhs, &mut self.nodes);
            lhs = lhs.peephole(&mut self.nodes);
        }
    }

    /// <pre>
    ///     unaryExpr : ('-') | '!') unaryExpr | postfixExpr | primaryExpr
    /// </pre>
    fn parse_unary(&mut self) -> PResult<Node> {
        if self.match_("-") {
            self.parse_unary()
                .map(|expr| Minus::new(expr, &mut self.nodes).peephole(&mut self.nodes))
        } else if self.match_("!") {
            self.parse_unary()
                .map(|expr| Not::new(expr, &mut self.nodes).peephole(&mut self.nodes))
        } else {
            let primary = self.parse_primary()?;
            self.parse_postfix(primary)
        }
    }

    /// <pre>
    ///     primaryExpr : integerLiteral | Identifier | true | false | null | new Identifier | '(' expression ')'
    /// </pre>
    fn parse_primary(&mut self) -> PResult<Node> {
        if self.lexer.peek_number() {
            self.parse_integer_literal()
        } else if self.match_("(") {
            let e = self.parse_expression()?;
            self.require(")")?;
            Ok(e)
        } else if self.matchx("true") {
            Ok(Constant::new(self.types.int_one, &mut self.nodes).peephole(&mut self.nodes))
        } else if self.matchx("false") {
            Ok(*self.nodes.zero)
        } else if self.matchx("null") {
            Ok(Constant::new(*self.types.pointer_null, &mut self.nodes).peephole(&mut self.nodes))
        } else if self.matchx("new") {
            let ty_name = self.require_id()?;
            let ty = self.name_to_type.get(ty_name);
            if let Some(ty) = ty.and_then(|t| t.to_struct()) {
                Ok(self.new_struct(ty))
            } else {
                Err(format!("Unknown struct type '{ty_name}'"))
            }
        } else if let Some(name) = self.lexer.match_id() {
            self.scope
                .lookup(name, &mut self.nodes)
                .map_err(|()| format!("Undefined name '{name}'"))
        } else {
            Err(self.error_syntax("an identifier or expression"))
        }
    }

    /// Return a NewNode but also generate instructions to initialize it.
    fn new_struct(&mut self, obj: TyStruct<'t>) -> Node {
        let ptr_ty = self.types.get_mem_ptr(obj, false);
        let ctrl = self.ctrl();
        let n = New::new(ptr_ty, ctrl, &mut self.nodes).peephole(&mut self.nodes);
        n.keep(&mut self.nodes);

        let types::Struct::Struct { name, fields } = *obj.data() else {
            unreachable!()
        };

        let mut alias = *self.nodes[self.nodes.start].alias_starts.get(name).unwrap();
        for &(fname, _) in fields {
            let mem_slice = self.mem_alias_lookup(alias).unwrap();
            let store = Store::new(
                fname,
                alias,
                [self.ctrl(), mem_slice, n, *self.nodes.zero],
                &mut self.nodes,
            )
            .peephole(&mut self.nodes);
            self.mem_alias_update(alias, store).unwrap();
            alias += 1;
        }
        n.unkeep(&mut self.nodes)
    }

    /// We set up memory aliases by inserting special vars in the scope these
    /// variables are prefixed by $ so they cannot be referenced in Simple code.
    /// Using vars has the benefit that all the existing machinery of scoping
    /// and phis work as expected
    fn mem_alias_lookup(&mut self, alias: u32) -> Result<Node, ()> {
        let name = self.types.get_str(&Self::mem_name(alias));
        self.scope.lookup(name, &mut self.nodes)
    }

    fn mem_alias_update(&mut self, alias: u32, st: Node) -> Result<Node, ()> {
        let name = self.types.get_str(&Self::mem_name(alias));
        self.scope.update(name, st, &mut self.nodes)
    }

    pub(crate) fn mem_name(alias: u32) -> String {
        format!("${alias}")
    }

    /// Parse postfix expression. For now this is just a field
    /// expression, but in future could be array index too.
    ///
    /// <pre>
    ///     expr ('.' IDENTIFIER)* [ = expr ]
    /// </pre>
    fn parse_postfix(&mut self, expr: Node) -> PResult<Node> {
        if !self.match_(".") {
            return Ok(expr);
        }

        let expr_ty = self.nodes.ty[expr].unwrap();
        let Some(ptr) = expr_ty.to_mem_ptr() else {
            return Err(format!(
                "Expected struct reference but got {}",
                expr_ty.str()
            ));
        };
        let name = self.types.get_str(self.require_id()?);

        let Some(idx) = ptr.data().to.fields().iter().position(|&f| f.0 == name) else {
            return Err(format!(
                "Accessing unknown field '{name}' from '{}'",
                expr_ty.str()
            ));
        };

        let alias = self.nodes[self.nodes.start]
            .alias_starts
            .get(ptr.data().to.name())
            .unwrap()
            + idx as u32;

        let old = self.lexer.remaining;
        if self.match_("=") {
            // Disambiguate "obj.fld==x" boolean test from "obj.fld=x" field assignment

            if self.peek('=') {
                self.lexer.remaining = old;
            } else {
                let val = self.parse_expression()?;
                let mem_slice = self.mem_alias_lookup(alias).unwrap();
                let store = Store::new(
                    name,
                    alias,
                    [self.ctrl(), mem_slice, expr, val],
                    &mut self.nodes,
                )
                .peephole(&mut self.nodes);
                self.mem_alias_update(alias, store).unwrap();
                return Ok(expr); // "obj.a = expr" returns the expression while updating memory
            }
        }

        let declared_type = ptr.data().to.fields()[idx].1;
        let mem_slice = self.mem_alias_lookup(alias).unwrap();
        let load = Load::new(
            name,
            alias,
            declared_type,
            [mem_slice, expr],
            &mut self.nodes,
        )
        .peephole(&mut self.nodes);
        self.parse_postfix(load)
    }

    /// <pre>
    ///     integerLiteral: [1-9][0-9]* | [0]
    ///     floatLiteral: [digits].[digits]?[e [digits]]?
    /// </pre>
    fn parse_literal(&mut self) -> PResult<Constant> {
        self.lexer.parse_number(self.types).map(|ty| self.con(ty))
    }

    fn int_con(&mut self, con: i64) -> Constant {
        let ty = self.types.get_int(con);
        self.con(ty)
    }

    fn con(&mut self, t: Ty<'t>) -> Constant {
        Constant::new(t, &mut self.nodes)
            .peephole(&mut self.nodes)
            .to_constant(&self.nodes)
            .unwrap()
    }

    fn peep(&mut self, n: Node) -> Node {
        // Peephole, then improve with lexically scoped guards
        self.scope
            .upcast_guard(n.peephole(&mut self.nodes), &mut self.nodes)
    }

    //
    // Utilities for lexical analysis
    //

    /// Return true and skip if "syntax" is next in the stream.
    fn match_(&mut self, prefix: &str) -> bool {
        self.lexer.match_(prefix)
    }

    /// Match must be exact and not followed by more ID characters.
    /// Prevents identifier "ifxy" from matching an "if" statement.
    fn matchx(&mut self, prefix: &str) -> bool {
        self.lexer.matchx(prefix)
    }

    fn match_opx(&mut self, c: [u8; 2]) -> bool {
        self.lexer.match_opx(c)
    }

    /// Return true and do NOT skip if `c` is next
    fn peek(&mut self, c: char) -> bool {
        self.lexer.peek_char(c)
    }

    fn pos(&mut self) -> &'s str {
        self.lexer.remaining
    }
    fn set_pos(&mut self, pos: &'s str) {
        self.lexer.remaining = pos;
    }

    /// Require and return an identifier
    fn require_id(&mut self) -> PResult<&'s str> {
        let id = self.lexer.match_id();
        id.filter(|i| !is_keyword(i))
            .ok_or_else(|| format!("Expected an identifier, found '{}'", id.unwrap_or("null")))
    }

    fn require(&mut self, syntax: &str) -> PResult<()> {
        self.lexer
            .match_(syntax)
            .then_some(())
            .ok_or_else(|| self.error_syntax(syntax))
    }

    fn error_syntax(&mut self, syntax: &str) -> ParseErr {
        format!(
            "Syntax error, expected {syntax}: {}",
            self.lexer.get_any_next_token()
        )
    }
}

struct Lexer<'a> {
    /// The entire input
    source: &'a str,
    /// The remaining input string
    remaining: &'a str,
}

fn is_number(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_id_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_id_letter(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_punctuation(c: char) -> bool {
    "=;[]<>()+-/*".contains(c)
}

impl<'a> Lexer<'a> {
    fn is_eof(&self) -> bool {
        self.remaining.is_empty()
    }

    fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    // Just crash if misused
    fn peek_offset(&self, offset: isize) -> u8 {
        debug_assert_eq!(
            self.source[self.remaining.len()..].as_ptr(),
            self.remaining.as_ptr()
        );
        self.source.as_bytes()[self.remaining.len().checked_add_signed(offset).unwrap()]
    }

    fn next_char(&mut self) -> Option<char> {
        self.peek()
            .inspect(|c| self.remaining = &self.remaining[c.len_utf8()..])
    }

    fn is_whitespace(&self) -> bool {
        self.peek().as_ref().is_some_and(char::is_ascii_whitespace)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.is_whitespace() {
                self.next_char();
            } else if self.remaining.starts_with("//") {
                if let Some(i) = self.remaining.find('\n') {
                    self.remaining = &self.remaining[i + 1..];
                } else {
                    self.remaining = "";
                }
            } else {
                break;
            }
        }
    }

    /// Next non-white-space character, or EOF
    fn next_x_char(&mut self) -> Option<char> {
        self.skip_whitespace();
        self.next_char()
    }

    /// skips a prefix if present
    fn skip(&mut self, prefix: &str) -> bool {
        self.remaining
            .strip_prefix(prefix)
            .map(|rest| self.remaining = rest)
            .is_some()
    }

    /// skip whitespace, then try to skip a prefix
    fn match_(&mut self, prefix: &str) -> bool {
        self.skip_whitespace();
        self.skip(prefix)
    }

    /// skip whitespace, then try to skip a prefix that is not followed by an id letter
    fn matchx(&mut self, prefix: &str) -> bool {
        self.skip_whitespace();
        let backup = self.remaining;
        if !self.skip(prefix) {
            return false;
        }
        if !self.peek().is_some_and(is_id_letter) {
            return true;
        }
        self.remaining = backup;
        false
    }

    fn match_opx(&mut self, c: [u8; 2]) -> bool {
        self.skip_whitespace();
        if self.remaining.as_bytes().starts_with(&c) {
            self.remaining = &self.remaining[2..];
            true
        } else {
            false
        }
    }

    fn peek_char(&mut self, c: char) -> bool {
        self.skip_whitespace();
        self.peek() == Some(c)
    }

    fn peek_is_id(&mut self) -> bool {
        self.skip_whitespace();
        self.peek().is_some_and(is_id_start)
    }

    fn match_id(&mut self) -> Option<&'a str> {
        self.peek_is_id().then(|| self.parse_id())
    }

    // used for errors
    fn get_any_next_token(&mut self) -> &'a str {
        match self.peek() {
            None => "",
            Some(c) if is_id_start(c) => self.parse_id(),
            Some(c) if is_number(c) => self.parse_number_string(),
            Some(c) if is_punctuation(c) => self.parse_punctuation(),
            Some(c) => &self.remaining[..c.len_utf8()],
        }
    }

    fn peek_number(&self) -> bool {
        self.peek().is_some_and(is_number)
    }

    fn parse_number<'t>(&mut self, types: &Types<'t>) -> PResult<Ty<'t>> {
        let snum = self.parse_number_string();

        if snum.len() > 1 && snum.starts_with("0") {
            Err("Syntax error: integer values cannot start with '0'".to_string())
        } else {
            snum.parse().map(|i| types.get_int(i)).map_err(|_| {
                format!("{snum} could not be parsed to a positive signed 64 bit integer")
            })
        }
    }

    fn parse_number_string(&mut self) -> &'a str {
        let old = self.remaining;
        self.remaining = self.remaining.trim_start_matches(is_number);
        &old[..old.len() - self.remaining.len()]
    }

    fn parse_id(&mut self) -> &'a str {
        let old = self.remaining;
        self.remaining = self.remaining.trim_start_matches(is_id_letter);
        &old[..old.len() - self.remaining.len()]
    }

    fn parse_punctuation(&mut self) -> &'a str {
        let (p, i) = self.remaining.split_at(1);
        self.remaining = i;
        p
    }
}
