//! Generate a pseudo random script.
//! This generator will generate the same script for two random number generators initialized with the same seed.
//! To generate valid code it implements the parser but instead of parsing it emits the statements and expressions.
//! It is guaranteed to terminate.

use arbitrary::{Arbitrary, Unstructured};
use simple_rust::sea_of_nodes::nodes::ScopeNode;
use simple_rust::sea_of_nodes::parser::is_keyword;

#[derive(Debug)]
pub struct GeneratedScript {
    pub source: String,
    pub maybe_invalid: bool,
}

impl<'a> Arbitrary<'a> for GeneratedScript {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let valid = u.arbitrary()?;
        ScriptGenerator::run(u, valid)
    }
}

/// Number of spaces per indentation
const INDENTATION: usize = 4;

/// Valid characters for identifiers. The last 10 are not valid for the first character.
const VAR_CHARS: &[u8; 63] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

/// Flag for a statement that it does not pass on control flow to the next statement.
const FLAG_STOP: u32 = 0x01;

/// Flag for a statement that it contains a final if statement without an else branch.
const FLAG_IF_WITHOUT_ELSE: u32 = 0x02;

/// Binary operators.
const BINARY_OP: [&str; 10] = ["==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/"];

/// Unary operators.
const UNARY_OP: [&str; 1] = ["-"];

pub struct ScriptGenerator<'a, 'b> {
    /// The random number generator used for random decisions.
    random: &'a mut Unstructured<'b>,

    /// Indentation depth.
    indentation: usize,

    /// Output string builder
    sb: String,

    /// Number of nested loops
    loop_depth: usize,

    /// Current scope start in the variables list.
    curr_scope_start: usize,

    /// The depth of blocks allowed.
    depth: usize,

    /// The depth of expressions allowed.
    expr_depth: usize,

    /// Current variables in scope.
    variables: Vec<String>,

    /// If this needs to generate valid scripts or is also allowed to generate invalid ones.
    generate_valid: bool,

    /// If this script might be invalid.
    maybe_invalid: bool,
}

impl<'a, 'b> ScriptGenerator<'a, 'b> {
    /// * `generate_valid` - if the script should be guaranteed valid or if it is allowed to contain bugs.
    pub fn run(random: &'a mut Unstructured<'b>, generate_valid: bool) -> arbitrary::Result<GeneratedScript> {
        let mut generator = Self {
            random,
            indentation: 0,
            sb: String::new(),
            loop_depth: 0,
            curr_scope_start: 0,
            depth: 7,
            expr_depth: 3,
            variables: vec![],
            generate_valid,
            maybe_invalid: false,
        };
        generator.gen_program()?;
        Ok(GeneratedScript {
            source: generator.sb,
            maybe_invalid: generator.maybe_invalid,
        })
    }

    /// Random number with log distribution
    /// * `max` - The maximum value
    /// Returns a random number in the range `[0, max)` with higher numbers being more uncommon.
    fn rand_log(&mut self, mut max: usize) -> arbitrary::Result<usize> {
        max -= 1;
        let n = 1usize << max;
        let num = self.random.int_in_range(0..=(n + 1))?;
        for i in (0..max).rev() {
            if (num & (1usize << i)) != 0 { return Ok(max - i - 1); };
        }
        Ok(max)
    }

    /// Helper function to print indentation.
    fn print_indentation(&mut self) {
        self.sb.extend([' '; INDENTATION])
    }

    /// Generate a random name for variables. This might be invalid when it is a keyword.
    fn get_random_name(&mut self) -> arbitrary::Result<String> {
        let len = self.random.int_in_range(1..=10)?;
        let mut sb = String::with_capacity(len);
        sb.push(*self.random.choose(&VAR_CHARS[..VAR_CHARS.len() - 10])? as char);
        for _ in 1..len {
            sb.push(*self.random.choose(VAR_CHARS)? as char);
        }
        Ok(sb)
    }

    /// Generate a variable name. This might be new random name or a name from an outer scope.
    /// If the program is allowed to be invalid this can return keywords and already used variable names in the current scope.
    fn get_var_name(&mut self) -> arbitrary::Result<String> {
        if self.curr_scope_start > 0 && self.random.int_in_range(0..=9)? > 7 {
            // Use a variable outside the current scope.

            let v = self.random.choose(&self.variables[..self.curr_scope_start])?;
            if self.variables.iter().rposition(|x| x == v).unwrap() < self.curr_scope_start {
                return Ok(v.clone());
            }
            if !self.generate_valid {
                self.maybe_invalid = true;
                return Ok(v.clone());
            }
        }

        // Generate a new random variable name
        let mut v = self.get_random_name()?;
        if !self.generate_valid && (is_keyword(&v) || self.variables.iter().rposition(|x| x == &v).is_some_and(|i| i >= self.curr_scope_start)) {
            self.maybe_invalid = true;
            return Ok(v);
        }
        while is_keyword(&v) || self.variables.iter().rposition(|x| x == &v).is_some_and(|i| i >= self.curr_scope_start) {
            v.push(*self.random.choose(VAR_CHARS)? as char);
        }
        Ok(v)
    }

    /// Generates a program and writes it into the string builder supplied in the constructor.
    /// @return If the program is allowed to be invalid returns if something potentially invalid was generated.
    fn gen_program(&mut self) -> arbitrary::Result<()> {
        self.variables.push(ScopeNode::ARG0.to_string());
        self.curr_scope_start = self.variables.len();
        if (self.gen_statements()? & FLAG_STOP) == 0 {
            if self.generate_valid || self.random.int_in_range(0..=9)? > 7 {
                self.gen_return()?;
            } else {
                self.maybe_invalid = true;
            }
        }
        Ok(())
    }

    /// Generate a list of statements.
    /// @return flags FLAG_STOP and FLAG_IF_WITHOUT_ELSE for the last statement generated
    fn gen_statements(&mut self) -> arbitrary::Result<u32> {
        let num = self.random.int_in_range(0..=9)?;
        for _ in 0..num {
            self.print_indentation();
            let stop = self.gen_statement()?;
            self.sb.push('\n');
            if (stop & FLAG_STOP) != 0 {
                return Ok(stop);
            }
        }
        Ok(0)
    }

    /// Generate a single statement
    /// @return flags FLAG_STOP and FLAG_IF_WITHOUT_ELSE for the generated statement
    fn gen_statement(&mut self) -> arbitrary::Result<u32> {
        match self.random.int_in_range(0..=9)? {
            1 => self.gen_block(),
            2 => self.gen_if(),
            3 => self.gen_while(),
            4 | 5 => self.gen_decl(),
            6 | 7 | 8 => self.gen_assignment(),
            _ => self.gen_exit(),
        }
    }

    /// Generate a statement for if, else and while blocks.
    /// This is special since it disallows declarations and prefers to generate blocks.
    /// @return flags FLAG_STOP and FLAG_IF_WITHOUT_ELSE for the generated statement
    fn gen_statement_block(&mut self) -> arbitrary::Result<u32> {
        self.indentation += INDENTATION;
        let res: u32;
        if self.depth == 0 {
            res = if self.random.arbitrary()? {
                self.gen_assignment()?
            } else {
                self.gen_exit()?
            };
        } else {
            self.depth -= 1;
            res = match self.random.int_in_range(0..=9)? {
                1..=5 => {
                    self.depth += 1;
                    self.indentation -= INDENTATION;
                    let ret = self.gen_block()?;
                    self.indentation += INDENTATION;
                    self.depth -= 1;
                    ret
                }
                6 => self.gen_if()?,
                7 => self.gen_while()?,
                8 => self.gen_assignment()?,
                _ => self.gen_exit()?,
            };
            self.depth += 1;
        }
        self.indentation -= INDENTATION;
        Ok(res)
    }

    /// Generate an exit. This can be return or break and continue when in a loop.
    /// @return FLAG_STOP
    fn gen_exit(&mut self) -> arbitrary::Result<u32> {
        if self.loop_depth == 0 && self.generate_valid {
            return self.gen_return();
        }
        match self.random.int_in_range(0..=6)? {
            0 | 1 | 2 => {
                if self.loop_depth == 0 { self.maybe_invalid = true; }
                self.sb.push_str("continue;");
                Ok(FLAG_STOP)
            }
            3 | 4 | 5 => {
                if self.loop_depth == 0 { self.maybe_invalid = true; }
                self.sb.push_str("break;");
                Ok(FLAG_STOP)
            }
            _ => self.gen_return(),
        }
    }

    /// Generate a block statement. After a certain depth only empty blocks are generated to ensure termination of the generator.
    /// @return flag FLAG_STOP for the generated statement
    fn gen_block(&mut self) -> arbitrary::Result<u32> {
        if self.depth == 0 {
            self.sb.push_str("{}");
            return Ok(0);
        }
        self.depth -= 1;
        let old_css = self.curr_scope_start;
        self.curr_scope_start = self.variables.len();
        self.sb.push_str("{\n");
        self.indentation += INDENTATION;
        let stop = self.gen_statements()?;
        self.indentation -= INDENTATION;
        self.print_indentation();
        self.sb.push('}');
        while self.variables.len() > self.curr_scope_start {
            self.variables.pop();
        }
        self.curr_scope_start = old_css;
        self.depth += 1;
        Ok(stop & !FLAG_IF_WITHOUT_ELSE)
    }

    /// Generate an if statement with an optional else block.
    /// @return flags FLAG_STOP and FLAG_IF_WITHOUT_ELSE for the generated statement
    fn gen_if(&mut self) -> arbitrary::Result<u32> {
        self.sb.push_str("if(");
        self.gen_expression()?;
        self.sb.push_str(") ");
        let mut stop = self.gen_statement_block()?;
        if (stop & FLAG_IF_WITHOUT_ELSE) == 0 && self.random.int_in_range(0..=9)? > 3 {
            self.sb.push_str("\n");
            self.print_indentation();
            self.sb.push_str("else ");
            stop &= self.gen_statement_block()?;
        } else {
            stop = FLAG_IF_WITHOUT_ELSE;
        }
        Ok(stop)
    }

    /// Generate a while loop.
    /// @return 0
    fn gen_while(&mut self) -> arbitrary::Result<u32> {
        self.sb.push_str("while(");
        self.gen_expression()?;
        self.sb.push_str(") ");
        self.loop_depth += 1;
        self.gen_statement_block()?;
        self.loop_depth -= 1;
        Ok(0)
    }


    /// Generate a declaration statement.
    /// @return 0
    fn gen_decl(&mut self) -> arbitrary::Result<u32> {
        let name = self.get_var_name()?.to_string();
        self.sb.push_str("int ");
        self.sb.push_str(&name);
        self.sb.push_str("=");
        self.gen_expression()?;
        self.sb.push_str(";");
        self.variables.push(name.into());
        Ok(0)
    }


    /// Generate an assignment statement.
    /// @return 0
    fn gen_assignment(&mut self) -> arbitrary::Result<u32> {
        if self.variables.is_empty() { return self.gen_decl(); }
        self.gen_variable()?;
        self.sb.push_str("=");
        self.gen_expression()?;
        self.sb.push_str(";");
        Ok(0)
    }


    /// Generate a return statement.
    /// @return FLAG_STOP
    fn gen_return(&mut self) -> arbitrary::Result<u32> {
        self.sb.push_str("return ");
        self.gen_expression()?;
        self.sb.push_str(";");
        Ok(FLAG_STOP)
    }

    /// Generate a binary expression.
    /// This method does not care about operator precedence.
    fn gen_expression(&mut self) -> arbitrary::Result<()> {
        let mut num = self.rand_log(10)?;
        while num > 0 {
            num -= 1;
            self.gen_unary()?;
            self.sb.push_str(self.random.choose(&BINARY_OP)?);
        }
        self.gen_unary()
    }


    /// Generate a unary expression.
    fn gen_unary(&mut self) -> arbitrary::Result<()> {
        let mut num = self.rand_log(6)?;
        while num > 0 {
            num -= 1;
            self.sb.push_str(self.random.choose(&UNARY_OP)?);
        }
        self.gen_primary()
    }


    /// Generate a primary expression.
    fn gen_primary(&mut self) -> arbitrary::Result<()> {
        match self.random.int_in_range(0..=9)? {
            0 => self.sb.push_str("true"),
            1 => self.sb.push_str("false"),
            2 => {
                if self.expr_depth == 0 {
                    // Ensure termination of the generator.
                    if self.random.arbitrary()? {
                        self.gen_number()?;
                    } else {
                        self.gen_variable()?;
                    }
                } else {
                    self.sb.push_str("(");
                    self.expr_depth -= 1;
                    self.gen_expression()?;
                    self.expr_depth += 1;
                    self.sb.push_str(")");
                }
            }
            3..=6 => self.gen_number()?,
            _ => self.gen_variable()?,
        }
        Ok(())
    }


    /// Generate a number.
    fn gen_number(&mut self) -> arbitrary::Result<()> {
        use std::fmt::Write;
        write!(self.sb, "{}", self.random.int_in_range(0..=99)?).unwrap();
        Ok(())
    }


    /// Generate a variable. If there are none generate a number instead.
    /// If the program is allowed to be invalid a random name can be generated in some cases.
    fn gen_variable(&mut self) -> arbitrary::Result<()> {
        if !self.generate_valid && self.random.int_in_range(0..=99)? > 97 {
            let name = self.get_random_name()?;
            self.sb.push_str(&name);
            self.maybe_invalid = true;
            return Ok(());
        }
        if self.variables.is_empty() {
            self.gen_number()?;
        } else {
            self.sb.push_str(self.random.choose(&self.variables)?);
        }
        Ok(())
    }
}