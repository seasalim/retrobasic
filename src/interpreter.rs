use std::fmt;
use std::io::{self, Write};
use std::collections::HashMap;
use std::str::FromStr;
use rand;
use ast::AST;
use error::{Error, Result};
use tokenid::TID;
use dimsum::MultiDim;

// Memory value
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum MV {
    Str(String),
    Num(f32),
}

impl fmt::Display for MV {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match *self {
            MV::Num(ref n) => format!("{}", n),
            MV::Str(ref s) => format!("{}", s),
        };
        write!(f, "{}", msg)
    }
}

impl MV {
    pub fn to_num(&self) -> f32 {
        match *self {
            MV::Num(n) => n,
            MV::Str(ref s) => s.parse().expect("Expected a floating point"),
        }
    }

    pub fn to_str(&self) -> String {
        match *self {
            MV::Num(n) => format!("{}", n),
            MV::Str(ref s) => s.clone(),
        }
    }
}

// Target for next run execution
#[derive(Debug, PartialEq, Clone)]
enum Execute {
    Stop,
    Next,
    NextLine,
    Line(u32),
    Stmt(u32),
}

// Struct for FOR-NEXT scope
#[derive(Debug, Clone)]
struct ForNext {
    end: f32,
    step: f32,
    target: u32,
    id: String,
}

// Main interpreter
pub struct Interpreter {
    vars: HashMap<String, MV>,
    md_vars: HashMap<String, MultiDim<MV>>,
    fn_vars: HashMap<String, MV>,
    fn_defs: HashMap<String, AST>,
    data: Vec<MV>,
    fornext: Vec<ForNext>,
    returns: Vec<u32>,
    curr_data: usize,
    curr_stmt: u32,
    line_num: u32,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            vars: HashMap::new(),
            md_vars: HashMap::new(),
            fn_defs: HashMap::new(),
            fn_vars: HashMap::new(),
            data: Vec::new(),
            fornext: Vec::new(),
            returns: Vec::new(),
            curr_data: 0,
            curr_stmt: 0,
            line_num: 0,
        }
    }

    fn var_value(&self, id: &str) -> Option<&MV> {
        if self.fn_vars.contains_key(id) {
            self.fn_vars.get(id)
        } else {
            self.vars.get(id)
        }
    }

    fn exec(&mut self, node: &AST) -> Result<Execute> {
        debug!("exec     : {:?}: {}", node.line_num, node);
        if node.line_num.is_some() {
            self.line_num = node.line_num.unwrap();
        }
        match node.token.id {
            TID::DEF => self.exec_def(&node),
            TID::LET => self.exec_let(&node.children[0], &node.children[1]),
            TID::IF => self.exec_if(&node.children[0], &node.children[1]),
            TID::FOR => self.exec_for(
                &node.children[0],
                &node.children[1],
                &node.children[2],
                &node.children[3],
            ),
            TID::NEXT => self.exec_next(&node.children),
            TID::PRINT => self.exec_print(&node.children),
            TID::GOTO | TID::LNUM => self.exec_goto(&node.children[0]),
            TID::INPUT => self.exec_input(&node.children),
            TID::DIM => self.exec_dim(&node.children),
            TID::END | TID::STOP => self.exec_end(),
            TID::ON => self.exec_on(&node),
            TID::DATA => self.exec_data(&node),
            TID::READ => self.exec_read(&node),
            TID::GOSUB => self.exec_gosub(&node),
            TID::RETURN => self.exec_return(&node),
            TID::RESTORE => self.exec_restore(&node),
            TID::REM => Ok(Execute::Next),
            _ => {
                debug!("Unexpected token type: {:?}", node.token.id);
                Err(Error::RuntimeError(
                    self.line_num,
                    "UNEXPECTED TOKEN".to_string(),
                ))
            }
        }
    }

    fn exec_on(&mut self, node: &AST) -> Result<Execute> {
        let v = self.into_dim(&node.children)?;
        if (v[0] > 0) && (v[0] < v.len()) {
            Ok(Execute::Line(v[v[0]] as u32))
        } else {
            Ok(Execute::Next)
        }
    }

    fn exec_data(&mut self, node: &AST) -> Result<Execute> {
        for c in &node.children {
            let mv = self.eval(&c)?;
            self.data.push(mv);
        }
        Ok(Execute::Next)
    }

    fn exec_restore(&mut self, _: &AST) -> Result<Execute> {
        self.curr_data = 0;
        Ok(Execute::Next)
    }

    fn exec_read(&mut self, node: &AST) -> Result<Execute> {
        for c in &node.children {
            if self.curr_data >= self.data.len() {
                debug!("Ran out of available DATA");
                return Err(Error::RuntimeError(
                    self.line_num,
                    "OUT OF DATA".to_string(),
                ));
            }

            let mv = self.data[self.curr_data].clone();
            self.curr_data = self.curr_data + 1;

            self.do_assign(&c, mv)?;
        }
        Ok(Execute::Next)
    }

    fn exec_end(&self) -> Result<Execute> {
        Ok(Execute::Stop)
    }

    fn exec_goto(&mut self, ast: &AST) -> Result<Execute> {
        let n: u32 = self.eval(ast)?.to_num() as u32;
        Ok(Execute::Line(n))
    }

    fn exec_gosub(&mut self, ast: &AST) -> Result<Execute> {
        let n: u32 = self.eval(&ast.children[0])?.to_num() as u32;
        self.returns.push(self.curr_stmt + 1);
        Ok(Execute::Line(n))
    }

    fn exec_return(&mut self, _: &AST) -> Result<Execute> {
        let n: u32 = self.returns.pop().unwrap();
        Ok(Execute::Stmt(n))
    }

    fn into_dim(&mut self, nodes: &Vec<AST>) -> Result<Vec<usize>> {
        Ok(nodes
            .iter()
            .map(|x| self.eval(x).unwrap_or(MV::Num(0.0)).to_num() as usize)
            .collect())
    }

    fn exec_dim(&mut self, nodes: &Vec<AST>) -> Result<Execute> {
        for i in 0..nodes.len() {
            let id = nodes[i].clone().token.val.unwrap();

            let mv = if id.contains("$") {
                MV::Str(String::from(""))
            } else {
                MV::Num(0.0)
            };

            let dim = self.into_dim(&nodes[i].children)?;
            let v = MultiDim::<MV>::new(&dim, mv);

            self.md_vars.insert(id, v);
        }

        Ok(Execute::Next)
    }

    fn read_line(&self) -> String {
        let mut s = String::new();
        io::stdin().read_line(&mut s).unwrap();
        String::from(s.clone().trim())
    }

    fn exec_input(&mut self, nodes: &Vec<AST>) -> Result<Execute> {
        let mut start: usize = 0;
        let end: usize = nodes.len();

        // Print out a prompt (if applicable)
        if nodes[0].token.id == TID::STR {
            start = 1;
            print!("{}", &nodes[0].token);
        }

        // Get valid inputs
        loop {
            print!("? ");
            io::stdout().flush().unwrap();
            let line = self.read_line();
            let split: Vec<&str> = line.split(",").collect();
            if split.len() != (end - start) {
                println!(
                    "INVALID INPUT! ENTER {} COMMA-SEPARATED VALUE(S)",
                    end - start
                );
                continue;
            }
            let mut valid: bool = true;
            for i in 0..split.len() {
                match nodes[i + start].token.val {
                    Some(ref s) => {
                        if s.contains("$") {
                            let val = MV::Str(split[i].trim().to_string().to_uppercase());
                            self.do_assign(&nodes[i + start], val)?;
                        } else {
                            match split[i].trim().parse::<f32>() {
                                Ok(n) => {
                                    let val = MV::Num(n);
                                    self.do_assign(&nodes[i + start], val)?;
                                }
                                Err(_) => {
                                    println!("INVALID INPUT! '{}' IS NOT A NUMBER", split[i]);
                                    valid = false;
                                }
                            }
                        }
                    }
                    None => (),
                }
            }
            if valid {
                break;
            }
        }
        Ok(Execute::Next)
    }

    fn do_assign(&mut self, id_ast: &AST, mv: MV) -> Result<()> {
        let id: String = id_ast.token.val.clone().unwrap();

        if id_ast.token.id == TID::MDID {
            let dim = self.into_dim(&id_ast.children)?;

            let not_found = match self.md_vars.get_mut(&id) {
                Some(md) => {
                    match md.set(&dim, mv.clone()) {
                        Ok(_) => (),
                        Err(e) => {
                            debug!("Error setting multi-dimensional value: {:?}", e);
                            return Err(Error::RuntimeError(
                                self.line_num,
                                "MULTIDIM ERROR".to_string(),
                            ));
                        }
                    };
                    false
                }
                None => true,
            };

            if not_found {
                let mv_def = if id.contains("$") {
                    MV::Str(String::from(""))
                } else {
                    MV::Num(0.0)
                };
                let mut md = MultiDim::<MV>::new(&[10], mv_def);
                md.set(&dim, mv).unwrap();
                self.md_vars.insert(id, md);
            }
        } else {
            self.vars.insert(id, mv);
        }

        Ok(())
    }

    fn exec_def(&mut self, ast: &AST) -> Result<Execute> {
        let id = ast.children[0].token.val.clone().unwrap();
        self.fn_defs.insert(id, ast.clone());
        Ok(Execute::Next)
    }

    fn exec_let(&mut self, left: &AST, right: &AST) -> Result<Execute> {
        let mv: MV = match right.token.id {
            TID::STR => MV::Str(right.token.val.clone().unwrap()),
            _ => self.eval(right)?,
        };

        self.do_assign(left, mv)?;
        Ok(Execute::Next)
    }

    fn exec_if(&mut self, left: &AST, right: &AST) -> Result<Execute> {
        let chk = self.eval(&left)?.to_num();
        if chk != 0.0 {
            return self.exec(&right);
        }
        Ok(Execute::NextLine)
    }

    fn exec_for(
        &mut self,
        id_ast: &AST,
        start_ast: &AST,
        end_ast: &AST,
        step_ast: &AST,
    ) -> Result<Execute> {
        let id = id_ast.token.val.clone().unwrap();
        let end = self.eval(&end_ast)?.to_num();
        let step = if step_ast.token.id == TID::NONE {
            1.0
        } else {
            self.eval(&step_ast)?.to_num()
        };
        self.exec_let(&id_ast, &start_ast)?;
        let v = ForNext {
            end: end,
            step: step,
            target: self.curr_stmt + 1,
            id: id,
        };
        debug!("exec_for : {:?}", v);
        self.fornext.push(v);
        Ok(Execute::Next)
    }

    fn eval_for_next(&mut self, frame: &ForNext) -> Result<Execute> {
        debug!("eval_for_next: {:?}", frame);

        let next_val = match self.vars.get(&frame.id) {
            Some(mv) => mv.to_num() + frame.step,
            None => {
                debug!("Expected id to be assigned");
                return Err(Error::RuntimeError(
                    self.line_num,
                    "VARIABLE UNASSIGNED".to_string(),
                ));
            }
        };

        // Update loop variable by incrementing it with step
        self.vars.insert(frame.id.clone(), MV::Num(next_val));

        if ((frame.step > 0.0) && (next_val <= frame.end))
            || ((frame.step < 0.0) && (next_val >= frame.end))
        {
            Ok(Execute::Stmt(frame.target))
        } else {
            Ok(Execute::Next)
        }
    }

    fn exec_next(&mut self, nodes: &Vec<AST>) -> Result<Execute> {
        // Rules:
        // If no for-next frames, bail
        // If no id specified, match the top-most frame
        // If one id specified, match a frame from the right
        // If multiple ids specified, match each in order from the right
        if self.fornext.is_empty() {
            debug!("No for-next frames!");
            return Err(Error::RuntimeError(
                self.line_num,
                "FOR-NEXT ERROR".to_string(),
            ));
        }

        let mut curr_idx = 0;
        loop {
            let curr_id = if nodes.is_empty() {
                self.fornext[self.fornext.len() - 1].id.clone()
            } else {
                nodes[curr_idx].token.val.clone().unwrap()
            };

            // Try to find a frame matching this id, searching from the right
            match self.fornext.iter().rposition(|ref f| f.id == curr_id) {
                Some(fr_idx) => {
                    // Ok, we found a matching frame
                    let frame = self.fornext[fr_idx].clone();
                    let exec = self.eval_for_next(&frame)?;
                    if exec == Execute::Next {
                        self.fornext.remove(fr_idx); // We are done with this loop index
                    }
                    return Ok(exec);
                }
                None => {
                    // Check the next id in the list
                    curr_idx = curr_idx + 1;
                    if curr_idx >= nodes.len() {
                        debug!("Did not find any matching ids");
                        return Err(Error::RuntimeError(
                            self.line_num,
                            "FOR-NEXT ERROR".to_string(),
                        ));
                    }
                }
            }
        }
    }

    fn exec_print(&mut self, nodes: &Vec<AST>) -> Result<Execute> {
        let mut newline = true;
        for i in 0..nodes.len() as usize {
            match nodes[i].token.id {
                TID::COMMA => {
                    newline = false;
                    print!(" ")
                }
                TID::SEMICOLON => {
                    newline = false;
                    print!("")
                }
                _ => {
                    newline = true;
                    match self.eval(&nodes[i])? {
                        MV::Num(n) => print!(" {} ", n),
                        MV::Str(ref s) => print!("{}", s),
                    };
                }
            }
        }
        if newline {
            println!("");
        }
        Ok(Execute::Next)
    }

    fn eval(&mut self, node: &AST) -> Result<MV> {
        debug!("eval: {:?}", node);
        match node.token.id {
            TID::NL => Ok(MV::Str(String::from(""))),
            TID::ID => {
                let id: String = node.token.val.clone().unwrap();
                match self.var_value(&id) {
                    Some(mv) => Ok(mv.clone()),
                    None => Ok(MV::Num(0.0)),
                }
            }
            TID::MDID => self.eval_mdid(&node),
            TID::INT | TID::REAL => match f32::from_str(&node.token.val.clone().unwrap()) {
                Ok(n) => Ok(MV::Num(n)),
                Err(e) => {
                    debug!("Failed to parse node value, {:?}", e);
                    Err(Error::RuntimeError(
                        self.line_num,
                        "INVALID NUMBER".to_string(),
                    ))
                }
            },
            TID::STR => Ok(MV::Str(node.token.val.clone().unwrap())),
            TID::POW => self.eval_pow(&node.children[0], &node.children[1]),
            TID::PLUS => self.eval_plus(&node.children[0], &node.children[1]),
            TID::MINUS => {
                if node.children.len() == 1 {
                    self.eval_negate(&node.children[0])
                } else {
                    self.eval_minus(&node.children[0], &node.children[1])
                }
            }
            TID::MULT => self.eval_mult(&node.children[0], &node.children[1]),
            TID::DIV => self.eval_div(&node.children[0], &node.children[1]),
            TID::OR => self.eval_or(&node.children[0], &node.children[1]),
            TID::AND => self.eval_and(&node.children[0], &node.children[1]),
            TID::NOT => self.eval_not(&node.children[0]),
            TID::EQ => self.eval_eq(&node.children[0], &node.children[1]),
            TID::NEQ => self.eval_neq(&node.children[0], &node.children[1]),
            TID::GT => self.eval_gt(&node.children[0], &node.children[1]),
            TID::GTEQ => self.eval_gteq(&node.children[0], &node.children[1]),
            TID::LT => self.eval_lt(&node.children[0], &node.children[1]),
            TID::LTEQ => self.eval_lteq(&node.children[0], &node.children[1]),
            TID::SEMICOLON => self.eval_semicolon(&node.children[0], &node.children[1]),

            TID::RND => self.eval_rnd(&node.children[0]),
            TID::ABS => self.eval_abs(&node.children[0]),
            TID::ATN => self.eval_atn(&node.children[0]),
            TID::COS => self.eval_cos(&node.children[0]),
            TID::EXP => self.eval_exp(&node.children[0]),
            TID::FINT => self.eval_fint(&node.children[0]),
            TID::SGN => self.eval_sgn(&node.children[0]),
            TID::SQR => self.eval_sqr(&node.children[0]),
            TID::SIN => self.eval_sin(&node.children[0]),
            TID::TAN => self.eval_tan(&node.children[0]),
            TID::TAB => self.eval_tab(&node.children[0]),

            TID::ASC => self.eval_asc(&node.children[0]),
            TID::CHR => self.eval_chr(&node.children[0]),
            TID::LEFT => self.eval_left(&node.children[0], &node.children[1]),
            TID::RIGHT => self.eval_right(&node.children[0], &node.children[1]),
            TID::MID => self.eval_mid(&node),
            TID::LEN => self.eval_len(&node.children[0]),
            TID::VAL => self.eval_val(&node.children[0]),
            TID::FSTR => self.eval_fstr(&node),

            TID::LNUM => self.eval_lnum(&node.children[0]),
            TID::FN => self.eval_fn(&node),
            _ => {
                debug!("Unexpected eval, got {:?}", node.token.id);
                Err(Error::RuntimeError(
                    self.line_num,
                    "UNEXPECTED EVALUATION".to_string(),
                ))
            }
        }
    }

    fn eval_fn(&mut self, node: &AST) -> Result<MV> {
        let fn_id = node.token.val.clone().unwrap();
        let fn_def = self.fn_defs.get(&fn_id).unwrap().clone();

        self.fn_vars.clear();
        for i in 1..fn_def.children.len() - 1 {
            match fn_def.children[i].token.val {
                Some(ref var_id) => {
                    let val = self.eval(&node.children[i - 1])?;
                    self.fn_vars.insert(var_id.to_string(), val);
                }
                None => {
                    debug!("Did not find variable for {}!", fn_def.children[i]);
                }
            }
        }

        let res = self.eval(&fn_def.children[fn_def.children.len() - 1]);
        self.fn_vars.clear();
        res
    }

    fn eval_mdid(&mut self, node: &AST) -> Result<MV> {
        let id: String = node.token.val.clone().unwrap();
        let dim = self.into_dim(&node.children)?;

        match self.md_vars.get(&id) {
            Some(md) => match md.get(&dim) {
                Ok(mv) => Ok(mv.clone()),
                Err(e) => {
                    debug!("Error retrieving multi-dimensional value: {:?}", e);
                    Err(Error::RuntimeError(
                        self.line_num,
                        "MULTIDIM ERROR".to_string(),
                    ))
                }
            },
            None => {
                if id.contains("$") {
                    Ok(MV::Str(String::from("")))
                } else {
                    Ok(MV::Num(0.0))
                }
            }
        }
    }

    fn eval_lnum(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num()))
    }

    fn eval_asc(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_str().as_bytes()[0] as f32))
    }

    fn eval_chr(&mut self, ast: &AST) -> Result<MV> {
        let v = vec![self.eval(ast)?.to_num() as u8];
        Ok(MV::Str(String::from_utf8(v).unwrap()))
    }

    fn eval_left(&mut self, left: &AST, right: &AST) -> Result<MV> {
        let op = self.eval(left)?.to_str();
        let start = self.eval(right)?.to_num() as usize;
        if start <= op.len() {
            let (first, _) = op.split_at(start);
            Ok(MV::Str(String::from(first)))
        } else {
            Ok(MV::Str(String::from("")))
        }
    }

    fn eval_right(&mut self, left: &AST, right: &AST) -> Result<MV> {
        let op = self.eval(left)?.to_str();
        let start = op.len() - self.eval(right)?.to_num() as usize;
        if start <= op.len() {
            let (_, last) = op.split_at(start);
            Ok(MV::Str(String::from(last)))
        } else {
            Ok(MV::Str(String::from("")))
        }
    }

    fn eval_mid(&mut self, ast: &AST) -> Result<MV> {
        let op = self.eval(&ast.children[0])?.to_str();
        let start = self.eval(&ast.children[1])?.to_num() as usize;
        let len = if ast.children.len() > 2 {
            self.eval(&ast.children[2])?.to_num() as usize
        } else {
            op.len() - start
        };
        let (_, second) = op.split_at(start - 1);
        let s = String::from(second);
        let (first, _) = s.split_at(len);
        Ok(MV::Str(String::from(first)))
    }

    fn eval_len(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_str().len() as f32))
    }

    fn eval_val(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num()))
    }

    fn eval_fstr(&mut self, ast: &AST) -> Result<MV> {
        let s = self.eval(&ast.children[0])?.to_str();
        Ok(MV::Str(s))
    }

    fn eval_rnd(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(rand::random::<f32>() * self.eval(ast)?.to_num()))
    }

    fn eval_abs(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().abs()))
    }

    fn eval_sqr(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().sqrt()))
    }

    fn eval_sgn(&mut self, ast: &AST) -> Result<MV> {
        let n = self.eval(ast)?.to_num();
        let s = if n < 0.0 {
            -1.0
        } else if n > 0.0 {
            1.0
        } else {
            0.0
        };
        Ok(MV::Num(s))
    }

    fn eval_sin(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().sin()))
    }

    fn eval_cos(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().cos()))
    }

    fn eval_tan(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().tan()))
    }

    fn eval_exp(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().exp()))
    }

    fn eval_atn(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(ast)?.to_num().atan()))
    }

    fn eval_fint(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(self.eval(&ast)?.to_num().floor()))
    }

    fn eval_tab(&mut self, ast: &AST) -> Result<MV> {
        let n: u32 = self.eval(&ast)?.to_num() as u32;
        let s = (1..n).map(|_| " ").collect::<String>();
        Ok(MV::Str(s))
    }

    fn eval_semicolon(&mut self, left: &AST, right: &AST) -> Result<MV> {
        Ok(MV::Str(format!(
            "{} {}",
            self.eval(left)?.to_str(),
            self.eval(right)?.to_str()
        )))
    }

    fn eval_or(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if (self.eval(left)?.to_num() != 0.0) || (self.eval(right)?.to_num() != 0.0) {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_and(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if (self.eval(left)?.to_num() != 0.0) && (self.eval(right)?.to_num() != 0.0) {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_not(&mut self, ast: &AST) -> Result<MV> {
        if self.eval(ast)?.to_num() != 0.0 {
            Ok(MV::Num(0.0))
        } else {
            Ok(MV::Num(-1.0))
        }
    }

    fn eval_eq(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? == self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_neq(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? != self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_lt(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? < self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_lteq(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? <= self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_gt(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? > self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_gteq(&mut self, left: &AST, right: &AST) -> Result<MV> {
        if self.eval(left)? >= self.eval(right)? {
            Ok(MV::Num(-1.0))
        } else {
            Ok(MV::Num(0.0))
        }
    }

    fn eval_pow(&mut self, left: &AST, right: &AST) -> Result<MV> {
        Ok(MV::Num(
            self.eval(left)?.to_num().powf(self.eval(right)?.to_num()),
        ))
    }

    fn eval_negate(&mut self, ast: &AST) -> Result<MV> {
        Ok(MV::Num(-(self.eval(ast)?.to_num())))
    }

    fn eval_plus(&mut self, left: &AST, right: &AST) -> Result<MV> {
        let op1 = self.eval(left)?;
        let op2 = self.eval(right)?;
        match (op1, op2) {
            (MV::Str(ref s1), MV::Str(ref s2)) => Ok(MV::Str(format!("{}{}", s1, s2))),
            _ => Ok(MV::Num(
                self.eval(left)?.to_num() + (self.eval(right)?.to_num()),
            )),
        }
    }

    fn eval_minus(&mut self, left: &AST, right: &AST) -> Result<MV> {
        Ok(MV::Num(
            self.eval(left)?.to_num() - (self.eval(right)?.to_num()),
        ))
    }

    fn eval_mult(&mut self, left: &AST, right: &AST) -> Result<MV> {
        Ok(MV::Num(
            self.eval(left)?.to_num() * (self.eval(right)?.to_num()),
        ))
    }

    fn eval_div(&mut self, left: &AST, right: &AST) -> Result<MV> {
        Ok(MV::Num(
            self.eval(left)?.to_num() / (self.eval(right)?.to_num()),
        ))
    }

    // The AST passed to run must have the following format:
    //
    //                      <ROOT>
    //          ______________|____________________
    //         |              |                    |
    //      <AST>           <AST>   .....        <AST>
    //
    // where each AST corresponds to a single statement. Statements
    // that correspond to line numbers have optional line_num metadata
    // in the AST node that will be used for execution targeting.

    pub fn run(&mut self, ast: &AST) -> Result<()> {
        if ast.token.id != TID::ROOT {
            debug!("AST must always start with a root node!");
            return Err(Error::RuntimeError(
                self.line_num,
                "INVALID PROGRAM".to_string(),
            ));
        }

        debug!("Interpreter start, {} lines", ast.children.len());

        // Map line numbers to the child indices
        let mut target_map: HashMap<u32, u32> = HashMap::new();
        for i in 0..ast.children.len() {
            match ast.children[i].line_num {
                Some(n) => {
                    target_map.insert(n, i as u32);
                }
                None => (),
            }
        }

        // Pre-process all DATA Statements
        for i in 0..ast.children.len() {
            if ast.children[i].token.id == TID::DATA {
                self.exec(&ast.children[i])?;
            }
        }

        let mut curr = 0;
        while curr < ast.children.len() {
            self.curr_stmt = curr as u32;
            match self.exec(&ast.children[curr])? {
                Execute::Stop => break,
                Execute::Next => curr = curr + 1,
                Execute::NextLine => loop {
                    curr = curr + 1;
                    match ast.children[curr].line_num {
                        Some(_) => break,
                        None => (),
                    }
                },
                Execute::Stmt(n) => curr = n as usize,
                Execute::Line(n) => {
                    let ix = target_map.get(&n).unwrap();
                    curr = *ix as usize
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use interpreter::*;
    use parser::*;
    use lexer::*;

    #[test]
    fn can_execute_basic_statements() {
        let input = r#"10 LET A = 10 : LET B = 20
        20 PRINT A : PRINT B
        30 IF A < 5 THEN 20
        40 IF B > 5 THEN PRINT "FOO"
        50 END
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap().clone();
        let mut interpreter = Interpreter::new();
        interpreter.run(&ast).unwrap();
    }

    #[test]
    fn can_execute_expressions() {
        let input = r#"10 LET A = 10 + 5.5 - 3 / 2 ^ 4
        20 B = A AND 5 OR (2 - 1.0) < 14
        30 PRINT -1;2;-3
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap().clone();
        let mut interpreter = Interpreter::new();
        interpreter.run(&ast).unwrap();
    }

    #[test]
    fn can_execute_statements_with_optional_arguments() {
        let input = r#"10 PRINT : PRINT : PRINT
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap().clone();
        let mut interpreter = Interpreter::new();
        interpreter.run(&ast).unwrap();
    }

    #[test]
    fn can_execute_built_in_functions() {
        let input = r#"10 PRINT ABS(-1); SGN(-1); SIN(3.14); SQR(4)
        20 PRINT ATN(0); COS(3.14); EXP(1); TAN(3.14)
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap().clone();
        let mut interpreter = Interpreter::new();
        interpreter.run(&ast).unwrap();
    }
}
