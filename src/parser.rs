use ast::AST;
use error::{Error, Result};
use lexer::{Lexer, Token};
use std::str::FromStr;
use tokenid::TID;

macro_rules! ast {
    (
        $x:expr, $( $y:expr ),*
    ) => {{
        let t = Token::new($x, None);
        let mut a = AST::new(t);
        $(
            a.add_child($y);
        )*
        a
    }}
}

pub struct Parser {
    lex: Lexer,
    t: Token,
    line_num: u32,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        Parser {
            lex: l,
            t: Token::new(TID::NONE, None),
            line_num: 0,
        }
    }

    fn consume(&mut self) {
        self.t = self.lex.next_token();
        debug!("Parser::consume next token: {:?}", self.t);
    }

    fn la(&self) -> &Token {
        &self.t
    }

    fn match_token(&mut self, tid: TID) -> Result<AST> {
        if self.la().id != tid {
            debug!("Expected id: {:?}, got {:?}", tid, self.la().id);
            return Err(Error::SyntaxError(
                self.line_num,
                "UNEXPECTED TOKEN".to_string(),
            ));
        }
        let a = AST::new(self.la().clone());
        self.consume();
        Ok(a)
    }

    fn parse_linenum(&mut self) -> Result<u32> {
        let a = self.match_token(TID::INT)?;

        match u32::from_str(&a.token.val.unwrap()) {
            Ok(n) => {
                self.line_num = n;
                Ok(n)
            }
            Err(e) => {
                debug!("Failed to parse line num, {:?}", e);
                Err(Error::SyntaxError(
                    self.line_num,
                    "INVALID LINE NUMBER".to_string(),
                ))
            }
        }
    }

    fn parse_id(&mut self) -> Result<AST> {
        let mut a = self.match_token(TID::ID)?;

        let tid = self.la().id;
        match tid {
            TID::LBRK => {
                self.consume();
                a.token.id = TID::MDID;
                a.add_children(self.parse_expression_list()?);
                self.match_token(TID::RBRK)?;
            }
            _ => (),
        }
        Ok(a)
    }

    fn parse_end(&mut self) -> Result<AST> {
        self.match_token(TID::END)
    }

    fn parse_let(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::LET)?
            .add_child(self.parse_id()?)
            .nop(self.match_token(TID::EQ)?)
            .add_child(self.parse_expression()?)
            .finalize())
    }

    fn parse_input(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::INPUT)?
            .add_optional_child(self.parse_optional_prompt()?)
            .add_children(self.parse_id_list()?)
            .finalize())
    }

    fn parse_optional_prompt(&mut self) -> Result<Option<AST>> {
        match self.la().id {
            TID::STR => {
                let a = self.match_token(TID::STR)?
                    .nop(self.match_token(TID::SEMICOLON)?)
                    .finalize();
                Ok(Some(a))
            }
            _ => Ok(None),
        }
    }

    fn parse_if(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::IF)?
            .add_child(self.parse_expression()?)
            .nop(self.match_token(TID::THEN)?)
            .add_child(self.parse_then()?)
            .finalize())
    }

    fn parse_then(&mut self) -> Result<AST> {
        match self.la().id {
            TID::INT => Ok(AST::new(Token::new(TID::LNUM, None))
                .add_child(self.match_token(TID::INT)?)
                .finalize()),
            _ => self.parse_statement(),
        }
    }

    // DIM <Dim List>
    // <Dim List> ::= <Dim Definition> ',' <Dim List> |
    //                <Dim Definition>
    fn parse_dim(&mut self) -> Result<AST> {
        let mut a = self.match_token(TID::DIM)?;
        a.add_child(self.parse_dim_definition()?);

        loop {
            let tid = self.la().id;
            match tid {
                TID::COMMA => {
                    self.consume();
                    a.add_child(self.parse_dim_definition()?);
                    continue;
                }
                _ => break,
            }
        }
        Ok(a)
    }

    // <Dim Definition> ::= ID '(' <Expression List> ')'
    fn parse_dim_definition(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::ID)?
            .nop(self.match_token(TID::LBRK)?)
            .add_children(self.parse_expression_list()?)
            .nop(self.match_token(TID::RBRK)?)
            .finalize())
    }

    // ON ID GOTO <Expression List>
    fn parse_on(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::ON)?
            .add_child(self.parse_expression()?)
            .nop(self.match_token(TID::GOTO)?)
            .add_children(self.parse_expression_list()?)
            .finalize())
    }

    fn parse_goto(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::GOTO)?
            .add_child(self.parse_expression()?)
            .finalize())
    }

    // GOSUB <Expression>
    fn parse_gosub(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::GOSUB)?
            .add_child(self.parse_expression()?)
            .finalize())
    }

    // RETURN
    fn parse_return(&mut self) -> Result<AST> {
        self.match_token(TID::RETURN)
    }

    // STOP
    fn parse_stop(&mut self) -> Result<AST> {
        self.match_token(TID::STOP)
    }

    // READ <ID List>
    fn parse_read(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::READ)?
            .add_children(self.parse_id_list()?)
            .finalize())
    }

    // DATA <Constant List>
    fn parse_data(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::DATA)?
            .add_children(self.parse_constant_list()?)
            .finalize())
    }

    // RESTORE
    fn parse_restore(&mut self) -> Result<AST> {
        self.match_token(TID::RESTORE)
    }

    // FOR ID '=' <Expression> TO <Expression> <Step Opt>
    fn parse_for(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::FOR)?
            .add_child(self.match_token(TID::ID)?)
            .nop(self.match_token(TID::EQ)?)
            .add_child(self.parse_expression()?)
            .nop(self.match_token(TID::TO)?)
            .add_child(self.parse_expression()?)
            .add_child(self.parse_step()?)
            .finalize())
    }

    // <Step Opt> ::= STEP <Expression>
    fn parse_step(&mut self) -> Result<AST> {
        if self.la().id == TID::NL || self.la().id == TID::SEP {
            return Ok(AST::new(Token::new(TID::NONE, None)));
        }

        self.consume();
        self.parse_expression()
    }

    // NEXT [<ID List>]
    fn parse_next(&mut self) -> Result<AST> {
        let mut a = self.match_token(TID::NEXT)?;
        if self.la().id == TID::ID {
            a.add_children(self.parse_id_list()?);
        }
        Ok(a)
    }

    fn parse_assignment(&mut self) -> Result<AST> {
        Ok(AST::new(Token::new(TID::LET, None))
            .add_child(self.parse_id()?)
            .nop(self.match_token(TID::EQ)?)
            .add_child(self.parse_expression()?)
            .finalize())
    }

    // <Print List>      ::= <PrintStr> <PrintList>
    //                     | <Expression> <PrintList>
    //                     | ';' <PrintList>
    fn parse_print(&mut self) -> Result<AST> {
        let mut a = self.match_token(TID::PRINT)?;

        loop {
            let tid = self.la().id;
            match tid {
                TID::NL | TID::SEP => return Ok(a),
                TID::SEMICOLON | TID::COMMA => a.add_child(self.match_token(tid)?),
                TID::STR => a.add_child(self.match_token(tid)?),
                _ => a.add_child(self.parse_expression()?),
            };
        }
    }

    // <ID List>  ::= ID ',' <ID List>
    //              | ID
    fn parse_id_list(&mut self) -> Result<Vec<AST>> {
        let mut v: Vec<AST> = Vec::new();
        v.push(self.parse_id()?);

        loop {
            let tid = self.la().id;
            match tid {
                TID::COMMA => {
                    self.consume();
                    v.push(self.parse_id()?);
                    continue;
                }
                _ => break,
            }
        }
        Ok(v)
    }

    // <Constant List>   ::= <Constant> ',' <Constant List>
    //                    | <Constant>
    fn parse_constant_list(&mut self) -> Result<Vec<AST>> {
        let mut v: Vec<AST> = Vec::new();
        v.push(self.parse_constant()?);

        loop {
            let tid = self.la().id;
            match tid {
                TID::COMMA => {
                    self.consume();
                    v.push(self.parse_constant()?);
                    continue;
                }
                _ => break,
            }
        }
        Ok(v)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<AST>> {
        let mut v: Vec<AST> = Vec::new();
        v.push(self.parse_expression()?);

        loop {
            let tid = self.la().id;
            match tid {
                TID::COMMA => {
                    self.consume();
                    v.push(self.parse_expression()?);
                    continue;
                }
                _ => break,
            }
        }
        Ok(v)
    }

    // <Expression>  ::= <And Exp> OR <Expression>
    //                 | <And Exp>
    fn parse_expression(&mut self) -> Result<AST> {
        let a = self.parse_and_expression()?;
        let tid = self.la().id;
        match tid {
            TID::OR => {
                self.consume();
                let b = self.parse_expression()?;
                Ok(ast!(tid, a, b))
            }
            _ => Ok(a),
        }
    }

    // <And Exp>     ::= <Not Exp> AND <And Exp>
    //                 | <Not Exp>
    fn parse_and_expression(&mut self) -> Result<AST> {
        let a = self.parse_not_expression()?;
        let tid = self.la().id;
        match tid {
            TID::AND => {
                self.consume();
                let b = self.parse_expression()?;
                Ok(ast!(tid, a, b))
            }
            _ => Ok(a),
        }
    }

    // <Not Exp>     ::= NOT <Compare Exp>
    //                 | <Compare Exp>
    fn parse_not_expression(&mut self) -> Result<AST> {
        let tid = self.la().id;
        match tid {
            TID::NOT => {
                self.consume();
                let a = self.parse_compare_expression()?;
                Ok(ast!(tid, a))
            }
            _ => self.parse_compare_expression(),
        }
    }

    // <Compare Exp> ::= <Add Exp> '='  <Compare Exp>
    //                 | <Add Exp> '<>' <Compare Exp>
    //                 | <Add Exp> '>'  <Compare Exp>
    //                 | <Add Exp> '>=' <Compare Exp>
    //                 | <Add Exp> '<'  <Compare Exp>
    //                 | <Add Exp> '<=' <Compare Exp>
    //                 | <Add Exp>
    fn parse_compare_expression(&mut self) -> Result<AST> {
        let a = self.parse_add_expression()?;
        let tid = self.la().id;
        match tid {
            TID::EQ | TID::NEQ | TID::GT | TID::GTEQ | TID::LT | TID::LTEQ => {
                self.consume();
                let b = self.parse_compare_expression()?;
                Ok(ast!(tid, a, b))
            }
            _ => Ok(a),
        }
    }

    // <Add Exp>     ::= <Mult Exp> ('+' <Mult Exp>)*
    //                 | <Mult Exp> ('-' <Mult Exp>)*
    fn parse_add_expression(&mut self) -> Result<AST> {
        let mut a = self.parse_mult_expression()?;

        loop {
            let tid = self.la().id;
            match tid {
                TID::PLUS | TID::MINUS => {
                    self.consume();
                    let b = self.parse_mult_expression()?;
                    a = ast!(tid, a, b)
                }
                _ => break,
            }
        }
        Ok(a)
    }

    // <Mult Exp>    ::= <Negate Exp> '*' <Negate Exp>
    //                 | <Negate Exp> '/' <Negate Exp>
    //                 | <Negate Exp>
    fn parse_mult_expression(&mut self) -> Result<AST> {
        let mut a = self.parse_negate_expression()?;

        loop {
            let tid = self.la().id;
            match tid {
                TID::MULT | TID::DIV => {
                    self.consume();
                    let b = self.parse_negate_expression()?;
                    a = ast!(tid, a, b)
                }
                _ => break,
            }
        }
        Ok(a)
    }

    // <Negate Exp>  ::= '-' <Power Exp>
    //                 | <Power Exp>
    fn parse_negate_expression(&mut self) -> Result<AST> {
        let tid = self.la().id;
        match tid {
            TID::MINUS => {
                self.consume();
                let a = self.parse_mult_expression()?;
                Ok(ast!(tid, a))
            }
            _ => self.parse_power_expression(),
        }
    }

    // <Power Exp>   ::= <Power Exp> '^' <Sub Exp>
    //                 | <Sub Exp>
    fn parse_power_expression(&mut self) -> Result<AST> {
        let a = self.parse_sub_expression()?;
        let tid = self.la().id;
        match tid {
            TID::POW => {
                self.consume();
                let b = self.parse_sub_expression()?;
                Ok(ast!(tid, a, b))
            }
            _ => Ok(a),
        }
    }

    // <Sub Exp>     ::= '(' <Expression> ')'
    //                 | <Value>
    fn parse_sub_expression(&mut self) -> Result<AST> {
        let tid = self.la().id;
        match tid {
            TID::LBRK => {
                self.consume();
                let a = self.parse_expression()?;
                if self.la().id == TID::RBRK {
                    self.consume();
                }
                Ok(a)
            }
            _ => self.parse_value(),
        }
    }

    fn parse_value(&mut self) -> Result<AST> {
        match self.la().id {
            TID::ID => self.parse_id(),
            TID::FN => Ok(self.match_token(TID::FN)?
                .nop(self.match_token(TID::LBRK)?)
                .add_children(self.parse_expression_list()?)
                .nop(self.match_token(TID::RBRK)?)
                .finalize()),
            TID::ABS
            | TID::ASC
            | TID::ATN
            | TID::CHR
            | TID::COS
            | TID::EXP
            | TID::FINT
            | TID::LEN
            | TID::PEEK
            | TID::RND
            | TID::SGN
            | TID::SIN
            | TID::SPC
            | TID::SQR
            | TID::TAB
            | TID::TAN
            | TID::VAL
            | TID::FSTR => {
                let tid = self.la().id;
                Ok(self.match_token(tid)?
                    .nop(self.match_token(TID::LBRK)?)
                    .add_child(self.parse_expression()?)
                    .nop(self.match_token(TID::RBRK)?)
                    .finalize())
            }
            TID::LEFT | TID::RIGHT => {
                let tid = self.la().id;
                Ok(self.match_token(tid)?
                    .nop(self.match_token(TID::LBRK)?)
                    .add_child(self.parse_expression()?)
                    .nop(self.match_token(TID::COMMA)?)
                    .add_child(self.parse_expression()?)
                    .nop(self.match_token(TID::RBRK)?)
                    .finalize())
            }
            TID::MID => Ok(self.match_token(TID::MID)?
                .nop(self.match_token(TID::LBRK)?)
                .add_children(self.parse_expression_list()?)
                .nop(self.match_token(TID::RBRK)?)
                .finalize()),
            TID::FRE | TID::POS => {
                let tid = self.la().id;
                Ok(self.match_token(tid)?
                    .add_child(self.parse_value()?)
                    .finalize())
            }
            _ => self.parse_constant(),
        }
    }

    fn parse_constant(&mut self) -> Result<AST> {
        match self.la().id {
            TID::INT => self.match_token(TID::INT),
            TID::REAL => self.match_token(TID::REAL),
            TID::STR => self.match_token(TID::STR),
            TID::ID => {
                // Special case - in a constant list, a token
                // that looks like an ID is actually a string
                let mut ast = self.match_token(TID::ID)?;
                ast.token.id = TID::STR;
                Ok(ast)
            }
            TID::MINUS => {
                self.consume();
                let a = self.parse_constant()?;
                Ok(ast!(TID::MINUS, a))
            }
            _ => {
                debug!("Expected constant, got {:?}", self.la().id);
                Err(Error::SyntaxError(
                    self.line_num,
                    "INVALID CONSTANT".to_string(),
                ))
            }
        }
    }

    fn parse_newline(&mut self) -> Result<AST> {
        self.match_token(TID::NL)
    }

    fn parse_rem(&mut self) -> Result<AST> {
        self.match_token(TID::REM)
    }

    // DEF FunctionID '(' <ID List> ')' '=' <Expression>
    fn parse_def(&mut self) -> Result<AST> {
        Ok(self.match_token(TID::DEF)?
            .add_child(self.match_token(TID::FN)?)
            .nop(self.match_token(TID::LBRK)?)
            .add_children(self.parse_id_list()?)
            .nop(self.match_token(TID::RBRK)?)
            .nop(self.match_token(TID::EQ)?)
            .add_child(self.parse_expression()?)
            .finalize())
    }

    fn parse_statement(&mut self) -> Result<AST> {
        match self.la().id {
            TID::ID => self.parse_assignment(),
            TID::END => self.parse_end(),
            TID::LET => self.parse_let(),
            TID::PRINT => self.parse_print(),
            TID::INPUT => self.parse_input(),
            TID::IF => self.parse_if(),
            TID::GOTO => self.parse_goto(),
            TID::FOR => self.parse_for(),
            TID::NEXT => self.parse_next(),
            TID::DIM => self.parse_dim(),
            TID::ON => self.parse_on(),
            TID::READ => self.parse_read(),
            TID::REM => self.parse_rem(),
            TID::GOSUB => self.parse_gosub(),
            TID::RETURN => self.parse_return(),
            TID::STOP => self.parse_stop(),
            TID::DATA => self.parse_data(),
            TID::RESTORE => self.parse_restore(),
            TID::DEF => self.parse_def(),
            _ => {
                debug!("Unknown statement, got {:?}", self.la().id);
                Err(Error::SyntaxError(
                    self.line_num,
                    "UNKNOWN STATEMENT".to_string(),
                ))
            }
        }
    }

    fn parse_statements(&mut self, line_num: u32, root: &mut AST) -> Result<()> {
        let mut a = self.parse_statement()?;
        a.set_linenum(line_num);
        root.add_child(a);

        while self.la().id == TID::SEP {
            self.consume();
            root.add_child(self.parse_statement()?);
        }

        Ok(())
    }

    pub fn parse(&mut self) -> Result<AST> {
        let mut a = AST::new(Token::new(TID::ROOT, None));
        self.consume(); // Prime the pump

        while self.la().id != TID::NONE {
            let n = self.parse_linenum()?;
            self.parse_statements(n, &mut a)?;
            self.parse_newline()?;
        }

        return Ok(a);
    }
}

#[cfg(test)]
mod tests {
    use parser::*;

    #[test]
    fn can_parse_single_statement() {
        let input = r#"10 END
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(_) => (),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn can_parse_multi_statement() {
        let input = r#"10 END : END : END : END
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(_) => (),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn can_parse_multiple_lines() {
        let input = r#"10 LET A = 10 : LET B = 20
        20 PRINT A : PRINT B
        30 GOTO (5 + 5)
        40 IF A < 5 THEN 20
        50 IF B > 10 THEN PRINT "FOO"
        60 END
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(_) => (),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn can_parse_expressions() {
        let input = r#"10 LET A = 10 + 5
        20 LET B = (10 + 5) ^ 2
        30 C = 20 - 10 * 4
        40 PRINT -1;2;-3
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(_) => (),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn can_parse_builtin_functions() {
        let input = r#"10 A = ABS(10 + 5)
        20 B = RND(100)
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        match parser.parse() {
            Ok(_) => (),
            Err(_) => assert!(false),
        }
    }
}
