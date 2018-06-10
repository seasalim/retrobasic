use lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct AST {
    pub line_num: Option<u32>,
    pub token: Token,
    pub children: Vec<AST>,
}

fn fmt_node(node: &AST) -> String {
    let mut s = String::new();
    s.push_str("(");
    let m = match node.token.val {
        Some(ref s) => format!("{:?}:{}", node.token.id, s),
        None => format!("{:?}", node.token.id),
    };
    s.push_str(&m);
    for i in 0..node.children.len() {
        s.push_str(&fmt_node(&node.children[i]));
    }
    s.push_str(")");
    s
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", fmt_node(&self))
    }
}

impl AST {
    pub fn new(token: Token) -> Self {
        AST {
            line_num: None,
            token: token,
            children: Vec::new(),
        }
    }

    pub fn set_linenum(&mut self, line_num: u32) {
        self.line_num = Some(line_num)
    }

    pub fn add_child(&mut self, child: AST) -> &mut Self {
        self.children.push(child);
        self
    }

    pub fn add_children(&mut self, children: Vec<AST>) -> &mut Self {
        for i in 0..children.len() {
            let c = &children[i];
            self.children.push(c.clone());
        }
        self
    }

    pub fn add_optional_child(&mut self, child: Option<AST>) -> &mut Self {
        match child {
            Some(c) => self.children.push(c.clone()),
            None => (),
        }
        self
    }

    pub fn nop(&mut self, _: AST) -> &mut Self {
        self
    }

    pub fn finalize(&self) -> Self {
        AST {
            line_num: self.line_num.clone(),
            token: self.token.clone(),
            children: self.children.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use lexer::*;
    use tokenid::*;

    #[test]
    fn can_add_children() {
        let mut a = AST::new(Token::new(TID::NONE, None));
        let c1 = AST::new(Token::new(TID::INT, Some(String::from("10"))));
        let c2 = AST::new(Token::new(TID::ID, Some(String::from("foo"))));
        a.add_child(c1).add_child(c2);

        assert_eq!(2, a.children.len());
    }
}
