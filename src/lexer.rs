use std::fmt;
use tokenid::TID;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub id: TID,
    pub val: Option<String>,
}

impl Token {
    pub fn new(tid: TID, v: Option<String>) -> Self {
        Token { id: tid, val: v }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val {
            Some(ref s) => write!(f, "{}", s),
            None => write!(f, ""),
        }
    }
}

pub struct Lexer {
    buf: Vec<u8>,
    curr: usize,
    c: char,
}

// NewLine        = {CR}{LF}|{CR}
// Remark         = REM{Space}{Printable}*
// ID             = {Letter}{Alphanumeric}?[$%]?
// FunctionID     = FN {Letter}{Letter}?
// String         = '"'{String Chars}*'"'
// Integer        = {Digit}+
// Real           = {Digit}+'.'{Digit}+

impl Lexer {
    pub fn new(s: String) -> Self {
        let mut l = Lexer {
            buf: s.into_bytes(),
            curr: 0,
            c: '\0',
        };
        if l.buf.len() > 0 {
            l.c = l.buf[0] as char;
        };
        l
    }

    fn consume(&mut self) {
        self.curr = self.curr + 1;
        self.c = if self.curr < self.buf.len() {
            self.buf[self.curr] as char
        } else {
            '\0'
        };
    }

    fn la(&self) -> char {
        self.c
    }

    fn newline(&mut self) -> Token {
        self.consume();
        Token::new(TID::NL, None)
    }

    fn number(&mut self) -> Token {
        let mut s = String::new();
        let mut tid = TID::INT;
        while self.la().is_digit(10) || (self.la() == '.') {
            s.push(self.la());
            if self.la() == '.' {
                tid = TID::REAL;
            };
            self.consume();
        }
        Token::new(tid, Some(s))
    }

    fn str(&mut self) -> Token {
        let mut s = String::new();
        self.consume();
        while self.la() != '"' {
            s.push(self.la());
            self.consume();
        }
        self.consume();
        Token::new(TID::STR, Some(s))
    }

    fn operator(&mut self) -> Token {
        let mut s = String::new();
        let c = self.la();
        s.push(c);
        self.consume();

        match c {
            '<' | '>' => {
                if self.la() == '=' || self.la() == '>' {
                    s.push(self.la());
                    self.consume();
                }
            }
            _ => (),
        }

        Token::new(TID::from(s.as_str()), None)
    }

    fn rem(&mut self) -> Token {
        let mut s = String::new();
        while self.la() != '\n' {
            s.push(self.la());
            self.consume();
        }
        Token::new(TID::REM, None)
    }

    fn identifier(&mut self) -> Token {
        let mut s = String::new();
        let mut last_ch;

        loop {
            s.push(self.la());
            last_ch = self.la();
            self.consume();
            if !self.la().is_alphanumeric() || (TID::from(s.as_str()) != TID::NONE)
                || last_ch.is_numeric() || (self.la().is_numeric() && (s.len() > 2))
            {
                break;
            };
        }

        if (self.la() == '$') || (self.la() == '%') {
            s.push(self.la());
            self.consume();
        }

        let tid = TID::from(s.as_str());
        match tid {
            TID::NONE => Token::new(TID::ID, Some(s)),
            TID::FN => {
                // Accept up to a 2 char function name
                let mut s = String::new();
                s.push(self.la());
                self.consume();
                if self.la().is_alphanumeric() {
                    s.push(self.la());
                };
                Token::new(TID::FN, Some(s))
            }
            TID::REM => self.rem(),
            _ => Token::new(tid, None),
        }
    }

    pub fn next_token(&mut self) -> Token {
        while self.la().is_whitespace() && (self.la() != '\n') {
            self.consume();
        }

        match self.la() {
            '\n' => self.newline(),
            '0'...'9' | '.' => self.number(),
            '"' => self.str(),
            'a'...'z' | 'A'...'Z' => self.identifier(),
            '=' | ':' | ';' | ',' | '*' | '/' | '+' | '-' | '^' | '<' | '>' | '(' | ')' => {
                self.operator()
            }
            _ => Token::new(TID::NONE, None),
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::*;
    use tokenid::*;

    #[test]
    fn can_tokenize_statement() {
        let input = r#"10 PRINT "HELLO WORLD" : AB$ 123.45 ; REM "IGNORED()$
        "#;
        let mut lexer = Lexer::new(String::from(input));
        let mut t: Token;

        t = lexer.next_token();
        assert_eq!(t.id, TID::INT);
        assert_eq!(t.val, Some(String::from("10")));

        t = lexer.next_token();
        assert_eq!(t.id, TID::PRINT);
        assert_eq!(t.val, None);

        t = lexer.next_token();
        assert_eq!(t.id, TID::STR);
        assert_eq!(t.val, Some(String::from("HELLO WORLD")));

        t = lexer.next_token();
        assert_eq!(t.id, TID::SEP);
        assert_eq!(t.val, None);

        t = lexer.next_token();
        assert_eq!(t.id, TID::ID);
        assert_eq!(t.val, Some(String::from("AB$")));

        t = lexer.next_token();
        assert_eq!(t.id, TID::REAL);
        assert_eq!(t.val, Some(String::from("123.45")));

        t = lexer.next_token();
        assert_eq!(t.id, TID::SEMICOLON);
        assert_eq!(t.val, None);

        t = lexer.next_token();
        assert_eq!(t.id, TID::REM);
        assert_eq!(t.val, None);

        t = lexer.next_token();
        assert_eq!(t.id, TID::NL);
        assert_eq!(t.val, None);

        t = lexer.next_token();
        assert_eq!(t.id, TID::NONE);
        assert_eq!(t.val, None);
    }
}
