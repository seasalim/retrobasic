#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    None,
}

impl<'a> From<&'a str> for Operator {
    fn from(s: &'a str) -> Self {
        match s {
            "=" => Operator::Equal,
            "<>" => Operator::NotEqual,
            ">" => Operator::GreaterThan,
            ">=" => Operator::GreaterThanOrEqual,
            "<" => Operator::LessThan,
            "<=" => Operator::LessThanOrEqual,
            "+" => Operator::Plus,
            "-" => Operator::Minus,
            "*" => Operator::Multiply,
            "/" => Operator::Divide,
            "^" => Operator::Power,
            _ => Operator::None,
        }
    }
}

#[cfg(test)]
mod tests {
    use operator::*;

    #[test]
    fn can_convert_from_string() {
        assert_eq!(Operator::from("="), Operator::Equal);
        assert_eq!(Operator::from("<>"), Operator::NotEqual);
        assert_eq!(Operator::from(">"), Operator::GreaterThan);
        assert_eq!(Operator::from(">="), Operator::GreaterThanOrEqual);
        assert_eq!(Operator::from("<"), Operator::LessThan);
        assert_eq!(Operator::from("<="), Operator::LessThanOrEqual);
        assert_eq!(Operator::from("+"), Operator::Plus);
        assert_eq!(Operator::from("-"), Operator::Minus);
        assert_eq!(Operator::from("*"), Operator::Multiply);
        assert_eq!(Operator::from("/"), Operator::Divide);
        assert_eq!(Operator::from("^"), Operator::Power);
    }
}
