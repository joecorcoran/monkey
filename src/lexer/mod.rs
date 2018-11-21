use unicode_segmentation::UnicodeSegmentation as US;
use std::char;
use token::*;

pub struct Lexer<'a> {
    input: Vec<&'a str>,
    index: Option<usize>
}

impl<'a> Iterator for Lexer<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        self.index = match self.index {
            Some(i) => Some(i + 1),
            None => Some(0)
        };
        let nxt = self.input.get(self.index.unwrap());
        nxt.and_then(|n| Some(*n))
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let graphemes = US::graphemes(input, true).collect::<Vec<&str>>();
        Lexer {
            input: graphemes,
            index: None
        }
    }

    fn peek(&mut self) -> Option<&'a str> {
        let next_index = if self.index.is_none() { 0 } else { self.index.unwrap() + 1 };
        let nxt = self.input.get(next_index);
        nxt.and_then(|n| Some(*n))
    }

    fn is_numeric(g: &'a str) -> bool {
        let first_char = g.chars().next().unwrap();
        first_char.is_digit(10)
    }

    fn is_alphabetic(g: &'a str) -> bool {
        g.chars().all(char::is_alphanumeric) && !Self::is_numeric(g)
    }

    fn is_whitespace(g: &'a str) -> bool {
        g.chars().all(char::is_whitespace)
    }

    fn take_alphanumeric(&mut self) -> String {
        let mut alphanum = vec![];
        while self.peek().unwrap_or("").chars().all(char::is_alphanumeric) {
            match self.next() {
                Some(g) => alphanum.push(g),
                None => break
            }
        }
        alphanum.concat()
    }

    fn take_numeric(&mut self) -> String {
        let mut num = vec![];
        while let Some(g) = self.peek() {
            match g {
                g if Self::is_numeric(g) => num.push(self.next().unwrap()),
                _ => break
            }
        }
        num.concat()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        match self.peek() {
            // Swallow whitespace
            Some(g) if Self::is_whitespace(g) => {
                self.next();
                self.next_token()
            },
            // Comparators
            Some("!") | Some("=") => {
                // TODO this is too greedy, e.g. !!1 and 1======1
                let mut result = vec![self.next().unwrap()];
                match self.peek() {
                    Some("!") | Some("=") => {
                        result.push(self.next().unwrap());
                    },
                    _ => {}
                }
                Some(lookup(result.concat()))
            },
            // Integers
            Some(g) if Self::is_numeric(g) => {
                let num = self.take_numeric();
                Some(Token::Integer(num))
            },
            // Keywords and identifiers
            Some(g) if Self::is_alphabetic(g) => {
                let alphanum = self.take_alphanumeric();
                Some(lookup(alphanum))
            },
            // Single-grapheme tokens
            Some(_) => Some(lookup(self.next().unwrap())),
            // EOF
            None => None
        }
    }

    pub fn peek_token(&mut self) -> Option<Token> {
        let current_index = self.index;
        let token = self.next_token();
        self.index = current_index;
        token
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iterator() {
        let s = "hä";
        let mut l = Lexer::new(s);

        assert_eq!("h", l.next().unwrap());
        assert_eq!("ä", l.next().unwrap());
        assert_eq!(None, l.next())
    }

    #[test]
    fn peekable() {
        let s = "hä?";
        let mut l = Lexer::new(s);

        assert_eq!("h", l.peek().unwrap());
        assert_eq!("h", l.peek().unwrap());
        assert_eq!("h", l.next().unwrap());

        assert_eq!("ä", l.peek().unwrap());
        assert_eq!("ä", l.next().unwrap());

        assert_eq!("?", l.peek().unwrap());
        assert_eq!("?", l.next().unwrap());

        assert_eq!(None, l.peek());
        assert_eq!(None, l.next())

    }

    #[test]
    fn token_iter() {
        let s = "let foo = 123; foo != 123;";
        let mut l = Lexer::new(s);

        assert_eq!(Token::Let, l.next_token().unwrap());
        assert_eq!(Token::Identifier("foo".to_string()), l.next_token().unwrap());
        assert_eq!(Token::Assign, l.next_token().unwrap());
        assert_eq!(Token::Integer("123".to_string()), l.next_token().unwrap());
        assert_eq!(Token::Semicolon, l.next_token().unwrap());

        assert_eq!(Token::Identifier("foo".to_string()), l.next_token().unwrap());
        assert_eq!(Token::NotEqual, l.next_token().unwrap());
        assert_eq!(Token::Integer("123".to_string()), l.next_token().unwrap());
        assert_eq!(Token::Semicolon, l.next_token().unwrap());

        assert_eq!(None, l.next_token())
    }

    #[test]
    fn token_peekable() {
        let s = "a == b;";
        let mut l = Lexer::new(s);

        assert_eq!(Token::Identifier("a".to_string()), l.peek_token().unwrap());
        assert_eq!(Token::Identifier("a".to_string()), l.peek_token().unwrap());
        assert_eq!(Token::Identifier("a".to_string()), l.next_token().unwrap());

        assert_eq!(Token::Equal, l.peek_token().unwrap());
        assert_eq!(Token::Equal, l.next_token().unwrap());

        assert_eq!(Token::Identifier("b".to_string()), l.peek_token().unwrap());
        assert_eq!(Token::Identifier("b".to_string()), l.next_token().unwrap());

        assert_eq!(Token::Semicolon, l.peek_token().unwrap());
        assert_eq!(Token::Semicolon, l.next_token().unwrap());

        assert_eq!(None, l.peek_token());
        assert_eq!(None, l.next_token());
    }
}
