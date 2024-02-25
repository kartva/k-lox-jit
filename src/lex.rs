use std::{iter::Peekable, ops::Index};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum TokenTy {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[derive(PartialEq, Eq, Debug)]
struct Token {
	start: usize,
	end: usize,
	kind: TokenTy
}

pub enum ParseResult {
    Tokens(Vec<TokenTy>),
    Errors(Vec<ParseError>),
}

struct ParseError {
    pub message: String,
    pub position: usize,
}

struct Tokens<'a> {
    src: &'a [u8],
    line_no: usize,
    start: usize,
	curr: usize,
    in_error: bool,
}

impl<'a> Tokens<'a> {
    pub fn new(src: &'a str) -> Self {
        Tokens {
            in_error: false,
            src: src.as_bytes(),
            line_no: 1,
            start: 0,
			curr: 0,
        }
    }

	fn emit_token(&mut self, kind: TokenTy) -> Token {
		let res = Token {
			start: self.start,
			end: self.curr,
			kind
		};
		self.start = self.curr;
		res
	}

    fn take_while<F>(&mut self, f: F)
    where
        F: Fn(u8) -> bool,
    {
        while self.curr < self.src.len() && f(self.src[self.curr]) {
            self.curr += 1;
        }
    }

    fn skip_while<F>(&mut self, f: F)
    where
        F: Fn(u8) -> bool,
    {
        while self.start < self.src.len() && f(self.src[self.start]) {
            self.start += 1;
        }
    }

    fn just(&mut self, s: &str) -> bool {
        if self.src[self.curr..].starts_with(s.as_bytes()) {
            self.curr += s.len();
            true
        } else {
            false
        }
    }

    /// Returns an error token if the string is not found.
    fn expect(&mut self, s: &str) {
        if !self.just(s) {
            self.in_error = true;
        }
    }
}

impl Iterator for Tokens<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        eprintln!("Skipping whitespace from {}", self.start);
        while self.start < self.src.len() && self.src[self.start].is_ascii_whitespace() {
            if self.src[self.start] == b'\n' {
                self.line_no += 1;
            }
            self.start += 1;
        }
        eprintln!("Skipped whitespace to {}", self.start);

        if self.start >= self.src.len() {
            return None;
        }
        self.curr = self.start;

        let str_to_tok = [
            ("==", TokenTy::EqualEqual),
            ("!=", TokenTy::BangEqual),
            ("<=", TokenTy::LessEqual),
            (">=", TokenTy::GreaterEqual),
            ("(", TokenTy::LeftParen),
            (")", TokenTy::RightParen),
            ("{", TokenTy::LeftBrace),
            ("}", TokenTy::RightBrace),
            (",", TokenTy::Comma),
            (".", TokenTy::Dot),
            ("-", TokenTy::Minus),
            ("+", TokenTy::Plus),
            (";", TokenTy::Semicolon),
            ("*", TokenTy::Star),
            ("!", TokenTy::Bang),
            ("=", TokenTy::Equal),
            ("<", TokenTy::Less),
            (">", TokenTy::Greater),
            ("/", TokenTy::Slash),
            (";", TokenTy::Semicolon),
            ("var", TokenTy::Var),
            ("and", TokenTy::And),
            ("class", TokenTy::Class),
            ("else", TokenTy::Else),
            ("false", TokenTy::False),
            ("for", TokenTy::For),
            ("fun", TokenTy::Fun),
            ("if", TokenTy::If),
            ("nil", TokenTy::Nil),
            ("or", TokenTy::Or),
            ("print", TokenTy::Print),
            ("return", TokenTy::Return),
            ("super", TokenTy::Super),
            ("this", TokenTy::This),
            ("true", TokenTy::True),
            ("while", TokenTy::While),
        ];

        eprintln!("Checking for keywords from {}", self.start);
        for (needle, ty) in str_to_tok {
            if self.just(needle) {
                return Some(self.emit_token(ty));
            }
        }
        eprintln!("No keywords found");

        // handle numbers
        if self.src[self.start].is_ascii_digit() {
            self.take_while(|c| c.is_ascii_digit());
            return Some(self.emit_token(TokenTy::Number));
        }
        eprintln!("No numbers found");

        // handle idents
        if self.src[self.start].is_ascii_alphabetic() {
            self.take_while(|c| c.is_ascii_alphanumeric() || c == b'_');
            return Some(self.emit_token(TokenTy::Identifier));
        }
        eprintln!("No idents found");

        // handle strings
        if self.src[self.start] == b'"' {
            self.expect("\"");
            self.take_while(|c| c != b'"');
            self.expect("\"");
            return Some(self.emit_token(TokenTy::String));
        }
        eprintln!("No strings found");

        // handle comments
        if self.just("//") {
            self.take_while(|c| c != b'\n');
            return Some(self.emit_token(TokenTy::Semicolon));
        }

        // error occurred
        // skip until ; and include ; in the error token
        self.take_while(|c| c != b';');
        self.curr += 1;
        Some(self.emit_token(TokenTy::Error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut tokens = Tokens::new("var x = 10;");
        assert_eq!(tokens.next(), Some(Token { start: 0, end: 3, kind: TokenTy::Var }));
        assert_eq!(tokens.next(), Some(Token { start: 4, end: 5, kind: TokenTy::Identifier }));
        assert_eq!(tokens.next(), Some(Token { start: 6, end: 7, kind: TokenTy::Equal }));
        assert_eq!(tokens.next(), Some(Token { start: 8, end: 10, kind: TokenTy::Number }));
        assert_eq!(tokens.next(), Some(Token { start: 10, end: 11, kind: TokenTy::Semicolon }));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn test_next_with_whitespace() {
        let mut tokens = Tokens::new("   var   x   =   10   ;   ");
        assert_eq!(tokens.next(), Some(Token { start: 3, end: 6, kind: TokenTy::Var }));
        assert_eq!(tokens.next(), Some(Token { start: 9, end: 10, kind: TokenTy::Identifier }));
        assert_eq!(tokens.next(), Some(Token { start: 13, end: 14, kind: TokenTy::Equal }));
        assert_eq!(tokens.next(), Some(Token { start: 17, end: 19, kind: TokenTy::Number }));
        assert_eq!(tokens.next(), Some(Token { start: 22, end: 23, kind: TokenTy::Semicolon }));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn test_next_with_keywords() {
        let mut tokens = Tokens::new("if (x == 10) { print(\"Hello, world!\"); }");
        assert_eq!(tokens.next(), Some(Token { start: 0, end: 2, kind: TokenTy::If }));
        assert_eq!(tokens.next(), Some(Token { start: 3, end: 4, kind: TokenTy::LeftParen }));
        assert_eq!(tokens.next(), Some(Token { start: 4, end: 5, kind: TokenTy::Identifier }));
        assert_eq!(tokens.next(), Some(Token { start: 6, end: 8, kind: TokenTy::EqualEqual }));
        assert_eq!(tokens.next(), Some(Token { start: 9, end: 11, kind: TokenTy::Number }));
        assert_eq!(tokens.next(), Some(Token { start: 11, end: 12, kind: TokenTy::RightParen }));
        assert_eq!(tokens.next(), Some(Token { start: 13, end: 14, kind: TokenTy::LeftBrace }));
        assert_eq!(tokens.next(), Some(Token { start: 15, end: 20, kind: TokenTy::Print }));
        assert_eq!(tokens.next(), Some(Token { start: 20, end: 21, kind: TokenTy::LeftParen }));
        assert_eq!(tokens.next(), Some(Token { start: 21, end: 36, kind: TokenTy::String }));
        assert_eq!(tokens.next(), Some(Token { start: 36, end: 37, kind: TokenTy::RightParen }));
        assert_eq!(tokens.next(), Some(Token { start: 37, end: 38, kind: TokenTy::Semicolon }));
        assert_eq!(tokens.next(), Some(Token { start: 39, end: 40, kind: TokenTy::RightBrace }));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn test_next_with_error() {
        // this tests currently fails since the parser identifies 1, and then a string instead of an error.
        let mut tokens = Tokens::new("var 1\" \" = 10;\n2;");
        assert_eq!(tokens.next(), Some(Token { start: 0, end: 3, kind: TokenTy::Var }));
        assert_eq!(tokens.next(), Some(Token { start: 4, end: 10, kind: TokenTy::Error }));
        assert_eq!(tokens.next(), Some(Token { start: 8, end: 10, kind: TokenTy::Number }));
        assert_eq!(tokens.next(), Some(Token { start: 10, end: 11, kind: TokenTy::Semicolon }));
        assert_eq!(tokens.next(), None);
    }
}