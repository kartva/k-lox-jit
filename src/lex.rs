enum Token {
	// Single-character tokens.
	LEFT_PAREN, RIGHT_PAREN,
	LEFT_BRACE, RIGHT_BRACE,
	COMMA, DOT, MINUS, PLUS,
	SEMICOLON, SLASH, STAR,
	// One or two character tokens.
	BANG, BANG_EQUAL,
	EQUAL, EQUAL_EQUAL,
	GREATER, GREATER_EQUAL,
	LESS, LESS_EQUAL,
	// Literals.
	IDENTIFIER, STRING, NUMBER,
	// Keywords.
	AND, CLASS, ELSE, FALSE,
	FOR, FUN, IF, NIL, OR,
	PRINT, RETURN, SUPER, THIS,
	TRUE, VAR, WHILE,

	ERROR, EOF
}

pub enum ParseResult {
	Tokens(Vec<Token>),
	Errors(Vec<ParseError>),
}

struct ParseError {
	pub message: String,
	pub position: usize,
}

struct Tokens<'a> {
	src: &'a str,
	idx: usize,
}

impl<'a> Tokens<'a> {
	pub fn new(src: &'a str) -> Self {
		Tokens {
			src,
			idx: 0,
		}
	}
}

macro_rules! match_toks {
	($slice: expr, $incr_id: expr, $([$text: literal, $token: expr]), *) => {
		$(
			if $slice.starts_with($text) {
				const LEN: usize = $text.len();
				$incr_id += LEN;
				return Some($token);
			}
		)*
	}
}

impl Iterator for Tokens<'_> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		if self.idx >= self.src.len() {
			return None;
		}
		
		// match double character tokens
		match_toks!(self.src[self.idx..self.idx+2], self.idx, 
			["!=", Token::BANG_EQUAL],
			["==", Token::EQUAL_EQUAL],
			[">=", Token::GREATER_EQUAL],
			["<=", Token::LESS_EQUAL]
		);

		self.idx += 1;

		todo!();
	}
}

fn parse () -> ParseResult {
	todo!()
}