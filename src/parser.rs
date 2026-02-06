use std::iter::Iterator;

#[derive(Debug, Eq, PartialEq)]
enum Token {
    Word(u16, u16),
    StringLiteral(u16, u16),
}

enum Err {
    UnterminatedString,
}

impl Token {
    fn str<'a>(&self, source_code: &'a str) -> &'a str {
        match self {
            &Token::Word(start, end) => &source_code[start.into()..end.into()],
            &Token::StringLiteral(start, end) => {
                let (start, end) = (start + 1, end - 1); // removing quotes
                &source_code[start.into()..end.into()]
            }
        }
    }
}

struct Tokenizer<'a> {
    source_code: &'a str,
    cursor: usize,
}

fn to_u16(i: usize) -> u16 {
    i.try_into().expect("source code is very long")
}

impl<'a> Tokenizer<'a> {
    fn advance_to_identifier(&self) -> Option<(usize, char)> {
        self.source_code[self.cursor..]
            .chars()
            .enumerate()
            .find(|(_i, c)| !c.is_whitespace())
    }

    fn advance_to_whitespace(&self) -> Option<usize> {
        self.source_code[self.cursor..].find(|c: char| c.is_whitespace())
    }

    fn advance_to_char(&self, c_to_find: char) -> Option<usize> {
        self.source_code[self.cursor + 1..]
            .find(|c: char| c == c_to_find)
            .map(|i| i + 2)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let (i, c) = self.advance_to_identifier()?;
        self.cursor += i;
        let start_pos = to_u16(self.cursor);

        match c {
            '"' => {
                self.cursor = self
                    .advance_to_char('"')
                    .map(|offset| self.cursor + offset)
                    .unwrap_or(self.source_code.len());

                Some(Token::StringLiteral(start_pos, to_u16(self.cursor)))
            }
            _ => {
                self.cursor = self
                    .advance_to_whitespace()
                    .map(|offset| self.cursor + offset)
                    .unwrap_or(self.source_code.len());

                Some(Token::Word(start_pos, to_u16(self.cursor)))
            }
        }
    }
}

fn tokenize<'a>(source_code: &'a str) -> Tokenizer<'a> {
    Tokenizer { source_code, cursor: 0 }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn split_basic() {
        let sc = "hello world";
        let mut ts = tokenize(sc);

        let t = ts.next();
        assert_eq!(t, Some(Token::Word(0, 5)));
        assert_eq!(t.unwrap().str(sc), "hello");

        let t = ts.next();
        assert_eq!(t, Some(Token::Word(6, 11)));
        assert_eq!(t.unwrap().str(sc), "world");

        let t = ts.next();
        assert_eq!(t, None);
    }

    #[test]
    fn split_with_quoted_string() {
        let sc = "\"hello\" \"world\" concat";
        let mut ts = tokenize(sc);

        let t = ts.next();
        assert_eq!(t, Some(Token::StringLiteral(0, 7)));
        assert_eq!(t.unwrap().str(sc), "hello");

        let t = ts.next();
        assert_eq!(t, Some(Token::StringLiteral(8, 15)));
        assert_eq!(t.unwrap().str(sc), "world");

        let t = ts.next();
        assert_eq!(t, Some(Token::Word(16, 22)));
        assert_eq!(t.unwrap().str(sc), "concat");

        let t = ts.next();
        assert_eq!(t, None);
    }

    #[test]
    fn unterminated_string_literal() {}
}
