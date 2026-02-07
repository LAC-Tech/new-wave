use std::{error::Error, iter::Iterator};

#[derive(Debug, Eq, PartialEq)]
enum Token {
    Word(u16, u16),
    StrLit(u16, u16),
}

#[derive(Debug, Eq, PartialEq)]
enum Err {
    UnterminatedStrLit,
}

impl Token {
    fn str<'a>(&self, source_code: &'a str) -> &'a str {
        match self {
            &Token::Word(start, end) => &source_code[start.into()..end.into()],
            &Token::StrLit(start, end) => {
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
    type Item = Result<Token, Err>;
    fn next(&mut self) -> Option<Self::Item> {
        let (i, c) = self.advance_to_identifier()?;
        self.cursor += i;
        let start_pos = to_u16(self.cursor);

        match c {
            '"' => match self.advance_to_char('"') {
                Some(offset) => {
                    let new_cursor = self.cursor + offset;
                    self.cursor = new_cursor;

                    let item = Ok(Token::StrLit(start_pos, to_u16(new_cursor)));

                    Some(item)
                }
                None => {
                    let new_cursor = self.cursor;
                    self.cursor = new_cursor;
                    let item = Err(Err::UnterminatedStrLit);
                    Some(item)
                }
            },
            _ => {
                let new_cursor = self
                    .advance_to_whitespace()
                    .map(|offset| self.cursor + offset)
                    .unwrap_or(self.source_code.len());

                self.cursor = new_cursor;

                let item = Ok(Token::Word(start_pos, to_u16(new_cursor)));

                Some(item)
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
        let ts: Result<Box<[Token]>, Err> = tokenize(sc).collect();
        let ts = ts.expect("no parse errors");
        assert_eq!(ts.as_ref(), &[Token::Word(0, 5), Token::Word(6, 11)]);

        let ids: Box<[&str]> = ts.iter().map(|t| t.str(sc)).collect();
        assert_eq!(ids.as_ref(), &["hello", "world"]);
    }

    #[test]
    fn split_with_quoted_string() {
        let sc = "\"hello\" \"world\" concat";
        let ts: Result<Box<[Token]>, Err> = tokenize(sc).collect();
        let ts = ts.expect("no parse errors");
        assert_eq!(
            ts.as_ref(),
            &[Token::StrLit(0, 7), Token::StrLit(8, 15), Token::Word(16, 22)]
        );

        let ids: Box<[&str]> = ts.iter().map(|t| t.str(sc)).collect();
        assert_eq!(ids.as_ref(), &["hello", "world", "concat"]);
    }

    #[test]
    fn unterminated_string_literal() {
        let sc = "\"hwæt";
        let ts: Result<Box<[Token]>, Err> = tokenize(sc).collect();

        assert_eq!(ts, Err(Err::UnterminatedStrLit));
    }
}
