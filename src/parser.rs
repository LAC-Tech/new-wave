use std::{error::Error, iter::Iterator};

#[derive(Debug, Eq, PartialEq)]
enum Tok {
    Word(u16, u16),
    StrLit(u16, u16),
    LQuote(u16),
    RQuote(u16),
}

#[derive(Debug, Eq, PartialEq)]
enum Err {
    UnterminatedStrLit,
}

impl Tok {
    fn str<'a>(&self, source_code: &'a str) -> &'a str {
        match self {
            &Tok::Word(start, end) => &source_code[start.into()..end.into()],
            &Tok::StrLit(start, end) => {
                let (start, end) = (start + 1, end - 1); // removing quotes
                &source_code[start.into()..end.into()]
            }
            &Tok::LQuote(_) => "[",
            &Tok::RQuote(_) => "]",
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
    type Item = Result<Tok, Err>;
    fn next(&mut self) -> Option<Self::Item> {
        let (i, c) = self.advance_to_identifier()?;
        self.cursor += i;
        let start_pos = to_u16(self.cursor);

        let (new_cusor, item) = match c {
            '"' => self
                .advance_to_char('"')
                .map(|offset| {
                    let new_cursor = self.cursor + offset;
                    let t = Tok::StrLit(start_pos, to_u16(new_cursor));
                    (new_cursor, Ok(t))
                })
                .unwrap_or((self.cursor, Err(Err::UnterminatedStrLit))),
            '[' => (self.cursor + 1, Ok(Tok::LQuote(to_u16(self.cursor)))),
            ']' => (self.cursor + 1, Ok(Tok::RQuote(to_u16(self.cursor)))),
            _ => {
                let new_cursor = self
                    .advance_to_whitespace()
                    .map(|offset| self.cursor + offset)
                    .unwrap_or(self.source_code.len());

                let t = Tok::Word(start_pos, to_u16(new_cursor));

                (new_cursor, Ok(t))
            }
        };

        self.cursor = new_cusor;
        Some(item)
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
        let ts: Result<Box<[Tok]>, Err> = tokenize(sc).collect();
        let ts = ts.expect("no parse errors");
        assert_eq!(ts.as_ref(), &[Tok::Word(0, 5), Tok::Word(6, 11)]);

        let ids: Box<[&str]> = ts.iter().map(|t| t.str(sc)).collect();
        assert_eq!(ids.as_ref(), &["hello", "world"]);
    }

    #[test]
    fn split_with_quoted_string() {
        let sc = r#""hello" "world" concat"#;
        let ts: Result<Box<[Tok]>, Err> = tokenize(sc).collect();
        let ts = ts.expect("no parse errors");
        assert_eq!(
            ts.as_ref(),
            &[Tok::StrLit(0, 7), Tok::StrLit(8, 15), Tok::Word(16, 22)]
        );

        let ids: Box<[&str]> = ts.iter().map(|t| t.str(sc)).collect();
        assert_eq!(ids.as_ref(), &["hello", "world", "concat"]);
    }

    #[test]
    fn unterminated_string_literal() {
        let sc = r#""hwæt"#;
        let ts: Result<Box<[Tok]>, Err> = tokenize(sc).collect();

        assert_eq!(ts, Err(Err::UnterminatedStrLit));
    }

    #[test]
    fn quotes() {
        let sc = "[ add1 ]";
        let ts: Result<Box<[Tok]>, Err> = tokenize(sc).collect();
        let ts = ts.expect("no parse errors");

        assert_eq!(
            ts.as_ref(),
            &[Tok::LQuote(0), Tok::Word(2, 6), Tok::RQuote(7),]
        );

        let ids: Box<[&str]> = ts.iter().map(|t| t.str(sc)).collect();
        assert_eq!(ids.as_ref(), &["[", "add1", "]"]);
    }
}
