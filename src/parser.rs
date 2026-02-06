use std::iter::Iterator;

#[derive(Debug, Eq, PartialEq)]
struct Token(u16, u16);

impl Token {
    fn str<'a>(&self, source_code: &'a str) -> &'a str {
        &source_code[self.0.into()..self.1.into()]
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
    fn advance_to_non_whitespace(&self) -> Option<usize> {
        self.source_code[self.cursor..].find(|c: char| !c.is_whitespace())
    }

    fn advance_to_whitespace(&self) -> Option<usize> {
        self.source_code[self.cursor..].find(|c: char| c.is_whitespace())
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.cursor += self.advance_to_non_whitespace()?;
        let start_pos = to_u16(self.cursor);

        self.cursor = self
            .advance_to_whitespace()
            .map(|offset| self.cursor + offset)
            .unwrap_or(self.source_code.len());

        Some(Token(start_pos, to_u16(self.cursor)))
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
    fn split() {
        let sc = "hello world";
        let mut ts = tokenize(sc);

        let t = ts.next();
        assert_eq!(t, Some(Token(0, 5)));
        assert_eq!(t.unwrap().str(sc), "hello");

        let t = ts.next();
        assert_eq!(t, Some(Token(6, 11)));
        assert_eq!(t.unwrap().str(sc), "world");

        let t = ts.next();
        assert_eq!(t, None);
    }
}
