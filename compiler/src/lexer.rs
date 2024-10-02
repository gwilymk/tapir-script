use logos::{Logos, SpannedIter};

use crate::tokens::{FileId, LexicalError, Token};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token<'input>>,
    file_id: FileId,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, file_id: FileId) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
            file_id,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, span) = self.token_stream.next()?;

        Some(match token {
            Ok(token) => Ok((span.start, token, span.end)),
            Err(err) => Err(err.with_span(self.file_id, span.start, span.end)),
        })
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use insta::{assert_ron_snapshot, glob};

    use super::*;

    #[test]
    fn snapshot_tests() {
        glob!("snapshot_tests", "lexer/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&input, FileId::new(0));
            let output = lexer
                .map(|token| token.map(|(_, token, _)| token))
                .collect::<Vec<_>>();

            assert_ron_snapshot!(output);
        });
    }
}
