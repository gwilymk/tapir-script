use logos::{Logos, SpannedIter};

use crate::{
    Span,
    tokens::{FileId, LexicalError, Token},
};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token<'input>>,
    file_id: FileId,
    comments: Vec<Comment<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, file_id: FileId) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
            file_id,
            comments: vec![],
        }
    }

    pub fn iter(&mut self) -> LexerIter<'_, 'input> {
        LexerIter { lexer: self }
    }
}

pub struct LexerIter<'l, 'input> {
    lexer: &'l mut Lexer<'input>,
}

impl<'l, 'input> Iterator for LexerIter<'l, 'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (token, span) = self.lexer.token_stream.next()?;

            match token {
                Ok(token) => {
                    if let Token::Comment(comment) = token {
                        let kind = if comment.starts_with("##") {
                            CommentKind::Doc
                        } else {
                            CommentKind::Regular
                        };

                        self.lexer.comments.push(Comment {
                            content: comment,
                            kind,
                            span: Span::new(self.lexer.file_id, span.start, span.end),
                        });

                        continue;
                    }

                    return Some(Ok((span.start, token, span.end)));
                }
                Err(err) => {
                    return Some(Err(err.with_span(self.lexer.file_id, span.start, span.end)));
                }
            };
        }
    }
}

pub struct Comment<'input> {
    content: &'input str,
    kind: CommentKind,
    span: Span,
}

pub enum CommentKind {
    Regular,
    Doc,
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

            let mut lexer = Lexer::new(&input, FileId::new(0));
            let output = lexer
                .iter()
                .map(|token| token.map(|(_, token, _)| token))
                .collect::<Vec<_>>();

            assert_ron_snapshot!(output);
        });
    }
}
