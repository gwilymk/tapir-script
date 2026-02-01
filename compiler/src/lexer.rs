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
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, file_id: FileId) -> Self {
        Self {
            token_stream: Token::lexer(input).spanned(),
            file_id,
            comments: vec![],
            input,
        }
    }

    pub fn iter(&mut self) -> LexerIter<'_, 'input> {
        LexerIter { lexer: self }
    }

    pub fn into_comment_table(self) -> CommentTable<'input> {
        CommentTable {
            comments: self.comments,
            input: self.input,
        }
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

struct Comment<'input> {
    content: &'input str,
    kind: CommentKind,
    span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CommentKind {
    Regular,
    Doc,
}

pub struct CommentTable<'input> {
    comments: Vec<Comment<'input>>,
    input: &'input str,
}

impl<'input> CommentTable<'input> {
    pub fn doc_for_item(&self, item: Span) -> Option<String> {
        let mut current_start = item.start;
        let mut doc_lines = vec![];

        loop {
            let matching = self
                .comments
                .iter()
                .filter(|c| c.span.file_id == item.file_id)
                .filter(|c| c.kind == CommentKind::Doc)
                .filter(|c| c.span.end <= current_start)
                .filter(|c| self.input[c.span.end..current_start].trim().is_empty())
                .max_by_key(|c| c.span.end);

            if let Some(comment) = matching {
                doc_lines.push(comment.content.trim_start_matches("##").trim());
                current_start = comment.span.start;
            } else {
                break;
            }
        }

        if doc_lines.is_empty() {
            None
        } else {
            doc_lines.reverse();
            Some(doc_lines.join("\n"))
        }
    }

    pub fn merge_from(&mut self, other: Self) {
        self.comments.extend(other.comments);
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

            let mut lexer = Lexer::new(&input, FileId::new(0));
            let output = lexer
                .iter()
                .map(|token| token.map(|(_, token, _)| token))
                .collect::<Vec<_>>();

            assert_ron_snapshot!(output);
        });
    }
}
