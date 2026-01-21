use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    io::{self, Write},
    iter,
    path::Path,
};

use ariadne::{Cache, Label, Source};

use crate::tokens::{FileId, Span};

use super::Diagnostic;

#[derive(Clone)]
pub struct DiagnosticCache {
    map: HashMap<FileId, (String, ariadne::Source<String>)>,
}

impl fmt::Debug for DiagnosticCache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (file_id, (filename, _)) in &self.map {
            write!(f, "({file_id:?} -> {filename})")?;
        }

        Ok(())
    }
}

impl DiagnosticCache {
    pub fn new(
        toplevel_file_id: FileId,
        toplevel_filename: impl AsRef<Path>,
        toplevel_content: &str,
    ) -> Self {
        let map = HashMap::from_iter(iter::once((
            toplevel_file_id,
            (
                toplevel_filename.as_ref().to_string_lossy().into_owned(),
                Source::from(toplevel_content.to_string()),
            ),
        )));

        Self { map }
    }
}

impl ariadne::Cache<FileId> for DiagnosticCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &FileId,
    ) -> Result<&ariadne::Source<<Self as Cache<FileId>>::Storage>, impl Debug> {
        match self.map.get(id) {
            Some((_, data)) => Ok(data),
            None => Err(Box::new(format!("Failed to find file with ID {id:?}"))),
        }
    }

    fn display<'a>(&self, id: &'a FileId) -> Option<impl Display + 'a> {
        let (filename, _) = self.map.get(id)?;
        Some(Box::new(filename.clone()))
    }
}

impl ariadne::Span for Span {
    type SourceId = FileId;

    fn source(&self) -> &Self::SourceId {
        &self.file_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

// ============================================================================
// Diagnostic Rendering
// ============================================================================

impl Diagnostic {
    pub fn write<W: Write>(
        &self,
        w: W,
        cache: &mut DiagnosticCache,
        include_colour: bool,
    ) -> io::Result<()> {
        let mut report = ariadne::Report::build(ariadne::ReportKind::Error, self.primary_span)
            .with_code(self.kind.code())
            .with_message(self.message());

        // Add all labels
        for (span, message) in &self.labels {
            report = report.with_label(Label::new(*span).with_message(message.render()));
        }

        // Add notes
        for note in &self.notes {
            report = report.with_note(note.render());
        }

        // Add help if present
        if let Some(help) = &self.help {
            report = report.with_help(help.render());
        }

        report
            .with_config(ariadne::Config::default().with_color(include_colour))
            .finish()
            .write_for_stdout(cache, w)
    }
}
