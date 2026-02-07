use std::collections::HashMap;

use compiler::AnalysisResult;
use lsp_types::Uri;

pub struct FileState {
    pub text: String,
    pub analysis: AnalysisResult,
}

pub type Files = HashMap<Uri, FileState>;
