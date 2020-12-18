use std::path::{Path, PathBuf};

#[derive(Default, Clone, Debug)]
pub struct Location {
    path: PathBuf,
    pos: usize,
    definition: Option<String>,
    pos_definition: Option<usize>,
}

impl Location {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            path: path.into(),
            pos: 1,
            ..Default::default()
        }
    }

    pub fn with_definition(mut self, definition: impl Into<String>) -> Self {
        self.definition = Some(definition.into());
        self.pos_definition = Some(1);
        self
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn add_pos(&mut self, diff: usize) {
        self.pos += diff;
        if let Some(ref mut pos) = self.pos_definition {
            *pos += diff;
        }
    }
}
