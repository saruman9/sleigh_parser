#[derive(Default, Clone, Debug)]
pub struct Location {
    path: String,
    pos: usize,
}

impl Location {
    pub fn new(filename: impl Into<String>, pos: usize) -> Self {
        Self {
            path: filename.into(),
            pos,
        }
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn add_pos(&mut self, diff: usize) {
        self.pos += diff;
    }
}
