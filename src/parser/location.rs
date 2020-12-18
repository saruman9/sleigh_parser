#[derive(Default, Clone, Debug)]
pub struct Location {
    path: String,
    pos: usize,
    definition: Option<String>,
    pos_definition: Option<usize>,
}

impl Location {
    pub fn new(filename: impl Into<String>, pos: usize) -> Self {
        Self {
            path: filename.into(),
            pos,
            ..Default::default()
        }
    }

    pub fn with_definition(mut self, definition: impl Into<String>, pos_definition: usize) -> Self {
        self.definition = Some(definition.into());
        self.pos_definition = Some(pos_definition);
        self
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
        if let Some(ref mut pos) = self.pos_definition {
            *pos += diff;
        }
    }
}
