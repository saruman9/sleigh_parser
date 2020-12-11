#[derive(Default, Clone, Debug)]
pub struct Location {
    filename: String,
    global_lineno: usize,
    local_lineno: usize,
    pos: usize,
}

impl Location {
    pub(crate) fn set_location(
        &mut self,
        filename: impl Into<String>,
        local_lineno: usize,
        pos: usize,
    ) {
        self.filename = filename.into();
        self.local_lineno = local_lineno;
        self.pos = pos;
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub(crate) fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub(crate) fn inc_global_lineno(&mut self) {
        self.global_lineno += 1;
    }

    pub(crate) fn inc_local_lineno(&mut self) {
        self.local_lineno += 1;
    }

    pub fn local_lineno(&self) -> usize {
        self.local_lineno
    }

    pub fn global_lineno(&self) -> usize {
        self.global_lineno
    }
}
