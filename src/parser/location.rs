#[derive(Default, Clone, Debug)]
pub struct Location {
    filename: String,
    global_lineno: usize,
    local_lineno: usize,
    global_pos: usize,
    local_pos: usize,
}

impl Location {
    pub(crate) fn set_location(
        &mut self,
        filename: impl Into<String>,
        global_pos: usize,
        local_pos: usize,
    ) {
        self.filename = filename.into();
        self.global_pos = global_pos;
        self.local_pos = local_pos;
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn global_pos(&self) -> usize {
        self.global_pos
    }

    pub fn local_pos(&self) -> usize {
        self.local_pos
    }

    pub(crate) fn set_global_pos(&mut self, pos: usize) {
        self.global_pos = pos;
    }

    pub(crate) fn set_local_pos(&mut self, pos: usize) {
        self.local_pos = pos;
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
