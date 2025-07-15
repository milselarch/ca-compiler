#[derive(PartialEq, Copy, Clone)]
pub enum TokenBuilderStates {
    Start,
    Accepting,
    Invalid,
    Done,
}

pub struct BaseTokenBuilder {
    built_string: String,
    state: TokenBuilderStates,
    complete: bool
}

impl BaseTokenBuilder {
    pub fn new() -> BaseTokenBuilder {
        BaseTokenBuilder {
            built_string: String::new(),
            state: TokenBuilderStates::Start,
            complete: false,
        }
    }

    pub fn get_state(&self) -> TokenBuilderStates {
        self.state
    }

    pub fn _get_built_str(&self) -> &String {
        &self.built_string
    }

    pub fn set_done(&mut self) {
        self.state = TokenBuilderStates::Done;
    }

    pub fn set_state(&mut self, state: TokenBuilderStates) {
        self.state = state;
    }

    pub fn get_length(&self) -> usize {
        // number of characters collected for token thus far
        self._get_built_str().chars().count()
    }

    pub fn set_complete(&mut self, complete: bool) {
        self.complete = complete;
    }

    pub fn get_complete(&self) -> bool { self.complete }

    pub fn _push_char(&mut self, char: char) {
        self.built_string.push(char);
    }
}