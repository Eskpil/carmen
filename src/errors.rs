use crate::lexer::Span;
use std::process;

#[derive(Debug, Clone)]
pub enum Step {
    Parsing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Level {
    Error,
    Ignore,
}

#[derive(Debug, Clone)]
pub struct OceanError {
    pub step: Step,
    pub level: Level,
    pub span: Option<Span>,
    pub message: String,
}

impl OceanError {
    pub fn new(level: Level, step: Step, span: Span, message: String) -> Self {
//        panic!("{}:{}:{}  {message}", span.file_name, span.row, span.col);
        Self {
            step,
            level,
            span: Some(span),
            message,
        }
    }

    pub fn report(&self) {
        match &self.span {
            Some(span) => match self.level {
                Level::Error => {
                    eprintln!(
                        "\x1b[31m{}:{}:{}\x1b[0m \x1b[1m{:?}:\x1b[0m {}",
                        span.file_name, span.row, span.col, self.step, self.message
                    );
                    process::exit(1);
                }
                Level::Ignore => {}
            },
            None => match self.level {
                Level::Error => {
                    eprintln!("\x1b[31m{:?}\x1b[0m {}", self.step, self.message);
                    process::exit(1);
                }
                Level::Ignore => {}
            },
        }
    }
}
