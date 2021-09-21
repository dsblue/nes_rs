#[allow(dead_code)]

use std::option::Option;

#[derive(Debug,PartialEq,PartialOrd)]
pub enum DebugCommand {
    Go,
    Run(u64),
    BreakPoint(u32),
    Display,
    Quit
}

pub fn debug_parse(s :&str) -> Option<DebugCommand> {

    let mut token = s.split_whitespace();
    
    if let Some(a) = token.next() {
        match a.to_lowercase().as_str() {
            "go" => Some(DebugCommand::Go),
            "quit" => Some(DebugCommand::Quit),
            "display" => Some(DebugCommand::Display),
            "run" => {
                if let Some(num) = token.next()
                    .and_then(|x| x.parse::<u64>().ok()) {
                        Some(DebugCommand::Run(num))
                } else {
                    None
                }
            }
            _ => None
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_run() {
        assert_eq!(debug_parse("Go").unwrap(), DebugCommand::Go);
        assert_eq!(debug_parse("xxx"), None);
        assert_eq!(debug_parse("run 100").unwrap(), DebugCommand::Run(100));
        assert_eq!(debug_parse("run xxx"), None);
        assert_eq!(debug_parse("run xxx 123"), None);
        assert_eq!(debug_parse("run x x x"), None);
    }

    #[test]
    fn start_debug() {
        
    }

}