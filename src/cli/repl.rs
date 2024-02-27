use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};
use std::fmt;
use std::str::FromStr;

use crate::expression;

const META_PREFIX: char = '.';
const PROMPT: &str = "(bytes) ";

#[derive(Debug)]
pub enum ReplError {
    _NotImplementedError,
}

impl fmt::Display for ReplError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Repl feature not implemented")
    }
}

#[derive(Debug)]
enum MetaToken {
    Unrecognized,
    Exit,
    Vars,
    ClearVars,
    Format,
    Ops,
    NumType,
    Help,
}

macro_rules! MetaTokenStr {
    (MetaToken::Unrecognized) => {
        "UNRECOGNIZED"
    };
    (MetaToken::Exit) => {
        ".exit"
    };
    (MetaToken::Vars) => {
        ".vars"
    };
    (MetaToken::ClearVars) => {
        ".clear_vars"
    };
    (MetaToken::Format) => {
        "./[d,b,x,p,l]"
    };
    (MetaToken::Ops) => {
        ".ops"
    };
    (MetaToken::NumType) => {
        ".num_type"
    };
    (MetaToken::Help) => {
        ".help"
    };

    ($x:ty) => {
        compile_error!(std::concat!(std::stringify!($x), " is not a MetaToken type"))
    };
}

impl fmt::Display for MetaToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match &self {
            MetaToken::Unrecognized => MetaTokenStr!(MetaToken::Unrecognized),
            MetaToken::Exit => MetaTokenStr!(MetaToken::Exit),
            MetaToken::Vars => MetaTokenStr!(MetaToken::Vars),
            MetaToken::ClearVars => MetaTokenStr!(MetaToken::ClearVars),
            MetaToken::Format => MetaTokenStr!(MetaToken::Format),
            MetaToken::Ops => MetaTokenStr!(MetaToken::Ops),
            MetaToken::NumType => MetaTokenStr!(MetaToken::NumType),
            MetaToken::Help => MetaTokenStr!(MetaToken::Help),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug)]
struct ParseMetaTokenError;

impl FromStr for MetaToken {
    type Err = ParseMetaTokenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let t = match s {
            MetaTokenStr!(MetaToken::Exit) => MetaToken::Exit,
            MetaTokenStr!(MetaToken::Vars) => MetaToken::Vars,
            MetaTokenStr!(MetaToken::ClearVars) => MetaToken::ClearVars,
            s if s.starts_with("./") => MetaToken::Format,
            MetaTokenStr!(MetaToken::Ops) => MetaToken::Ops,
            MetaTokenStr!(MetaToken::NumType) => MetaToken::NumType,
            MetaTokenStr!(MetaToken::Help) => MetaToken::Help,
            _ => MetaToken::Unrecognized,
        };
        Ok(t)
    }
}

impl MetaToken {
    pub fn print_help() {
        static META_TOKENS: [MetaToken; 7] = [
            MetaToken::Exit,
            MetaToken::Vars,
            MetaToken::ClearVars,
            MetaToken::Format,
            MetaToken::Ops,
            MetaToken::NumType,
            MetaToken::Help,
        ];
        for i in &META_TOKENS {
            println!("{:?}", i.to_string());
        }
    }
}

fn build_prompt() -> Editor<()> {
    let config = Config::builder().completion_type(CompletionType::List).build();
    Editor::with_config(config)
}

#[cfg(debug_assertions)]
fn print_result(res: expression::format::FormatResult, time: std::time::Duration) {
    match res {
        expression::FormatResult::Ok(num) => {
            println!("{}          [{:?}]", num, time);
        }
        expression::FormatResult::OverWidth(num, width) => {
            println!("Warning: result wider than {}-bit", width);
            println!("{}          [{:?}]", num, time);
        }
    }
}

#[cfg(not(debug_assertions))]
fn print_result(res: expression::format::FormatResult) {
    match res {
        expression::FormatResult::Ok(num) => {
            println!("{}", num);
        }
        expression::FormatResult::OverWidth(num, width) => {
            println!("Warning: result wider than {}-bit", width);
            println!("{}", num);
        }
    }
}

pub fn print_help(program_name: &str) {
    println!("\n{}, a program for calculating simple integer expressions.", program_name);

    println!("Supports mathematical integer ops, binary and logical ops, and simple variables.");
    println!("For a full list of operators, type \".ops\" (all meta commands start with '.').");

    println!("\nThe backing integer type can be printed with \".num_type\".");
    println!("Note: the backing type is unsigned, but division works with two's complement.");
    println!("This means that division behaves like signed arithmetic i.e. -10 / -5 = 2");

    println!("\nVariables can be bound to single word variables using alphanumeric characters and '_'.");
    println!("The value of the last expression is bound to the variable '_' automatically");
    println!("To print the current variable state, type \".vars\"");

    println!("\nSupports arbitrary depth evaluation for expressions, like:");
    println!("\n(bytes) result = (15 << 4)? (2 * 15 - (log2(12)) / 3) % 2 : (12 << 4) < (2 * 4)#5");
    println!("1");
    println!("(bytes) _ << 4");
    println!("16");

    println!("\nAvailble meta commands:");
    MetaToken::print_help();

    println!("\nTo exit the cli, give EOF (^D) or type {}", MetaToken::Exit);
    println!();
}

pub fn main_loop(program_name: &str, program_version: &str) -> Result<(), ReplError> {
    println!("{} {}:", program_name, program_version);

    let mut repl = build_prompt();
    let mut vars = expression::Variables::new();
    let mut formatter = expression::Formatter::new();

    loop {
        let readline = repl.readline(PROMPT);
        match readline {
            Ok(line) => {
                let lpad = line.len() - line.trim_start().len();
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                repl.add_history_entry(line);

                if line.starts_with(META_PREFIX) {
                    match MetaToken::from_str(line).unwrap() {
                        MetaToken::Exit => break,
                        MetaToken::Vars => {
                            let f = |var| match formatter.format(var) {
                                expression::format::FormatResult::Ok(mut res) => {
                                    if &expression::format::PrintStyle::Pretty == formatter.get_fmt() {
                                        res = format!("\n{}\n", res);
                                    }
                                    res
                                }
                                expression::format::FormatResult::OverWidth(res, width) => {
                                    format!("{:<18}  [wider than {}-bit]", res, width)
                                }
                            };

                            vars.print(f);
                        }
                        MetaToken::ClearVars => vars.clear(),
                        MetaToken::Format => match formatter.update_fmt(&line[1..]) {
                            Ok(()) => {
                                println!("Set format: {}", formatter.get_fmt());
                                continue;
                            }
                            Err(e) => {
                                println!("Error: {}", e);
                                continue;
                            }
                        },
                        MetaToken::Ops => expression::print_ops(),
                        MetaToken::NumType => {
                            println!("{}", vars.solver_type());
                        }
                        MetaToken::Help => print_help(program_name),
                        MetaToken::Unrecognized => {
                            println!("unrecognized meta command '{}'", line);
                            continue;
                        }
                    }
                } else {
                    #[cfg(debug_assertions)]
                    let s = std::time::Instant::now();
                    let expr = expression::solve(line, &mut vars);
                    #[cfg(debug_assertions)]
                    let d = s.elapsed();

                    match expr {
                        Ok(res) => {
                            #[cfg(debug_assertions)]
                            print_result(formatter.format(res), d);
                            #[cfg(not(debug_assertions))]
                            print_result(formatter.format(res));
                        }
                        Err(err) => match err {
                            expression::SolverError::ParseUnsignedPowError(_) | expression::SolverError::NotImplementedError => {
                                println!("{}", err)
                            }
                            expression::SolverError::ParseTooLargeNumError(e)
                            | expression::SolverError::InvalidExpressionError(e)
                            | expression::SolverError::UnbalancedBracketError(e)
                            | expression::SolverError::UnsetVariableError(e) => {
                                if e.get_loc() != expression::FEED_OFFSET_END {
                                    println!("{: <1$}^", "", PROMPT.len() + e.get_loc());
                                } else {
                                    println!("{: <1$}^", "", PROMPT.len() + line.len() + lpad);
                                }

                                println!("{}", e);
                            }
                        },
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
