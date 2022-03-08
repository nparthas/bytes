use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};
use std::{env, fmt};

use crate::expression;
use crate::expression::solver::IsSigned;

const META_PREFIX: char = '.';
const PROMPT: &str = "# ";

#[derive(Debug)]
pub enum ReplError {
    _NotImplementedError,
}

impl fmt::Display for ReplError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Repl feature not implemented")
    }
}

enum MetaToken {
    Exit,
    Vars,
    Format,
    Ops,
    NumType,
    Unrecognized,
}

fn tokenize_meta(line: &str) -> MetaToken {
    match line {
        ".exit" => MetaToken::Exit,
        ".vars" => MetaToken::Vars,
        line if line.starts_with("./") => MetaToken::Format,
        ".ops" => MetaToken::Ops,
        ".num_type" => MetaToken::NumType,
        _ => MetaToken::Unrecognized,
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

pub fn main_loop() -> Result<(), ReplError> {
    let version_info: String = format!("{} {}", get_program_name(), env!("CARGO_PKG_VERSION"));

    println!("{} cli:", version_info);

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
                    match tokenize_meta(line) {
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
                            // type is signed
                            let signed = if expression::SolverInt::is_signed() {
                                "signed"
                            } else {
                                "unsigend"
                            };

                            println!("{}-bit {} integer", expression::SolverInt::BITS, signed);
                        }
                        MetaToken::Unrecognized => {
                            println!("unrecognized meta command '{}'", line);
                            continue;
                        }
                    }
                } else {
                    #[cfg(debug_assertions)]
                    let s = std::time::Instant::now();
                    let expr = expression::solve(line, Some(&mut vars));
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

fn get_program_name() -> String {
    String::from(
        env::current_exe()
            .unwrap()
            .file_name()
            .ok_or("unable to convert file name")
            .unwrap()
            .to_str()
            .unwrap(),
    )
}
