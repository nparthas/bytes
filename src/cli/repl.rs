use log::info;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Config, Editor};
use std::{env, fmt};

const META_PREFIX: char = '.';

use crate::expression;

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
    Unrecognized,
}

fn tokenize_meta(line: &str) -> MetaToken {
    match line {
        ".exit" => MetaToken::Exit,
        _ => MetaToken::Unrecognized,
    }
}

fn build_prompt() -> Editor<()> {
    let config = Config::builder().completion_type(CompletionType::List).build();
    Editor::with_config(config)
}

pub fn main_loop() -> Result<(), ReplError> {
    let version_info: String = format!("{} {}", get_program_name(), env!("CARGO_PKG_VERSION"));

    info!("{} cli start", version_info);
    println!("{} cli:", version_info);

    let mut repl = build_prompt();

    loop {
        let readline = repl.readline("# ");
        match readline {
            Ok(line) => {
                let lpad = line.len() - line.trim_start().len();
                let line = line.trim();
                if line == "" {
                    continue;
                }

                if line.starts_with(META_PREFIX) {
                    match tokenize_meta(&line) {
                        MetaToken::Exit => break,
                        MetaToken::Unrecognized => {
                            println!("unrecognized meta command '{}'", line);
                            continue;
                        }
                    }
                } else {
                    repl.add_history_entry(line);

                    let s = std::time::Instant::now();
                    let expr = expression::solve(&line);
                    let d = s.elapsed();

                    match expr {
                        Ok(res) => println!("{}    [{:?}]", expression::format(res), d),
                        Err(err) => match err {
                            expression::SolverError::ParseUnsignedPowError(_)
                            | expression::SolverError::NotImplementedError => println!("{}", err),
                            expression::SolverError::InvalidExpressionError(e)
                            | expression::SolverError::UnbalancedBracketError(e) => {
                                if e.get_loc() != usize::MAX {
                                    println!("{: <1$}^", "", e.get_loc() + 2);
                                } else {
                                    println!("{: <1$}^", "", line.len() + lpad + 2);
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
