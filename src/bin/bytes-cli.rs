use bytes_cli::cli;
use std::env;

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

fn main() {
    let program_name: String = get_program_name();
    let program_version: &str = env!("CARGO_PKG_VERSION");

    // only handle --help being passed
    let help = "--help".to_string();
    if env::args().any(|x| x == help) {
        cli::print_help(&program_name);
        println!("To run the cli, call without \"--help\"\n");
        return;
    }

    if let Err(e) = cli::main_loop(&program_name, program_version) {
        eprintln!("Error running cli: {}", e);
    }
}
