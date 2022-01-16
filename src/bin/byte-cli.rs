use byte_server::cli;

fn main() {
    if let Err(e) = cli::main_loop() {
        eprintln!("Error running cli: {}", e);
    }
}
