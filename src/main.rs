use actix_web::rt::System;
use log::debug;
use std::env;
use std::sync::mpsc;
use std::thread;

use simplelog::*;
use std::fs::File;

mod cli;
mod expression;
mod server;

enum Mode {
    Cli,
    Server,
}

pub struct Args {
    mode: Mode,
}

impl Args {
    pub fn new(mut args: env::Args) -> Result<Args, &'static str> {
        args.next();

        let mode = match args.next() {
            Some(mode) => match &*mode.to_lowercase() {
                "cli" => Mode::Cli,
                "server" => Mode::Server,
                _ => return Err("Expected first arg 'mode' to be one of [cli, server]"),
            },
            None => return Err("Did not get a mode [cli,server]."),
        };

        Ok(Args { mode })
    }
}

#[actix_web::main]
async fn main() {
    // std::env::set_var("RUST_LOG", "actix_web=debug,rustyline=info");
    // env_logger::init();

    let log_config = ConfigBuilder::new()
        .add_filter_ignore_str("rustyline")
        .set_time_to_local(true)
        .build();
    WriteLogger::init(
        LevelFilter::Debug,
        log_config,
        File::create("server.log").unwrap(),
    )
    .unwrap();

    let args = match Args::new(env::args()) {
        Ok(args) => args,
        Err(e) => {
            eprintln!("Problem parsing arguments: {}", e);
            std::process::exit(-1);
        }
    };

    match args.mode {
        Mode::Server => {
            debug!("Starting server...");

            let (tx, rx) = mpsc::channel();

            thread::spawn(move || {
                let sys = System::new("http-server");

                let srv = server::start_server()?;
                let _ = tx.send(srv);
                sys.run()
            });

            let srv = rx.recv().unwrap();
            if let Err(e) = srv.await {
                eprintln!("Error running server: {}", e);
            }
        }
        Mode::Cli => {
            if let Err(e) = cli::main_loop() {
                eprintln!("Error running cli: {}", e);
            }
        }
    }
}
