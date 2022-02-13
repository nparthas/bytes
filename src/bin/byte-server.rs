use actix_web::rt::System;
use log::debug;
use std::sync::mpsc;
use std::thread;

use simplelog::*;
use std::fs::File;

use byte_server::server;

#[actix_web::main]
async fn main() {
    let log_config = ConfigBuilder::new()
        .add_filter_ignore_str("rustyline")
        .set_time_to_local(true)
        .build();
    WriteLogger::init(LevelFilter::Trace, log_config, File::create("server.log").unwrap()).unwrap();

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
