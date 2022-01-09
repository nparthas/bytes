use actix_web::{error, middleware, web, App, HttpResponse, HttpServer};
use log::debug;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct MyObj {
    name: String,
    number: i32,
}

/// This handler uses json extractor
async fn index(item: web::Json<MyObj>) -> HttpResponse {
    debug!("model: {:?}", &item);
    HttpResponse::Ok().json(item.0) // <- send response
}

pub fn start_server() -> std::io::Result<actix_web::dev::Server> {
    Ok(HttpServer::new(|| {
        App::new()
            // enable logger
            .wrap(middleware::Logger::default())
            .data(
                web::JsonConfig::default()
                    .limit(4096)
                    .content_type(|_| true)
                    .error_handler(|err, _req| {
                        let err = format!(r#"{{"error":"{}"}}"#, err);
                        error::InternalError::from_response(
                            err.clone(),
                            HttpResponse::BadRequest()
                                .content_type("application/json")
                                .body(err),
                        )
                        .into()
                    }),
            )
            .service(web::resource("/").route(web::post().to(index)))
    })
    .bind("127.0.0.1:8080")?
    .shutdown_timeout(60)
    .run())
}
