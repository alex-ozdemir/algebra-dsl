extern crate websocket;
extern crate iron;
extern crate router;
extern crate mount;
extern crate staticfile;

extern crate khwarizmi;

use iron::mime;
use iron::{Iron, Request, Response, IronResult, status};
use mount::Mount;
use staticfile::Static;
use std::fs::File;
use std::path::Path;
use std::str::FromStr;
use std::thread;
use websocket::header::WebSocketProtocol;
use websocket::message::Type;
use websocket::{Server, Message, Sender, Receiver};

mod cmd;

use khwarizmi::EqOrExpr;

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
}


/// Perform the simplifications that should happen automatically
///
/// For now, just constant simplification
fn auto_simplify(e: EqOrExpr) -> EqOrExpr {
    e.simplify_constants()
}


fn main() {
    // Start listening for http connections
    thread::spawn(move || {

        let mut router = router::Router::new();
        router.get("/", send_mainpage);

        let mut mount = Mount::new();
        mount.mount("/", router)
            .mount("/static/", Static::new(Path::new("static")));

        Iron::new(mount).http("0.0.0.0:8080").unwrap();
    });

    // Start listening for WebSocket connections
    let ws_server = Server::bind("0.0.0.0:2794").unwrap();

    for connection in ws_server {
        // Spawn a new thread for each connection.
        thread::spawn(move || {
            let request = connection.unwrap().read_request().unwrap(); // Get the request
            let headers = request.headers.clone(); // Keep the headers so we can check them

            request.validate().unwrap(); // Validate the request

            let mut response = request.accept(); // Form a response

            if let Some(&WebSocketProtocol(ref protocols)) = headers.get() {
                if protocols.contains(&("rust-websocket".to_string())) {
                    // We have a protocol we want to use
                    response.headers.set(WebSocketProtocol(vec!["rust-websocket".to_string()]));
                }
            }

            let client = response.send().unwrap(); // Send the response

            let (mut sender, mut receiver) = client.split();

            let mut formula_num = 0;
            let mut history: Vec<EqOrExpr> = vec![];
            for message in receiver.incoming_messages() {
                let message: Message = message.unwrap();

                match message.opcode {
                    Type::Close => {
                        let message = Message::close();
                        sender.send_message(&message).unwrap();
                        return;
                    }
                    Type::Ping => {
                        let message = Message::pong(message.payload);
                        sender.send_message(&message).unwrap();
                    }
                    Type::Text => {
                        let string = std::str::from_utf8(&*message.payload).unwrap();
                        println!("Received {}", string);
                        let output = match cmd::Cmd::from_str(string) {
                            Ok(cmd) => cmd.execute(history.last(), &history),
                            Err(e) => Err(e),
                        };
                        let msg = match output {
                            Ok(cmd::Return::EqOrExpr(e)) => {
                                let simpler = auto_simplify(e);
                                let s = format!("{}@Math@{}", formula_num, simpler);
                                history.push(simpler);
                                s
                            }
                            Ok(cmd::Return::LaTeXStr(s)) => format!("{}@LaTeX@{}", formula_num, s),
                            Err(e) => format!("{}@Err@Error: {:?}", formula_num, e),
                        };

                        formula_num += 1;
                        println!("Output: {:#?}", msg);
                        println!("The current formula is {:#?}", history.last());
                        sender.send_message(&Message::text(msg)).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
        });
    }
}
