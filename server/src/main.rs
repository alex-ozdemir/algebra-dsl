extern crate websocket;
extern crate iron;
extern crate router;
extern crate mount;
extern crate staticfile;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;

extern crate khwarizmi;

use iron::mime;
use iron::{Iron, Request, Response, IronResult, status};
use mount::Mount;
use staticfile::Static;
use std::fs::File;
use std::path::Path;
use std::str::FromStr;
use std::thread;
use websocket::header::{Headers,WebSocketProtocol};
use websocket::server::sync::Server;
use websocket::{Message, OwnedMessage};

mod cmd;

use khwarizmi::{Math, TreeIdx};

// Where do we store our messages?
const REPORTFILE: &'static str = "feedback.txt";

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
}

#[derive(Serialize, Deserialize, Debug)]
struct TextMsg {
    options: AutoSimplifyOptions,
    cmd: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct AutoSimplifyOptions {
    constants: bool,
    inverses: bool,
    powers: bool,
}

/// Perform the simplifications that should happen automatically
///
/// For now, just constant simplification
fn auto_simplify(m: Math, opts: &AutoSimplifyOptions) -> Math {
    let mut maybe_m = Some(m.clone());
    if opts.inverses {
        maybe_m.as_mut().map(|m| m.combine_coeff(&TreeIdx::make_empty()));
    }
    if opts.powers {
        maybe_m.as_mut().map(|m| m.simplify_powers(&TreeIdx::make_empty()));
    }
    if opts.constants {
        maybe_m = maybe_m.map(|m| m.simplify_constants());
    }
    maybe_m = maybe_m.map(Math::reduce_identities);
    maybe_m.unwrap_or(m)
}


fn main() {
    // Start listening for http connections
    thread::spawn(move || {

        let mut router = router::Router::new();
        router.get("/", send_mainpage, "mainpage");

        let mut mount = Mount::new();
        mount.mount("/", router)
            .mount("/static/", Static::new(Path::new("static")));

        Iron::new(mount).http("0.0.0.0:8080").unwrap();
    });

    // Start listening for WebSocket connections
    let ws_server = Server::bind("0.0.0.0:2794").unwrap();

    for connection in ws_server.filter_map(Result::ok) {
        // Spawn a new thread for each connection.
        thread::spawn(move || {
            let mut headers = Headers::new();
            headers.set(WebSocketProtocol(vec!["rust-websocket".to_string()]));
            let client = connection.accept_with(&headers).unwrap();

            let (mut receiver, mut sender) = client.split().unwrap();

            let mut formula_num = 0;
            let mut history: Vec<Math> = vec![];
            for message in receiver.incoming_messages() {
                let message: OwnedMessage = message.unwrap();

                match message {
                    OwnedMessage::Close(_) => {
                        let message = Message::close();
                        sender.send_message(&message).unwrap();
                        return;
                    }
                    OwnedMessage::Ping(payload) => {
                        let message = Message::pong(payload);
                        sender.send_message(&message).unwrap();
                    }
                    OwnedMessage::Text(string) => {
                        println!("Received: \"{:?}\"", string);
                        let msg: TextMsg = serde_json::from_str(&string).unwrap();
                        let output = match cmd::Cmd::from_str(&msg.cmd) {
                            Ok(cmd) => cmd.execute(history.last(), &history),
                            Err(e) => Err(e),
                        };
                        let (math_to_send, other_msg) = match output {
                            Ok(cmd::Return::Math(e)) => {
                                let simpler = auto_simplify(e, &msg.options);
                                history.push(simpler.clone());
                                (Some(simpler), None)
                            }
                            Ok(cmd::Return::LaTeXBlock(s)) => {
                                (history.last()
                                     .cloned()
                                     .map(|last| {
                                         history.push(last.clone());
                                         last
                                     }),
                                 Some(format!("LaTeXBlock@{}", s)))
                            }
                            Ok(cmd::Return::LaTeXLine(code)) => {
                                (None, Some(format!("LaTeXLine@{}", code)))
                            }
                            Ok(cmd::Return::NoReturn) => (None, None),
                            Err(e) => {
                                (history.last()
                                     .cloned()
                                     .map(|last| {
                                         history.push(last.clone());
                                         last
                                     }),
                                 Some(format!("Err@{}", e)))
                            }
                        };
                        let check_math_checkbox = other_msg.is_none();
                        if let Some(msg) = other_msg {
                            println!("Output: {:#?}", msg);
                            sender.send_message(&Message::text(msg)).unwrap();
                        }
                        if let Some(math) = math_to_send {
                            let msg =
                                format!("Math@{}@{}@{}", formula_num, check_math_checkbox, math);

                            println!("Output: {:#?}", msg);
                            println!("The current formula is {:#?}", history.last());
                            sender.send_message(&Message::text(msg)).unwrap();

                            formula_num += 1;
                        }
                    }
                    _ => {
                        println!("Got unknown mesage.Closing connection");
                        let message = Message::close();
                        sender.send_message(&message).unwrap();
                    }
                }
            }
        });
    }
}
