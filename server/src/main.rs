extern crate websocket;
extern crate iron;
#[macro_use(router)]
extern crate router;
extern crate mount;
extern crate staticfile;

extern crate khwarizmi;

use std::fs::File;
use std::path::Path;
use std::thread;
use websocket::{Server, Message, Sender, Receiver};
use websocket::header::WebSocketProtocol;
use websocket::message::Type;
use iron::{Iron, Request, Response, IronResult, status};
use iron::mime;
use staticfile::Static;
use mount::Mount;

use khwarizmi::{Expression, Equation, EquationIdx, AlgebraDSLError};

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
}

enum Cmd {
    Equation(Equation),
    Make(EquationIdx, Expression),
}

fn parse_cmd(s:&str) -> Result<Cmd, AlgebraDSLError> {
    let s: &str = s.trim();
    if s.chars().next().map(|c| c == '$').unwrap_or(false) {
        let inner = s.trim_matches('$');
        println!("Equation: `{}'", inner);
        Ok(Cmd::Equation(Equation::from_str(inner)?))
    } else {
        let sections = s.split('$').collect::<Vec<_>>();
        println!("Sections: `{:?}'", sections);
        if sections.len() == 3 {
            if sections[0].trim() != "make" {
                println!("command: `{}'", sections[0].trim());
                Err(AlgebraDSLError::IllFormattedCommand)
            } else {
                println!("Make");
                let idx = EquationIdx::from_str(sections[1].trim())?;
                println!("Index: {:?}", idx);
                let expr = Expression::from_str(sections[2].trim())?;
                println!("Expression: {:#?}", expr);
                Ok(Cmd::Make(idx, expr))
            }
        } else { Err(AlgebraDSLError::IllFormattedCommand) }
    }
}

fn main() {
    // Start listening for http connections
    thread::spawn(move || {

        let mut router = router::Router::new();
        router.get("/", send_mainpage);

        let mut mount = Mount::new();
        mount.mount("/", router)
             .mount("/static/", Static::new(Path::new("static")));

        Iron::new(mount).http("127.0.0.1:8080").unwrap();
    });

    // Start listening for WebSocket connections
    let ws_server = Server::bind("127.0.0.1:2794").unwrap();

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
            let mut equation = None;
            for message in receiver.incoming_messages() {
                let message: Message = message.unwrap();

                match message.opcode {
                    Type::Close => {
                        let message = Message::close();
                        sender.send_message(&message).unwrap();
                        return;
                    },
                    Type::Ping => {
                        let message = Message::pong(message.payload);
                        sender.send_message(&message).unwrap();
                    },
                    Type::Text => {
                        let string = std::str::from_utf8(&*message.payload).unwrap();
                        println!("Received {}", string);
                        let response = match parse_cmd(string) {
                            Ok(Cmd::Equation(eq)) => {
                                equation = Some(eq);
                                format!("{}", equation.as_ref().unwrap())
                            },
                            Ok(Cmd::Make(idx, expr)) => {
                                match equation {
                                    None => format!("Badness: ∞ (make without an expression)"),
                                    Some(ref mut equation) => {
                                        if equation.replace_with_expr(idx, expr).is_ok() {
                                            format!("{}", equation)
                                        } else {
                                            format!("Badness: ∞ (invalid index)")
                                        }
                                    },
                                }
                            },
                            Err(e) => format!("Badness: ∞ (unrecognized command: {:#?})", e),
                        };
                        sender.send_message(&Message::text(format!("{}@{}", formula_num, response))).unwrap();
                        //sender.send_message(&Message::text(format!("{}@<math xmlns=\"http://www.w3.org/1998/Math/MathML\"> <mrow mathTreeNode=\"\"> <mrow mathTreeNode=\"0\"> <msup mathTreeNode=\"0,0\"> <mi mathTreeNode=\"0,0,0\">x</mi> <mn mathTreeNode=\"0,0,1\">2</mn> </msup> <mo>+</mo> <mrow mathTreeNode=\"0,1\"> <mn mathTreeNode=\"0,1,0\">3</mn> <mo> &#8290; </mo> <mi mathTreeNode=\"0,1,1\">x</mi> </mrow> <mo>+</mo> <mn mathTreeNode=\"0,1\">4</mn> </mrow> <mo>=</mo> <mn mathTreeNode=\"1\"> 5 </mn> </mrow> </math>", formula_num))).unwrap();

                        formula_num += 1;
                    },
                    _ => unreachable!(),
                }
            }
        });
    }
}
