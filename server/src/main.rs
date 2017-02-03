extern crate websocket;
extern crate iron;
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

use khwarizmi::{Expression, Equation, TreeIdx, AlgebraDSLError, Indexable, SiblingIndices};

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
}

enum Op {
    Plus,
    Times,
}

enum Cmd {
    Equation(Equation),
    Expression(Expression),
    Make(Vec<TreeIdx>, Expression),
    Map(Op, Expression),
    Output(Vec<usize>),
}

enum Return<'a> {
    Equation(Box<Indexable>),
    LaTeXStr(&'a str),
}

fn parse_cmd(s: &str) -> Result<Cmd, AlgebraDSLError> {
    let s: &str = s.trim();
    if s.starts_with("make ") {
        let mut indices = Vec::new();
        let mut rest: &str = &s[5..].trim();
        while rest.starts_with("#") {
            let idx_end = rest.find(')').ok_or(AlgebraDSLError::IllFormattedIndex)?;
            let idx = TreeIdx::from_str(&rest[..(idx_end + 1)])?;
            indices.push(idx);
            rest = &rest[(idx_end + 1)..];
        }
        let expr = Expression::from_str(rest)?;
        return Ok(Cmd::Make(indices, expr));
    }
    if s.starts_with("output ") {
        let mut indices = Vec::new();
        let mut rest: &str = &s[7..].trim();
        loop {
            use std::str::FromStr;
            if let Some(comma_idx) = rest.find(',') {
                indices.push(usize::from_str(&rest[..comma_idx]));
                rest = rest[comma_idx+1..].trim();
            } else {
                indices.push(usize::from_str(rest));
                break;
            }
        }
    }
    if s.starts_with("+") {
        let rest = &s[1..].trim();
        let expr = Expression::from_str(rest)?;
        return Ok(Cmd::Map(Op::Plus, expr));
    }
    if s.starts_with("*") {
        let rest = &s[1..].trim();
        let expr = Expression::from_str(rest)?;
        return Ok(Cmd::Map(Op::Times, expr));
    }
    if let Some(eq) = Equation::from_str(s).ok() {
        Ok(Cmd::Equation(eq))
    } else {
        Ok(Cmd::Expression(Expression::from_str(s)?))
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
            let mut equation: Option<Box<Indexable>> = None;
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
                        let output = match parse_cmd(string) {
                            Ok(Cmd::Expression(ex)) => {
                                Ok(Return::Equation(Box::new(ex)))
                            }
                            Ok(Cmd::Equation(eq)) => {
                                Ok(Return::Equation(Box::new(eq)))
                            }
                            Ok(Cmd::Make(mut indices, expr)) => {
                                match (equation.as_mut(), indices.len()) {
                                    (None, _) => Err("make without an expression".to_string()),
                                    (Some(equation), 1) => {
                                        indices.pop().ok_or(AlgebraDSLError::InvalidIdx).map(|idx|
                                            Return::Equation(equation.clone().replace(
                                                &idx, expr).ok_or(AlgebraDSLError::InvalidIdx)))
                                    },
                                    (Some(equation), n) if n > 1 => {
                                        SiblingIndices::from_indices(indices.as_slice()).map(|sibs|
                                            Return::Equation(equation.clone().replace_siblings(
                                                sibs, expr).ok_or(AlgebraDSLError::InvalidIdx)))
                                    },
                                    _ => Err(AlgebraDSLError::InvalidIdx),
                                }
                            }
                            Ok(Cmd::Map(Op::Times, expr)) => {
                                equation.as_mut().and_then(|i| i.as_equation())
                                    .ok_or(AlgebraDSLError::MapExpression).map(|eq|
                                        Return::Equation(eq.clone().times_to_both(expr)))
                            }
                            Ok(Cmd::Map(Op::Plus, expr)) => {

                                equation.as_mut().and_then(|i| i.as_equation())
                                    .ok_or(AlgebraDSLError::MapExpression).map(|eq|
                                        Return::Equation(eq.clone().plus_to_both(expr)))
                            }
                            Ok(Cmd::Output(eqn_nums)) => {
                                Return::LaTeXStr("moo!")
                            }
                            Err(e) => Some(format!("unrecognized command: {:#?}", e)),
                        };
                        let msg = match output {
                            Ok(Return::Equation(e)) => format!("{}@Eqn@{}", formula_num, e),
                            Ok(Return::LaTeXStr(s)) => format!("{}@Latex@{}", formula_num, s),
                            Err(e) => format!("{}@Err@Error: {}", formula_num, e),
                        };
                        formula_num += 1;
                        println!("Output: {:#?}", msg);
                        sender.send_message(&Message::text(msg)).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
        });
    }
}
