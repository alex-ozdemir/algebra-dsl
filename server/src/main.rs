extern crate websocket;
extern crate iron;
extern crate router;
extern crate mount;
extern crate staticfile;

extern crate khwarizmi;

use std::fmt;
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

use khwarizmi::{Expression, Equation, TreeIdx, AlgebraDSLError, Indexable, SiblingIndices, TreeIdxRef};

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
}

#[derive(Debug, PartialEq, Clone)]
enum Op {
    Plus,
    Times,
}

#[derive(Debug, PartialEq, Clone)]
enum Cmd {
    New(EqOrExpr),
    Make(Vec<TreeIdx>, Expression),
    Map(Op, Expression),
    Output(Vec<usize>),
}

impl Cmd {
    fn execute(self, e: &Option<EqOrExpr>) -> Result<Return, AlgebraDSLError> {
        match (self, e.as_ref()) {
            (Cmd::New(e), _) => Ok(Return::EqOrExpr(e)),
            (Cmd::Make(mut indices, new_expr), Some(old_expr)) => {
                let mut expr = old_expr.clone();
                if indices.len() == 1 {
                    let idx = indices.pop().expect("unreachable");
                    expr.replace(&idx, new_expr)?;
                    Ok(Return::EqOrExpr(expr))
                } else {
                    if indices.len() > 1 {
                        let sibs = SiblingIndices::from_indices(indices.as_slice())?;
                        expr.replace_siblings(sibs, new_expr)?;
                        Ok(Return::EqOrExpr(expr))
                    } else {
                        Err(AlgebraDSLError::InvalidIdx)
                    }
                }
            },
            (Cmd::Make(_, _), None) => Err(AlgebraDSLError::MakeNeedsExpression),
            (Cmd::Map(op, new_expr), Some(e)) => {
                if let &EqOrExpr::Eq(ref e) = e {
                    let mut eq = e.clone();
                    match op {
                        Op::Times => eq.times_to_both(new_expr),
                        Op::Plus => eq.plus_to_both(new_expr),
                    }
                    Ok(Return::EqOrExpr(EqOrExpr::Eq(eq)))
                } else { Err(AlgebraDSLError::MapExpression) }
            },
            (Cmd::Map(_, _), _) => Err(AlgebraDSLError::MapExpression),
            (Cmd::Output(_), _) => Ok(Return::LaTeXStr("moo!".to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum EqOrExpr {
    Eq(Equation),
    Ex(Expression),
}

impl fmt::Display for EqOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &EqOrExpr::Eq(ref eq) => write!(f, "{}", eq),
            &EqOrExpr::Ex(ref ex) => write!(f, "{}", ex),
        }
    }
}

impl Indexable for EqOrExpr {
    fn get(&self, index: TreeIdxRef) -> Result<&Expression, AlgebraDSLError> {
        match self {
            &EqOrExpr::Eq(ref eq) => eq.get(index),
            &EqOrExpr::Ex(ref ex) => ex.get(index),
        }
    }
    fn get_mut(&mut self, index: TreeIdxRef) -> Result<&mut Expression, AlgebraDSLError> {
        match self {
            &mut EqOrExpr::Eq(ref mut eq) => eq.get_mut(index),
            &mut EqOrExpr::Ex(ref mut ex) => ex.get_mut(index),
        }
    }
}

enum Return {
    EqOrExpr(EqOrExpr),
    LaTeXStr(String),
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
                indices.push(usize::from_str(&rest[..comma_idx]).unwrap());
                rest = rest[comma_idx+1..].trim();
            } else {
                indices.push(usize::from_str(rest).unwrap());
                break;
            }
        }
        return Ok(Cmd::Output(indices));
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
        Ok(Cmd::New(EqOrExpr::Eq(eq)))
    } else {
        Ok(Cmd::New(EqOrExpr::Ex(Expression::from_str(s)?)))
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
            let mut history = vec![];
            let mut equation: Option<EqOrExpr> = None;
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
                            Ok(cmd) => cmd.execute(&equation),
                            Err(e) => Err(e),
                        };
                        let msg = match output {
                            Ok(Return::EqOrExpr(e)) => {
                                equation.take().map(|e| history.push(e));
                                let s = format!("{}@Eqn@{}", formula_num, e);
                                equation = Some(e);
                                s
                            },
                            Ok(Return::LaTeXStr(s)) => format!("{}@LaTeX@{}", formula_num, s),
                            Err(e) => format!("{}@Err@Error: {:?}", formula_num, e),
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
