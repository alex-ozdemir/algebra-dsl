extern crate websocket;
extern crate iron;
#[macro_use(router)]
extern crate router;
extern crate mount;
extern crate staticfile;

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

// The HTTP server handler
fn send_mainpage(_: &mut Request) -> IronResult<Response> {

    Ok(Response::with((status::Ok,
                       mime::Mime(mime::TopLevel::Text, mime::SubLevel::Html, Vec::new()),
                       File::open(Path::new("static/index.html")).unwrap())))
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
                        println!("Received {}", std::str::from_utf8(&*message.payload).unwrap());
                        sender.send_message(&Message::text("<math id=\"Moop\" xmlns=\"http://www.w3.org/1998/Math/MathML\"> <mrow class=\"mathTreeNode\" id=\"asdf1\"> <mstyle class=\"mathTreeNode\" id=\"asdf2\" displaystyle=\"true\"> <munderover class=\"mathTreeNode\" id=\"asdf3\"> <mo class=\"mathTreeNode\" id=\"asdf4\">&int;</mo> <mn class=\"mathTreeNode\" id=\"asdf5\">4</mn> <mn class=\"mathTreeNode\" id=\"asdf6\">8</mn> </munderover> </mstyle> <mfrac class=\"mathTreeNode\" id=\"asdf7\" linethickness=\"1\"> <mn class=\"mathTreeNode\" id=\"asdf8\">1</mn> <mrow class=\"mathTreeNode\" id=\"asdf9\"> <mfrac class=\"mathTreeNode\" id=\"asdf10\" linethickness=\"1\"> <mn class=\"mathTreeNode\" id=\"asdf11\">8</mn> <mrow class=\"mathTreeNode\" id=\"asdf12\"> <mfrac class=\"mathTreeNode\" id=\"asdf13\" linethickness=\"1\"> <mrow class=\"mathTreeNode\" id=\"asdf14\"> <msup class=\"mathTreeNode\" id=\"asdf15\"> <mi class=\"mathTreeNode\" id=\"asdf16\">x</mi> <mrow class=\"mathTreeNode\" id=\"asdf17\"> <mn class=\"mathTreeNode\" id=\"asdf18\">2</mn> <mfrac class=\"mathTreeNode\" id=\"asdf19\" linethickness=\"1\"> <mn class=\"mathTreeNode\" id=\"asdf20\">3</mn> <mi class=\"mathTreeNode\" id=\"asdf21\">y</mi> </mfrac> </mrow> </msup> <mi class=\"mathTreeNode\" id=\"asdf22\">y</mi> </mrow> <mrow class=\"mathTreeNode\" id=\"asdf23\"> <msqrt class=\"mathTreeNode\" id=\"asdf24\"> <mrow class=\"mathTreeNode\" id=\"asdf25\"> <mn class=\"mathTreeNode\" id=\"asdf26\">1</mn> <mo class=\"mathTreeNode\" id=\"asdf27\">-</mo> <msup class=\"mathTreeNode\" id=\"asdf28\"> <mi class=\"mathTreeNode\" id=\"asdf29\">y</mi> <mn class=\"mathTreeNode\" id=\"asdf30\">2</mn> </msup> </mrow> </msqrt> </mrow> </mfrac> </mrow> </mfrac> </mrow> </mfrac> <mrow class=\"mathTreeNode\" id=\"asdf31\"> <mi class=\"mathTreeNode\" id=\"asdf32\">d</mi> <mi class=\"mathTreeNode\" id=\"asdf33\">x</mi> </mrow> </mrow> </math>".to_string())).unwrap();
                    },
                    _ => unreachable!(),
                }
            }
        });
    }
}
