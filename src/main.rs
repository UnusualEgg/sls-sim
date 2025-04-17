use crate::sls::NodeType;
use clap::{Parser, Subcommand};
use core::panic;
use std::str::FromStr;
use std::sync::{
    mpsc,
    mpsc::{Receiver, TryRecvError},
};
use std::time::{Duration, Instant};
use std::{io, thread};

mod sls;
mod types;

enum CircType {
    Custom,
    Star8,
}

fn main() {
    println!("Hello, world!");
    let mut args = std::env::args();
    args.next().unwrap();
    let filename = args.next().expect("gimmeh a file.");
    //deserialize in 2 parts:
    //1: use serde
    let f = std::fs::File::open(filename).unwrap();
    let mut n: sls::Circuit = serde_json::from_reader(std::io::BufReader::new(f)).unwrap();

    if let Some(ref arg) = args.next() {
        match arg.as_str() {
            "double" => {
                let mem_re = regex::Regex::new(r"Memory \(\(d+) Byte\)").unwrap();
                let str_num = mem_re.captures(&n.header.name).unwrap().get(1).unwrap().as_str();
                let num_of_bytes:usize = usize::from_str(str_num).unwrap();
                // TODO double Memory (256 Byte) aka RAM
            }
            unknown_arg => {
                panic!("Unknown arg: {}",unknown_arg);
            }
        }
    }

    //connect bits and stuff
    n.init_circ();
    let circ_type = match n.header.id.0.as_str() {
        "0282d111-5222-4675-80d7-69156904bf03" => CircType::Star8,
        _ => CircType::Custom,
    };

    let stdin_channel = spawn_stdin_channel();
    match circ_type {
        CircType::Star8 => {
            types::star8::run(&mut n, stdin_channel);
        }
        CircType::Custom => {
            types::custom::run(&mut n, stdin_channel);
        }
    }

    //println!("{:#?}", n);
    //println!("{:?}",b.inputs[0].src_output.upgrade())
}
//from https://stackoverflow.com/questions/30012995/how-can-i-read-non-blocking-from-stdin
fn spawn_stdin_channel() -> Receiver<String> {
    let (tx, rx) = mpsc::channel::<String>();
    thread::spawn(move || loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();
        tx.send(buffer).unwrap();
    });
    rx
}
