use crate::sls::NodeType;
use clap::{Parser, Subcommand};
use core::panic;
use std::io::{Read, Write};
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

    if let Some(ref arg) = args.next() {
        match arg.as_str() {
            "rom" => {
                let filename = args.next().expect("expected rom file");
                let mut buf:String=String::new();
                let mut f = std::fs::File::open(&filename).unwrap().read_to_string(&mut buf);
                let len = buf.len();

                let mut id:usize = 0;

                //aka log base 2
                let mut addr_lines = len.next_power_of_two().trailing_zeros() as usize;

                //make input and output
                let mut inputs:Vec<sls::Node> = Vec::with_capacity(addr_lines as usize);
                for n in 0..addr_lines {
                    inputs.push(
                        {
                            let button = sls::Node::new(NodeType::TOGGLE_BUTTON, Some(n.to_string()), id.to_string());
                            id+=1;
                            button
                        }
                    )
                }
                //make it 8 bit
                const BITWIDTH:usize = 8;
                let mut outputs:Vec<sls::Node> = Vec::with_capacity(BITWIDTH);
                for n in 0..BITWIDTH {
                    outputs.push(
                        {
                            let light = sls::Node::new(NodeType::LIGHT_BULB, Some(n.to_string()), id.to_string());
                            id+=1;
                            light
                        }
                    )
                }
                //add mux
                let mux1 = sls::Node::new(NodeType::TOGGLE_BUTTON, None, id.to_string());
                id+=1;
                //need to make this a vec
                let mut muxes: Vec<Vec<sls::Node>> = Vec::new();
                muxes.push(vec![mux1]);


                //for loop
                let backup = addr_lines;
                let mut num_of_muxes:Vec<usize> = Vec::new();

                let new_addr_lines:usize = if addr_lines<4 {addr_lines} else {4};
                num_of_muxes.push(new_addr_lines);
                addr_lines-=new_addr_lines;
                while addr_lines>0 {
                    let new_addr_lines:usize = if addr_lines<4 {addr_lines} else {4};
                    num_of_muxes.push(new_addr_lines);

                    if addr_lines-new_addr_lines==0 {
                        break;
                    }
                    let mut n = num_of_muxes.len()-2;
                    while n>=0 {
                        n=num_of_muxes[n+1];
                        n-=1;
                    }

                    addr_lines-=new_addr_lines;
                }
                addr_lines=backup;
                for n in num_of_muxes {
                    let mut v = Vec::with_capacity(n);
                    let new_addr_lines:usize = if addr_lines<4 {addr_lines} else {4};
                    let size = 2usize.pow(new_addr_lines as u32);
                    for _ in 0..n {
                        v.push(
                            {
                                let mut mux = sls::Node::new(NodeType::MUX, None, id.to_string());
                                mux.set_size(size);
                                id+=1;
                                mux
                            }
                        )
                    }
                    addr_lines-=new_addr_lines;
                    muxes.push(v);
                }
                //now connect
                let mut wires:Vec<sls::Wire> = Vec::new();
                addr_lines=backup;


                //connect addr lines
                let mut button_n = 0;

                for layer in &muxes {
                    let iter = layer.iter().map(|n|(n.get_size().unwrap().trailing_zeros(),n.get_id()));
                    let mut size = 0;
                    for (num_addr_lines,id) in iter {
                        size=num_addr_lines;
                        for addr_line in 0..num_addr_lines {
                            wires.push(sls::Wire::new(inputs[(button_n+addr_line)as usize].get_id().clone(), 0, id.clone(), addr_line as usize));
                        }
                    }
                    button_n+=size;
                }

                //connect muxes to each other
                let mut layer_i = if muxes.len()>0 {muxes.len()-1} else {0};
                while layer_i>0 {
                    let iter = muxes[layer_i].iter().map(|n|(n.get_size().unwrap(),n.get_id())).enumerate();
                    for (i,(num_inputs,id)) in iter {
                        for n in 0..num_inputs {
                            let prev:&sls::Node=&muxes[layer_i-1][i*num_inputs+n]; 
                            wires.push(sls::Wire::new(prev.get_id().clone(), 0, id.clone(), (n+num_inputs) as usize));
                        }
                    }
                    layer_i-=1;
                }

                //connect const_hi to muxes
                //conenct ligth bulbs to muxes
                //create circ
                let mut components:Vec<sls::Node>=Vec::with_capacity(inputs.len()+outputs.len()+muxes.len());
                let mut muxes_comp = muxes.concat();
                components.extend(muxes_comp);
                components.extend(inputs);
                components.extend(outputs);
                //let mut components:Vec<sls::Node>=[muxes,inputs,outputs].iter().flatten();
                
                let circ = sls::Circuit::new("Rom".to_string(), "ROM".to_string(), components, wires);
                let out_str = serde_json::to_string(&circ).unwrap();
                let mut f = std::fs::OpenOptions::new().write(true).open(&filename).unwrap();
                f.write(out_str.as_str().as_bytes());


            }
            "double" => {
                let f = std::fs::File::open(filename).unwrap();
                let mut n: sls::Circuit = serde_json::from_reader(std::io::BufReader::new(f)).unwrap();
                let mem_re = regex::Regex::new(r"Memory \(\(d+) Byte\)").unwrap();
                let str_num = mem_re.captures(&n.header.name).unwrap().get(1).unwrap().as_str();
                let num_of_bytes:usize = usize::from_str(str_num).unwrap();
                // TODO double Memory (256 Byte) aka RAM
            }
            "sim" => {
                let f = std::fs::File::open(filename).unwrap();
                let mut n: sls::Circuit = serde_json::from_reader(std::io::BufReader::new(f)).unwrap();

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
            unknown_arg => {
                panic!("Unknown arg: {}",unknown_arg);
            }
        }
    }
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
