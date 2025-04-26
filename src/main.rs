#[deny(unused_must_use)]
use crate::sls::NodeType;
use core::panic;
use std::io::{Read, Write};
use std::str::FromStr;
use std::sync::{
    mpsc,
    mpsc::Receiver,
};
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
                let filename_rom = args.next().expect("expected rom file");
                let mut buf: Vec<u8> = Vec::new();
                std::fs::File::open(&filename_rom)
                    .unwrap()
                    .read_to_end(&mut buf)
                    .unwrap();
                let len = buf.len();

                let mut id: usize = 0;

                //aka log base 2
                let mut addr_lines = len.next_power_of_two().trailing_zeros() as usize;
                println!("addr_lines: {}", addr_lines);

                //make input and output
                let mut inputs: Vec<sls::Node> = Vec::with_capacity(addr_lines as usize);
                for n in 0..(addr_lines) {
                    inputs.push({
                        let button = sls::Node::new(
                            NodeType::TOGGLE_BUTTON,
                            Some(n.to_string()),
                            id.to_string(),
                        )
                        .at(-100.0, n as f32 * 100.0);
                        id += 1;
                        button
                    })
                }
                //make it 8 bit
                const BITWIDTH: usize = 8;
                let mut outputs: Vec<sls::Node> = Vec::with_capacity(BITWIDTH);
                for n in 0..BITWIDTH {
                    outputs.push({
                        let light = sls::Node::new(
                            NodeType::LIGHT_BULB,
                            Some(n.to_string()),
                            id.to_string(),
                        )
                        .at(100.0, n as f32 * 100.0);
                        id += 1;
                        light
                    })
                }
                //need to make this a vec
                //muxes[bit][layer(x)][y]
                let mut muxes: Vec<Vec<Vec<sls::Node>>> = Vec::new();
                //add mux
                //let mut mux1 = sls::Node::new(NodeType::MUX, None, id.to_string());
                //let new_addr_lines:usize = if addr_lines<4 {addr_lines} else {4};
                //let size = 2usize.pow(new_addr_lines as u32);
                //mux1.set_size(size);
                //id+=1;
                //muxes.push(vec![mux1]);

                //for loop
                let backup = addr_lines;
                let mut num_of_muxes: Vec<usize> = Vec::new();

                //actually we can just start left to right
                //ignore what type of input is left
                //only check how many inputs
                //start with addr_lines
                let mut num_of_inputs = len;
                while num_of_inputs > 1 {
                    if num_of_inputs <= 16 {
                        num_of_muxes.push(1);
                        num_of_inputs = 1;
                    } else {
                        let mut num = num_of_inputs as usize;
                        if num & 0b01111 != 0 {
                            //doesn't evenly split into 16
                            num = (num / 16) + 1;
                        } else {
                            num /= 16;
                        }
                        //num of 16 guys
                        num_of_muxes.push(num);
                        num_of_inputs = num;
                    }
                    //let new_addr_lines:usize = if addr_lines<=4 {addr_lines} else {4};
                    //num_of_muxes.push(new_addr_lines);

                    //addr_lines-=new_addr_lines;
                    //let mut n = num_of_muxes.len()-2;
                    //while n>0 {
                    //    n=num_of_muxes[n+1];
                    //    n-=1;
                    //}
                }
                println!("num_of_muxes: {:?}", num_of_muxes);
                for bit in 0..BITWIDTH {
                    addr_lines = backup;
                    let mut muxes_bit = Vec::with_capacity(num_of_muxes.len());
                    for n in &num_of_muxes {
                        let mut v = Vec::with_capacity(*n);
                        let new_addr_lines: usize = if addr_lines <= 4 { addr_lines } else { 4 };
                        let size = 2usize.pow(new_addr_lines as u32);
                        for _ in 0..*n {
                            v.push({
                                let mut mux = sls::Node::new(NodeType::MUX, None, id.to_string());
                                mux.set_size(size);
                                id += 1;
                                mux
                            })
                        }
                        addr_lines -= new_addr_lines;
                        muxes_bit.push(v);
                    }
                    muxes.push(muxes_bit);
                }
                //now connect
                let mut wires: Vec<sls::Wire> = Vec::new();

                //connect addr lines
                for bit in 0..BITWIDTH {
                    let mut button_n = 0;

                    for layer in &muxes[bit] {
                        let iter = layer
                            .iter()
                            .map(|n| (n.get_size().unwrap().trailing_zeros(), n.get_id()));
                        let mut size = 0;
                        for (num_addr_lines, id) in iter {
                            size = num_addr_lines;
                            for addr_line in 0..num_addr_lines {
                                //TODO button+addr_line
                                wires.push(sls::Wire::new(
                                    inputs[(button_n + addr_line) as usize].get_id().clone(),
                                    0,
                                    id.clone(),
                                    (num_addr_lines - addr_line - 1) as usize,
                                ));
                            }
                        }
                        button_n += size;
                    }
                }

                //connect muxes to each other
                for bit in 0..BITWIDTH {
                    let mut layer_i = if muxes[bit].len() > 0 {
                        muxes[bit].len() - 1
                    } else {
                        0
                    };
                    while layer_i > 0 {
                        let iter = muxes[bit][layer_i]
                            .iter()
                            .map(|n| (n.get_size().unwrap(), n.get_id()))
                            .enumerate();
                        for (i, (num_inputs, id)) in iter {
                            for n in 0..num_inputs {
                                let prev: &sls::Node = &muxes[bit][layer_i - 1][i * num_inputs + n];
                                wires.push(sls::Wire::new(
                                    prev.get_id().clone(),
                                    0,
                                    id.clone(),
                                    (n + num_inputs) as usize,
                                ));
                            }
                        }
                        layer_i -= 1;
                    }
                }

                //connect const_hi to muxes
                let hi = sls::Node::new(NodeType::HIGH_CONSTANT, None, id.to_string());
                id += 1;
                for bit in 0..BITWIDTH {
                    if let Some(first_muxes) = muxes[bit].first() {
                        let first_addr_size = if backup <= 4 { backup } else { 4 };
                        let first_size = 2usize.pow(first_addr_size as u32);
                        let mut total_byte: usize = 0;
                        for mux in first_muxes {
                            for mux_bit in 0..first_size {
                                let byte: u8 = if total_byte < buf.len() {
                                    buf[total_byte]
                                } else {
                                    0
                                };
                                if byte & (1 << bit) != 0 {
                                    wires.push(sls::Wire::new(
                                        hi.get_id().clone(),
                                        0,
                                        mux.get_id().clone(),
                                        first_addr_size + mux_bit,
                                    ));
                                }
                                total_byte += 1;
                            }
                        }
                    }
                }
                //conenct ligth bulbs to muxes
                for bit in 0..BITWIDTH {
                    if let Some(last_muxes) = muxes[bit].last() {
                        //should only be one in last layer
                        if let Some(last_mux) = last_muxes.first() {
                            wires.push(sls::Wire::new(
                                last_mux.get_id().clone(),
                                0,
                                outputs[bit].get_id().clone(),
                                0,
                            ));
                        }
                    }
                }

                //create circ
                let mut components: Vec<sls::Node> =
                    Vec::with_capacity(inputs.len() + outputs.len() + muxes.len());
                let muxes_comp = muxes.concat().concat();
                components.extend(muxes_comp);
                components.extend(inputs);
                components.extend(outputs);
                components.push(hi);
                //let mut components:Vec<sls::Node>=[muxes,inputs,outputs].iter().flatten();

                let circ =
                    sls::Circuit::new("Rom".to_string(), "ROM".to_string(), components, wires);
                let out_str = serde_json::to_string_pretty(&circ).unwrap();
                let mut f = std::fs::OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(&filename)
                    .unwrap();
                f.write(out_str.as_str().as_bytes()).unwrap();
            }
            "double" => {
                let f = std::fs::File::open(filename).unwrap();
                let mut n: sls::Circuit =
                    serde_json::from_reader(std::io::BufReader::new(f)).unwrap();
                let mem_re = regex::Regex::new(r"Memory \(\(d+) Byte\)").unwrap();
                let str_num = mem_re
                    .captures(&n.header.name)
                    .unwrap()
                    .get(1)
                    .unwrap()
                    .as_str();
                let num_of_bytes: usize = usize::from_str(str_num).unwrap();
                // TODO double Memory (256 Byte) aka RAM
            }
            "sim" => {
                let f = std::fs::File::open(filename).unwrap();
                let mut n: sls::Circuit =
                    serde_json::from_reader(std::io::BufReader::new(f)).unwrap();

                //connect bits and stuff
                n.init_circ();
                let circ_type = match n.header.id.0.as_str() {
                    //"0282d111-5222-4675-80d7-69156904bf03" => CircType::Star8,
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
                panic!("Unknown arg: {}", unknown_arg);
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
