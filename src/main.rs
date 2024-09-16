use serde::{Deserialize, Deserializer, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::rc::{Rc, Weak};
use std::str::FromStr;
use std::sync::{
    mpsc,
    mpsc::{Receiver, TryRecvError},
};
use std::time::{Duration, Instant};
use std::{io, thread};

use crate::sls::NodeType;

mod sls;

fn main() {
    println!("Hello, world!");
    let mut args = std::env::args();
    args.next().unwrap();
    let filename = args.next().expect("gimmeh a file.");
    //deserialize in 2 parts:
    //1: use serde
    let f = std::fs::File::open(filename).unwrap();
    let mut n: sls::Circuit = serde_json::from_reader(std::io::BufReader::new(f)).unwrap();
    //connect bits and stuff
    n.init_circ();

    let mut paused: bool = true;
    let stdin_channel = spawn_stdin_channel();
    //timing
    let mut last_tick = Instant::now();
    let target_tps: u64 = 20;
    let target_dur = Duration::from_millis(1000 / target_tps);
    'main: loop {
        //target tps
        let delta = last_tick - Instant::now();
        if delta < target_dur {
            std::thread::sleep(target_dur - delta);
        }
        last_tick = Instant::now();

        if !paused {
            n.tick();
        } else {
        }
        match stdin_channel.try_recv() {
            Ok(key) => {
                //println!("Received: {}", key);
                let removed = key.replace("\n", "");
                let mut it = removed.split(' ');
                match it.next() {
                    Some(comm) => match comm {
                        "q" => {
                            break 'main;
                        }
                        "p" => {
                            paused = !paused;
                            println!("{}", if paused { "paused" } else { "unpaused" });
                        }
                        "t" => {
                            n.tick();
                        }
                        "h" => {
                            println!("hewro");
                        }
                        "o" => {
                            println!("outputs:");
                            for i in &n.outputs {
                                let comp = &n.components[*i];
                                match comp.node_type {
                                    NodeType::LIGHT_BULB => {
                                        print!("light");
                                        match &comp.arguments.label {
                                            Some(label) => {
                                                println!("({})", label);
                                            }
                                            None => {}
                                        }
                                        let b: bool = comp.outputs.try_borrow().unwrap()[0];
                                        println!(":{}\n", b);
                                    }
                                    NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => {
                                        print!("hex");
                                        match &comp.arguments.label {
                                            Some(label) => {
                                                print!("({}) ", label);
                                            }
                                            None => {
                                                print!("Display: ")
                                            }
                                        }
                                        let mut num = 0;
                                        for input in &comp.input_states {
                                            if input.state {
                                                num += 8 >> input.in_pin;
                                            }
                                        }
                                        println!("{:x}", num);
                                    }
                                    _ => {}
                                }
                            }
                        }
                        "i" => {
                            println!("buttons:");
                            for i in 0..n.inputs.len() {
                                let comp = &n.components[n.inputs[i]];
                                print!("{}:\t{:?}", i, n.components[n.inputs[i]].node_type);
                                match &comp.arguments.label {
                                    Some(label) => {
                                        print!("({})", label);
                                    }
                                    None => {}
                                }
                                println!(
                                    " - {:#?}\n",
                                    n.components[n.inputs[i]].outputs.try_borrow().unwrap()[0]
                                );
                            }
                        }
                        "c" => match it.next() {
                            Some(s) => match usize::from_str(s) {
                                Ok(num) => {
                                    let comp = &n.components[num];
                                    //check if we have anotehr num
                                    match it.next() {
                                        Some(s) => match usize::from_str(s) {
                                            Ok(num2) => {
                                                //index into ic_instance
                                                match &comp.ic_instance {
                                                    Some(ic) => match ic.components.get(num2) {
                                                        Some(inner_comp) => {
                                                            println!("inner:{:#?}", inner_comp);
                                                        }
                                                        None => {
                                                            for i in 0..ic.components.len() {
                                                                println!(
                                                                    "{} {:?}({:?})",
                                                                    i,
                                                                    &ic.components[i].node_type,
                                                                    &ic.components[i]
                                                                        .arguments
                                                                        .label
                                                                );
                                                            }
                                                        }
                                                    },
                                                    None => {
                                                        println!("component {} doesn't have an ic instance",num);
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                println!("parsing after c: {}", e);
                                            }
                                        },
                                        None => {
                                            println!("components:{:#?}\n", comp);
                                        }
                                    }
                                }
                                Err(e) => {
                                    println!("parsing after c: {}", e);
                                }
                            },
                            None => {
                                println!("expected switch num after c");
                                for i in 0..n.components.len() {
                                    println!(
                                        "{} {:?}({:?})",
                                        i,
                                        &n.components[i].node_type,
                                        &n.components[i].arguments.label
                                    );
                                }
                            }
                        },
                        "s" => 's: {
                            match it.next() {
                                Some(s) => match usize::from_str(s) {
                                    Ok(num) => {
                                        if n.inputs.len() <= num {
                                            println!("{} is isn't below {}", num, n.inputs.len());
                                            break 's;
                                        }
                                        let comp_index = n.inputs[num];
                                        let comp = &mut n.components[comp_index];
                                        comp.next_outputs[0] = !comp.next_outputs[0];
                                        println!("set {} to {}\n", num, comp.next_outputs[0]);
                                    }
                                    Err(e) => {
                                        println!("parsing after s: {}", e);
                                    }
                                },
                                None => {
                                    println!("expected switch num after s");
                                }
                            };
                        }
                        _ => {
                            println!("wot?");
                        }
                    },
                    None => {
                        println!("wot? gimmeh a command");
                    }
                }
            }
            Err(TryRecvError::Empty) => (),
            Err(TryRecvError::Disconnected) => panic!("Channel disconnected"),
        };
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
