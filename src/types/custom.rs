use crate::sls::{Circuit, NodeType};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::str::FromStr;
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::{Duration, Instant};

use crate::sls;

fn get_component_output(n: &sls::Circuit, component: usize, output: usize) -> &Weak<RefCell<Vec<bool>>> {
    &n.components[component].ic_instance.as_ref().unwrap().components
        [n.components[component].ic_instance.as_ref().unwrap().outputs[output]]
        .inputs[0]
        .other_output
        .weak
}

pub fn run(n: &mut sls::Circuit, stdin_channel: Receiver<String>) {
    let mut paused: bool = false;
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
        }

        match stdin_channel.try_recv() {
            Ok(key) => {
                //println!("Received: {}", key);
                let removed = key.replace("\n", "");
                let mut it = removed.split(' ');
                match it.next() {
                    Some(comm) => {
                        let mut command_chars = comm.chars();
                        match command_chars.next() {
                            None => (),
                            Some(c) => match c {
                                'q' => {
                                    break 'main;
                                }
                                'p' => {
                                    paused = !paused;
                                    println!("{}", if paused { "paused" } else { "unpaused" });
                                }
                                'm' => {
                                    println!("ms per tick: {}",n.get_speed());
                                }
                                't' => {
                                    n.tick();
                                }
                                'h' => {
                                    println!("hewro");
                                }
                                'a' => {
                                    let instance = n.components[7].ic_instance.as_ref().unwrap();
                                    let instance2 = n.components[11].ic_instance.as_ref().unwrap();
                                    println!(
                                        "{:?}",
                                        (instance as *const Circuit as usize)
                                            == instance2 as *const Circuit as usize
                                    );
                                }
                                'd' => {
                                    let instance = n.components[7].ic_instance.as_ref().unwrap();
                                    for i in 0..instance.outputs.len() {
                                        let comp_index = instance.outputs[i];
                                        println!(
                                            "{} {:#?}",i,
                                            instance.components[comp_index].inputs[0]
                                                .other_output
                                                .weak
                                                .ptr_eq(
                                                    get_component_output(n, 11, i)
                                                )
                                        );
                                    }
                                    for i in 0..instance.outputs.len() {
                                        println!(
                                            "7({}) {:#?}",i,
                                            get_component_output(n, 7, i).as_ptr()
                                        );
                                        println!(
                                            "11({}) {:#?}",i,
                                            get_component_output(n, 11, i).as_ptr()
                                        );
                                    }
                                }
                                'o' => {
                                    println!("outputs:");
                                    for i in &n.outputs {
                                        let comp = &n.components[*i];
                                        match comp.node_type {
                                            NodeType::LIGHT_BULB => {
                                                print!("light");
                                                match &comp.label {
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
                                                match &comp.label {
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
                                'i' => {
                                    println!("buttons:");
                                    for i in 0..n.inputs.len() {
                                        let comp = &n.components[n.inputs[i]];
                                        print!("{}:\t{:?}", i, n.components[n.inputs[i]].node_type);
                                        match &comp.label {
                                            Some(label) => {
                                                print!("({})", label);
                                            }
                                            None => {}
                                        }
                                        println!(
                                            " - {:#?}\n",
                                            n.components[n.inputs[i]].outputs.try_borrow().unwrap()
                                                [0]
                                        );
                                    }
                                }
                                'c' => match it.next() {
                                    Some(s) => match usize::from_str(s) {
                                        Ok(num) => {
                                            let comp = &n.components[num];
                                            //check if we have anotehr num
                                            match it.next() {
                                                Some(s) => match usize::from_str(s) {
                                                    Ok(num2) => {
                                                        //index into ic_instance
                                                        match &comp.ic_instance {
                                                            Some(ic) => match ic
                                                                .components
                                                                .get(num2)
                                                            {
                                                                Some(inner_comp) => {
                                                                    println!(
                                                                        "inner:{:#?}",
                                                                        inner_comp
                                                                    );
                                                                }
                                                                None => {
                                                                    for i in 0..ic.components.len()
                                                                    {
                                                                        println!(
                                                                            "{} {:?}({:?})",
                                                                            i,
                                                                            &ic.components[i]
                                                                                .node_type,
                                                                            &ic.components[i].label
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
                                                None => match command_chars.next() {
                                                    None => println!("components:{:#?}\n", comp),
                                                    Some(c) => match c {
                                                        'i' => {
                                                            println!(
                                                                "{:#?}\n{:#?}",
                                                                &comp.input_states, &comp.outputs
                                                            );
                                                        }
                                                        _ => println!("unknown option {} for c", c),
                                                    },
                                                },
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
                                                &n.components[i].label
                                            );
                                        }
                                    }
                                },
                                's' => 's: {
                                    match it.next() {
                                        Some(s) => match usize::from_str(s) {
                                            Ok(num) => {
                                                if n.inputs.len() <= num {
                                                    println!(
                                                        "{} is isn't below {}",
                                                        num,
                                                        n.inputs.len()
                                                    );
                                                    break 's;
                                                }
                                                let comp_index = n.inputs[num];
                                                let comp = &mut n.components[comp_index];
                                                comp.next_outputs[0] = !comp.next_outputs[0];
                                                println!(
                                                    "set {} to {}\n",
                                                    num, comp.next_outputs[0]
                                                );
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
                        }
                    }
                    None => {
                        println!("wot? gimmeh a command");
                    }
                }
            }
            Err(TryRecvError::Empty) => (),
            Err(TryRecvError::Disconnected) => panic!("Channel disconnected"),
        };
    }
}
