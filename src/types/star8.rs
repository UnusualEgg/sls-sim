use clap::{Parser, Subcommand};
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::{Duration, Instant};

use crate::sls::{self, Circuit, Node};

#[derive(Parser)]
struct StarArgs {
    #[command(subcommand)]
    commands: SubCommands,
}
#[derive(Subcommand)]
enum SubCommands {
    P, //programming mode
    T, //tick
    C, //togle clock
}

struct Buttons {
    //0x81 input
    submit: usize,
    input_number: usize, //8 bits

    //programming
    prog_toggle: usize,
    write: usize,
    next: usize,
    load: usize,      //addresss
    clear: usize,     //instruction
    prog_addr: usize, //8 bits
    //instruction
    inc: usize,
    add: usize,
    sub: usize,
    lia: usize, //imm
    stm: usize,
    jmp: usize,
    jmp_if: usize,
    halt: usize,
    lda: usize, //addr
    data: usize,
    //accum
    a: usize,
    b: usize,
    c: usize,
    d: usize,
    //condition
    a_zero: usize,
    a_eq_b: usize,
    zero: usize,
    overflow: usize,
    negative: usize,

    //clock
    clock_toogle: usize,
    clock: usize,
    //accum
    clear_accum: usize,
    //pc
    reset_pc: usize,
}
impl Default for Buttons {
    fn default() -> Self {
        Self {
            submit: 1,
            input_number: 2,
            prog_toggle: 10,
            write: 38,
            next: 39,
            load: 40,
            clear: 41,
            inc: 11,
            add: 12,
            sub: 13,
            lia: 14,
            stm: 15,
            jmp: 16,
            jmp_if: 17,
            halt: 18,
            lda: 19,
            data: 20,
            a: 21,
            b: 22,
            c: 23,
            d: 24,
            a_zero: 25,
            a_eq_b: 26,
            zero: 27,
            overflow: 28,
            negative: 29,
            prog_addr: 30,
            clock_toogle: 43,
            clock: 44,
            clear_accum: 42,
            reset_pc: 0,
        }
    }
}
impl Buttons {
    fn all_prog_buttons(&self) -> [usize; 24] {
        [
            //instruction
            self.clear,
            self.prog_addr,
            self.inc,
            self.add,
            self.sub,
            self.write,
            self.next,
            self.load, //addresss
            //imm
            self.lia,
            self.stm,
            self.jmp,
            self.jmp_if,
            self.halt,
            //addr
            self.lda,
            self.data,
            //accum
            self.a,
            self.b,
            self.c,
            self.d,
            //condition
            self.a_zero,
            self.a_eq_b,
            self.zero,
            self.overflow,
            self.negative,
        ]
    }
}
struct Outputs {
    read_input: usize,
    expect_ins: usize,
    expect_acc: usize,
    expect_cond: usize,
    expect_addr: usize,
    expect_value: usize,
    expect_write: usize,
    prog_addr: usize, //hi then lo nibbles
    regs: usize,      //4 regs with 2 nibbles per
}
impl Default for Outputs {
    fn default() -> Self {
        Self {
            read_input: 0,
            expect_ins: 1,
            expect_acc: 2,
            expect_cond: 3,
            expect_addr: 4,
            expect_value: 5,
            expect_write: 7,
            prog_addr: 14,
            regs: 16,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ExpectLight {
    Ins,
    Acc,
    Cond,
    Addr,
    Value,
    Write,
}
fn is_on(c: &Node) -> bool {
    c.outputs.borrow()[0]
}
fn get_expected(n: &Circuit, outputs: &Outputs) -> Option<ExpectLight> {
    let l = &n.components;
    //TODO actually index inputs
    if is_on(&l[n.outputs[outputs.expect_ins]]) {
        Some(ExpectLight::Ins)
    } else if is_on(&l[n.outputs[outputs.expect_acc]]) {
        Some(ExpectLight::Acc)
    } else if is_on(&l[n.outputs[outputs.expect_cond]]) {
        Some(ExpectLight::Cond)
    } else if is_on(&l[n.outputs[outputs.expect_addr]]) {
        Some(ExpectLight::Addr)
    } else if is_on(&l[n.outputs[outputs.expect_value]]) {
        Some(ExpectLight::Value)
    } else if is_on(&l[n.outputs[outputs.expect_write]]) {
        Some(ExpectLight::Write)
    } else {
        None
    }
}
#[derive(Clone, Copy)]
enum ProgState {
    addr,
    ins,
}
#[derive(Clone, Copy)]
enum State {
    run,
    prog(ProgState),
}
fn state_run<'arg, IT>(
    args: &mut IT,
    n: &mut Circuit,
    outputs: &Outputs,
    buttons: &Buttons,
    stdin_channel: &Receiver<String>,
    paused: &mut bool,
    state: &mut State,
) -> bool
where
    IT: Iterator<Item = &'arg str>,
{
    if let Some(x) = args.next() {
        match x {
            "p" => {
                //TODO todo!("programming mode");
                let prog = &mut n.components[n.inputs[buttons.prog_toggle]];
                prog.next_outputs[0] = true;
                *state = State::prog(ProgState::addr);
            }
            "t" => {
                n.tick();
            }
            "c" => {
                *paused = !*paused;
            }
            "r" => {
                println!("{}", outputs.regs);
            }
            "q" => return true,
            cmd => eprintln!("Unknown command \"{cmd}\"",),
        }
    }
    false
}
fn state_prog_addr(
    x: &str,
    n: &mut Circuit,
    outputs: &Outputs,
    buttons: &Buttons,
    state: &mut State,
) {
    match x {
        "n" => {
            n.components[n.inputs[buttons.next]].next_outputs[0] = true;
            let old = n.components[n.outputs[outputs.prog_addr]]
                .next_outputs
                .clone();
            let old2 = n.components[n.outputs[outputs.prog_addr + 1]]
                .next_outputs
                .clone();
            loop {
                n.tick();
                let new = &n.components[n.outputs[outputs.prog_addr]].next_outputs;
                let new2 = &n.components[n.outputs[outputs.prog_addr + 1]].next_outputs;
                if &old != new || &old2 != new2 {
                    break;
                }
            }
            n.components[n.inputs[buttons.next]].next_outputs[0] = false;
        }
        //TODO eventually add load
        "i" => {
            *state = State::prog(ProgState::addr);
        }
        cmd => eprintln!("Unknown command \"{cmd}\"",),
    }
}
fn state_prog(
    x: &str,
    n: &mut Circuit,
    outputs: &Outputs,
    buttons: &Buttons,
    stdin_channel: &Receiver<String>,
    state: &mut State,
    prog_state: ProgState,
) {
    //now do prog stuff
    let mut last = None;
    loop {
        //simulate till expected
        let expected = get_expected(n, outputs);
        if expected == last {
            n.tick();
            continue;
        }
        for i in buttons.all_prog_buttons() {
            n.components[n.inputs[i]].next_outputs[0] = false;
        }
        match expected {
            None => n.tick(),
            Some(l) => {
                println!("{:?}", l);
                match l {
                    ExpectLight::Ins => {
                        //check which button pressed
                        loop {
                            match stdin_channel.try_recv() {
                                Ok(btn) => match btn.as_str() {
                                    "inc" => {
                                        n.components[n.inputs[buttons.inc]].next_outputs[0] = true;
                                        break;
                                    }
                                    "add" => {
                                        n.components[n.inputs[buttons.add]].next_outputs[0] = true;
                                        break;
                                    }
                                    "sub" => {
                                        n.components[n.inputs[buttons.sub]].next_outputs[0] = true;
                                        break;
                                    }
                                    "lia" => {
                                        n.components[n.inputs[buttons.lia]].next_outputs[0] = true;
                                        break;
                                    }
                                    "stm" => {
                                        n.components[n.inputs[buttons.stm]].next_outputs[0] = true;
                                        break;
                                    }
                                    "jmp" => {
                                        n.components[n.inputs[buttons.jmp]].next_outputs[0] = true;
                                        break;
                                    }
                                    "jif" => {
                                        n.components[n.inputs[buttons.jmp_if]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    "hlt" => {
                                        n.components[n.inputs[buttons.halt]].next_outputs[0] = true;
                                        break;
                                    }
                                    "lda" => {
                                        n.components[n.inputs[buttons.lda]].next_outputs[0] = true;
                                        break;
                                    }
                                    "db" => {
                                        n.components[n.inputs[buttons.data]].next_outputs[0] = true;
                                        break;
                                    }
                                    s => {
                                        eprintln!("shrug {}", s);
                                    }
                                },
                                Err(TryRecvError::Empty) => (),
                                Err(TryRecvError::Disconnected) => {
                                    panic!("Channel disconnected")
                                }
                            }
                        }
                    }
                    ExpectLight::Acc => {
                        //check which button pressed
                        loop {
                            match stdin_channel.try_recv() {
                                Ok(btn) => match btn.as_str() {
                                    "a" => {
                                        n.components[n.inputs[buttons.a]].next_outputs[0] = true;
                                        break;
                                    }
                                    "b" => {
                                        n.components[n.inputs[buttons.b]].next_outputs[0] = true;
                                        break;
                                    }
                                    "c" => {
                                        n.components[n.inputs[buttons.c]].next_outputs[0] = true;
                                        break;
                                    }
                                    "d" => {
                                        n.components[n.inputs[buttons.d]].next_outputs[0] = true;
                                        break;
                                    }
                                    s => {
                                        eprintln!("shrug {}", s);
                                    }
                                },
                                Err(TryRecvError::Empty) => (),
                                Err(TryRecvError::Disconnected) => {
                                    panic!("Channel disconnected")
                                }
                            }
                        }
                    }
                    ExpectLight::Cond => {
                        //check which button pressed
                        loop {
                            match stdin_channel.try_recv() {
                                Ok(btn) => match btn.as_str() {
                                    "aez" => {
                                        n.components[n.inputs[buttons.a_zero]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    "aeb" => {
                                        n.components[n.inputs[buttons.a_eq_b]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    "zro" => {
                                        n.components[n.inputs[buttons.zero]].next_outputs[0] = true;
                                        break;
                                    }
                                    "ovr" => {
                                        n.components[n.inputs[buttons.overflow]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    "neg" => {
                                        n.components[n.inputs[buttons.negative]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    s => {
                                        eprintln!("shrug {}", s);
                                    }
                                },
                                Err(TryRecvError::Empty) => (),
                                Err(TryRecvError::Disconnected) => {
                                    panic!("Channel disconnected")
                                }
                            }
                        }
                    }
                    ExpectLight::Addr | ExpectLight::Value => {
                        //check which button pressed
                        loop {
                            match stdin_channel.try_recv() {
                                Ok(btn) => match u8::from_str_radix(&btn, 16) {
                                    Ok(v) => {
                                        for i in 0..8 {
                                            let b_index = buttons.prog_addr + i;
                                            let bit = (v >> i) != 0;
                                            n.components[n.inputs[b_index]].next_outputs[0] = bit;
                                        }
                                        n.components[n.inputs[buttons.write]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    Err(e) => eprintln!("Invalid number {}", e),
                                },
                                Err(TryRecvError::Empty) => (),
                                Err(TryRecvError::Disconnected) => {
                                    panic!("Channel disconnected")
                                }
                            }
                        }
                    }
                    ExpectLight::Write => {
                        //check which button pressed
                        //TODO tell which one dis is (expect light)
                        //TODO print possible butons to press
                        println!("expect write");
                        loop {
                            match stdin_channel.try_recv() {
                                Ok(btn) => match btn.as_str() {
                                    "w" => {
                                        n.components[n.inputs[buttons.write]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    "c" => {
                                        n.components[n.inputs[buttons.clear]].next_outputs[0] =
                                            true;
                                        break;
                                    }
                                    s => {
                                        eprintln!("shrug {}", s);
                                    }
                                },
                                Err(TryRecvError::Empty) => (),
                                Err(TryRecvError::Disconnected) => {
                                    panic!("Channel disconnected")
                                }
                            }
                        }
                    }
                };
            }
        }
        last = expected;
    }
}
pub fn run(n: &mut sls::Circuit, stdin_channel: Receiver<String>) {
    for comp in &n.components {
        println!("{:?} {}", comp.label, comp.y)
    }
    //find buttons
    let buttons = Buttons::default();
    let outputs = Outputs::default();

    //setup for loop
    let mut state = State::run;
    let mut paused: bool = true;
    //timing
    let mut last_tick = Instant::now();
    let target_tps: u64 = 20;
    let target_dur = Duration::from_millis(1000 / target_tps);
    'main: loop {
        match state {
            State::run => {
                if !paused {
                    //target tps
                    let delta = last_tick - Instant::now();
                    if delta < target_dur {
                        std::thread::sleep(target_dur - delta);
                    }
                    last_tick = Instant::now();
                    n.tick();
                }
                match stdin_channel.try_recv() {
                    Ok(cmd) => {
                        let mut x = cmd.split_whitespace();
                        if state_run(
                            &mut x,
                            n,
                            &outputs,
                            &buttons,
                            &stdin_channel,
                            &mut paused,
                            &mut state,
                        ) {
                            break 'main;
                        }
                    }

                    Err(TryRecvError::Empty) => (),
                    Err(TryRecvError::Disconnected) => panic!("Channel disconnected"),
                }
            }
            State::prog(prog_state) => match stdin_channel.try_recv() {
                Ok(cmd) => {
                    state_prog(
                        cmd.trim(),
                        n,
                        &outputs,
                        &buttons,
                        &stdin_channel,
                        &mut state,
                        prog_state,
                    );
                }

                Err(TryRecvError::Empty) => (),
                Err(TryRecvError::Disconnected) => panic!("Channel disconnected"),
            },
        }
        //TODO maybe export and read RAM from instance
        //FIXME why can't match ToT
    }
}
