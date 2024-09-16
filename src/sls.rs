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

#[allow(non_camel_case_types)]
#[derive(Deserialize, Serialize, Debug, PartialEq, Clone)]
pub enum NodeType {
    PULSE_BUTTON,
    TOGGLE_BUTTON,
    LIGHT_BULB,
    NOTE,
    D_FLIP_FLOP,
    SR_FLIP_FLOP,
    JK_FLIP_FLOP,
    T_FLIP_FLOP,
    SR_LATCH,
    MUX,
    DEMUX,
    FULL_ADDER,
    HALF_ADDER,
    AND_GATE,
    BUFFER_GATE,
    NAND_GATE,
    NOR_GATE,
    NOT_GATE,
    OR_GATE,
    XOR_GATE,
    XNOR_GATE,
    CLOCK,
    HIGH_CONSTANT,
    LOW_CONSTANT,
    FLASHLIGHT,
    RGB_LIGHT,
    SEVEN_SEGMENT_DISPLAY,
    DOT_MATRIX_DISPLAY_5X7,
    SPEAKER,
    VIBRATION,
    CHARGER_PLUGGED_SENSOR,
    BATTERY_LEVEL,
    LIGHT_SENSOR,
    MAGNETIC_FIELD_SENSOR,
    PROXIMITY_SENSOR,
    NOTIFICATION_LED,
    SEVEN_SEGMENT_DISPLAY_DECODER,
    INTEGRATED_CIRCUIT,
}
fn none() -> Weak<RefCell<Vec<bool>>> {
    return Weak::new();
}

#[derive(PartialEq, Clone, Debug, Eq, Hash, Default)]
struct ID(String);
struct IDVisitor;
use serde::de::{self, Error, Visitor};
impl<'de> Visitor<'de> for IDVisitor {
    type Value = ID;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("u32 or string")
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID((value).to_string()))
    }
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID((value).to_string()))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID(value.to_string()))
    }
    fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(ID(value))
    }
}
impl<'de> Deserialize<'de> for ID {
    fn deserialize<D>(deserializer: D) -> Result<ID, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(IDVisitor)
    }
}
#[derive(Deserialize, Debug, Clone, Default)]
pub struct InputState {
    pub state: bool,
    pub in_pin: usize,
}
#[derive(Deserialize, Clone)]
struct Input {
    #[serde(skip, default = "none")]
    other_output: Weak<RefCell<Vec<bool>>>,
    #[serde(rename = "OTHER_CONNECTOR_ID")]
    other_pin: usize,
    #[serde(rename = "OTHER_COMPONENT")]
    other_id: ID,
    #[serde(rename = "CONNECTOR_ID")]
    in_pin: usize,
}
impl Debug for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let o;
        f.debug_struct("Input")
            .field(
                "other_output",
                match self.other_output.upgrade() {
                    Some(x) => {
                        o = (&x).try_borrow().unwrap().clone();
                        if self.other_pin >= o.len() {
                            &"Pin more than input"
                        } else {
                            &o[self.other_pin]
                        }
                    }
                    None => &"Disconnected",
                },
            )
            .field("other_pin", &self.other_pin)
            .field("other_id", &self.other_id)
            .field("in_pin", &self.in_pin)
            .finish()
    }
}

#[derive(Deserialize, Debug, Clone, Default)]
#[serde(rename_all = "UPPERCASE")]
pub struct Arguments {
    pub label: Option<String>,
    enabled: Option<bool>,
    uri: Option<String>,
    cid: Option<String>,
    num_of_in: Option<usize>,
    num_of_out: Option<usize>,
}

fn default_outputs() -> Rc<RefCell<Vec<bool>>> {
    return Rc::new(RefCell::new(Vec::new()));
}
#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub struct Node {
    #[serde(rename = "TAG")]
    pub node_type: NodeType,
    inputs: Vec<Input>,
    #[serde(skip)]
    pub input_states: Vec<InputState>,
    #[serde(skip, default = "default_outputs")]
    pub outputs: Rc<RefCell<Vec<bool>>>,
    #[serde(skip)]
    pub next_outputs: Vec<bool>,
    #[serde(skip, default)]
    flip_flop: bool,
    id: ID,
    x: f32,
    y: f32,
    #[serde(default)]
    pub arguments: Arguments,
    #[serde(skip)]
    pub ic_instance: Option<IC>,
    #[serde(default)]
    pub period: u64,
    #[serde(skip)]
    pub last_cycle: Option<Instant>,
}

fn get_input(input: &Input, default: bool) -> bool {
    match input.other_output.upgrade() {
        Some(x) => {
            let o = (&x).try_borrow().unwrap();
            if input.other_pin >= o.len() {
                println!("{:#?}[{}]", o, input.other_pin);
            }
            o[input.other_pin]
        }
        None => default,
    }
}
impl Node {
    fn ic_add_deps(
        &self,
        dependencies: &HashMap<String, IC>,
        needed: &mut HashMap<String, String>,
        uris: &HashMap<String, String>,
    ) {
        let cid = self.arguments.cid.as_ref().expect("cid");
        let uri = match self.arguments.uri.as_ref() {
            Some(uri) => uri,
            //check if it's in URIs.json
            None => match uris.get(cid) {
                Some(uri) => uri,
                None => {
                    panic!(
                        "Couldn't find dep(cid: {}) and no URI was provided\n {:#?}",
                        cid, self
                    );
                }
            },
        };
        if !dependencies.contains_key(cid) {
            needed.insert(cid.clone(), uri.clone());
            println!("found {}", uri);
        }
    }
    fn init_output(&mut self) {
        let output_n = match self.node_type {
            NodeType::INTEGRATED_CIRCUIT => {
                let num_of_out = self.arguments.num_of_out.expect("num_of_out");
                println!(
                    "{} outputs for {}",
                    num_of_out,
                    self.arguments
                        .label
                        .as_ref()
                        .get_or_insert(&"IC".to_string())
                );
                num_of_out
            }
            NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => 7,
            NodeType::SEVEN_SEGMENT_DISPLAY => 0,
            //Q ~Q
            NodeType::D_FLIP_FLOP
            | NodeType::SR_LATCH
            | NodeType::JK_FLIP_FLOP
            | NodeType::SR_FLIP_FLOP
            | NodeType::T_FLIP_FLOP => 2,
            _ => 1,
        };
        let mut outputs = self.outputs.borrow_mut();
        outputs.resize(output_n, false);
        self.next_outputs.resize(output_n, false);
        match self.node_type {
            NodeType::D_FLIP_FLOP | NodeType::SR_LATCH => {
                outputs[0] = false;
                outputs[1] = true;
                self.next_outputs[0] = false;
                self.next_outputs[1] = true;
            }
            NodeType::HIGH_CONSTANT => {
                outputs[0] = true;
                self.next_outputs[0] = true;
            }
            NodeType::LOW_CONSTANT => {
                outputs[0] = false;
                self.next_outputs[0] = false;
            }
            _ => {}
        }
        //also input_states
        self.input_states
            .resize(self.inputs.len(), InputState::default());
        //println!("outputs:{:#?} for {:?}", outputs, self.node_type);
    }
    fn get_inputs(&mut self) {
        let default = match &self.node_type {
            NodeType::AND_GATE | NodeType::NOR_GATE => true,
            _ => false,
        };
        for (i, input) in self.inputs.iter().enumerate() {
            self.input_states[i] = InputState {
                in_pin: input.in_pin,
                state: get_input(input, default),
            }
        }
    }
    fn next_output(&mut self, tick: u64) {
        match &self.node_type {
            NodeType::LIGHT_BULB => {
                for input in &self.input_states {
                    self.next_outputs[0] = input.state;
                }
            }
            NodeType::AND_GATE => {
                let mut out = true;
                for input in &self.input_states {
                    out &= input.state;
                }
                if self.input_states.len() == 0 {
                    out = false;
                }
                self.next_outputs[0] = out;
            }
            NodeType::OR_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out |= input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::XOR_GATE => {
                let mut out = true;
                for input in &self.input_states {
                    out ^= input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::NOR_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out |= input.state;
                }
                self.next_outputs[0] = !out;
            }
            NodeType::NOT_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out = !input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::BUFFER_GATE => {
                let mut out = false;
                for input in &self.input_states {
                    out = input.state;
                }
                self.next_outputs[0] = out;
            }
            NodeType::CLOCK => {
                let now = Instant::now();
                match self.last_cycle {
                    Some(last_cycle) => {
                        if now.duration_since(last_cycle).as_millis() as u64 > (self.period / 2) {
                            self.next_outputs[0] = !self.next_outputs[0];
                            self.last_cycle = Some(now);
                        }
                    }
                    None => {
                        self.last_cycle = Some(now);
                    }
                }
            }
            NodeType::INTEGRATED_CIRCUIT => {
                let instance = &mut self.ic_instance.as_mut().unwrap();
                for comp in &mut instance.components {
                    comp.get_inputs();
                }
                //set/override instance's inputs
                for input in &self.input_states {
                    /*
                    println!(
                        "getting {:?} for {}",
                        input,
                        self.arguments.label.as_ref().unwrap_or(&"IC".to_string())
                    );
                    */
                    let out: bool = input.state;
                    let comp_index: usize = instance.inputs[input.in_pin];
                    instance.components[comp_index].next_outputs[0] = out;
                    /*
                    println!(
                        "next_output {:?}\tset to {} from {}",
                        instance.components[comp_index].node_type, out, self.y
                    );
                    */
                }
                for comp in &mut instance.components {
                    comp.next_output(tick);
                }
                //set next out based on inner IC
                //println!(
                //    "we have {} inner componenents. and {} out pins",
                //    instance.components.len(),
                //    self.next_outputs.len()
                //);
                for i in 0..instance.outputs.len() {
                    //println!("get comp {}", instance.outputs[i]);
                    let comp_index: usize = instance.outputs[i];
                    self.next_outputs[i] = instance.components[comp_index].next_outputs[0];
                }
            }
            NodeType::SR_LATCH => {
                let mut set = false;
                let mut reset = false;
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            set = state;
                        }
                        1 => {
                            reset = state;
                        }
                        n => {
                            panic!("tried to acess input {} of {:?}", n, self.node_type);
                        }
                    }
                }
                match (set, reset) {
                    (true, false) => {
                        self.next_outputs[0] = true;
                        self.next_outputs[1] = false;
                    }
                    (false, true) => {
                        self.next_outputs[0] = false;
                        self.next_outputs[1] = true;
                    }
                    (true, true) => {
                        self.next_outputs[0] = false;
                        self.next_outputs[1] = false;
                    }
                    (false, false) => {}
                }
            }
            NodeType::T_FLIP_FLOP => {
                let mut set: bool = false;
                let mut reset: bool = false;
                let mut data: bool = false;
                let mut clock: bool = false;
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            set = state;
                        }
                        1 => {
                            reset = state;
                        }
                        2 => {
                            data = state;
                        }
                        3 => {
                            clock = state;
                        }
                        n => {
                            panic!("tried to acess input {} of D_FLIP_FLOP", n);
                        }
                    }
                }
                (self.next_outputs[0], self.next_outputs[1]) = match (set, reset) {
                    (false, true) => (true, false),
                    (true, false) => (false, true),
                    //reset (technically should be false false)
                    //but we emulate sls and say false,true
                    (false, false) => (false, true),
                    (true, true) => {
                        //rising edge
                        if clock && !self.flip_flop && data {
                            (self.next_outputs[1], self.next_outputs[0])
                        } else {
                            (self.next_outputs[0], self.next_outputs[1])
                        }
                    }
                };
                self.flip_flop = clock; //past clock
            }
            NodeType::D_FLIP_FLOP => {
                let mut data: bool = false;
                let mut clock: bool = false;
                let mut set: bool = false;
                let mut reset: bool = false;
                for input in &self.input_states {
                    let state = input.state;
                    match input.in_pin {
                        0 => {
                            set = state;
                        }
                        1 => {
                            reset = state;
                        }
                        2 => {
                            data = state;
                        }
                        3 => {
                            clock = state;
                        }
                        n => {
                            panic!("tried to acess input {} of D_FLIP_FLOP", n);
                        }
                    }
                }
                (self.next_outputs[0], self.next_outputs[1]) = match (set, reset) {
                    (false, true) => (true, false),
                    (true, false) => (false, true),
                    //reset (technically should be false false)
                    //but we emulate sls and say false,true
                    (false, false) => (false, true),
                    (true, true) => {
                        //rising edge
                        if clock && !self.flip_flop {
                            (data, !data)
                        } else {
                            (self.next_outputs[0], self.next_outputs[1])
                        }
                    }
                };
                self.flip_flop = clock; //past clock
            }
            NodeType::SEVEN_SEGMENT_DISPLAY_DECODER => {
                //not gonna actually simulate, just gonna show input number
            }
            NodeType::SEVEN_SEGMENT_DISPLAY => {}
            NodeType::NOTE => {}
            NodeType::PULSE_BUTTON | NodeType::TOGGLE_BUTTON => {}
            NodeType::HIGH_CONSTANT | NodeType::LOW_CONSTANT => {}
            _ => {
                todo!("{:?}", &self.node_type);
            }
        }
    }
    fn update_output(&mut self) {
        self.outputs.borrow_mut().clone_from(&self.next_outputs);
        if let Some(instance) = &mut self.ic_instance.as_mut() {
            for comp in &mut instance.components {
                comp.update_output();
                /*
                if comp.node_type == NodeType::PULSE_BUTTON
                    || comp.node_type == NodeType::TOGGLE_BUTTON
                {
                    println!(
                        "update {:?}\t{:?} < {:?} from {}",
                        comp.node_type,
                        comp.outputs.try_borrow().unwrap(),
                        comp.next_outputs,
                        self.y
                    );
                }
                // */
            }
        }
    }
}

#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "UPPERCASE")]
enum CircuitType {
    Project,
    #[default]
    Ic,
}
#[derive(Deserialize, Default, Debug, Clone)]
#[serde(rename_all = "UPPERCASE")]
struct ICHeader {
    app_version: usize,
    name: String,
    #[serde(default)]
    color: String,
    #[serde(skip, default)]
    id: String,
}

#[derive(Deserialize, Debug, Clone)]
struct Wire {
    //TODO make this "ID:pin"
    #[serde(rename = "S")]
    to: WireID,
    #[serde(rename = "E")]
    from: WireID,
}
#[derive(PartialEq, Clone, Debug, Eq, Hash, Default)]
struct WireID(ID, usize);

impl<'de> Deserialize<'de> for WireID {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_string(WireIDVisitor)
    }
}
#[derive(Debug)]
enum WireError {
    Message(String),
}
impl std::error::Error for WireError {}
impl std::fmt::Display for WireError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WireError::Message(m) => write!(f, "WireID \"{}\"", m),
        }
    }
}
impl de::Error for WireError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        print!("WireError: {}", msg);
        WireError::Message("Error".to_string())
    }
}

struct WireIDVisitor;

impl<'de> Visitor<'de> for WireIDVisitor {
    type Value = WireID;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("string")
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        let mut iter = v.split(':');
        let id;
        let pin;
        match iter.next() {
            Some(id_str) => {
                id = ID(id_str.to_string());
            }
            None => {
                return Err(de::Error::custom(format!("Expected an ID but got {}", v)));
            }
        }
        match iter.next() {
            Some(pin_str) => {
                pin = match usize::from_str(pin_str) {
                    Err(e) => {
                        return Err(de::Error::custom(format!(
                            "Expected a pin number after ID but got {} ({})",
                            pin_str, e
                        )));
                    }
                    Ok(pin_n) => pin_n,
                };
            }
            None => {
                return Err(de::Error::custom(format!(
                    "Expected a pin number after ID but got {}",
                    v
                )));
            }
        }
        Ok(WireID(id, pin))
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub struct IC {
    header: ICHeader,
    pub components: Vec<Node>,
    //indicies of inputs
    #[serde(skip, default)]
    pub inputs: Vec<usize>,
    #[serde(skip, default)]
    pub outputs: Vec<usize>,
    #[serde(default)]
    wires: Vec<Wire>,
}
impl IC {
    //get input indecies
    fn init_ic(&mut self, ics: &mut HashMap<String, IC>) {
        let mut ids = HashMap::new();
        for comp in &self.components {
            ids.insert(comp.id.clone(), Rc::downgrade(&comp.outputs));
        }
        for comp in &mut self.components {
            for input in &mut comp.inputs {
                //find other comp
                input.other_output = ids.get(&input.other_id).unwrap().clone();
            }
            comp.init_output();
        }
        //for new fromat with wires
        for wire in &self.wires {
            self.components
                .iter_mut()
                .find(|comp| comp.id == wire.to.0)
                .unwrap()
                .inputs
                .push(Input {
                    other_output: ids.get(&wire.from.0).unwrap().clone(),
                    other_pin: wire.from.1,
                    other_id: wire.from.0.clone(),
                    in_pin: wire.to.1,
                })
        }
        for i in 0..self.components.len() {
            let comp = &self.components[i];
            //init input indecies Vec
            if comp.node_type == NodeType::PULSE_BUTTON || comp.node_type == NodeType::TOGGLE_BUTTON
            {
                self.inputs.push(i);
            } else if comp.node_type == NodeType::LIGHT_BULB {
                self.outputs.push(i);
            }
            //set instance for IC
            else if comp.node_type == NodeType::INTEGRATED_CIRCUIT {
                set_instance(&mut self.components[i], ics);
            }
        }
        self.inputs.sort_by(|comp1, comp2| {
            self.components[*comp1]
                .y
                .partial_cmp(&self.components[*comp2].y)
                .unwrap()
        });
        self.outputs.sort_by(|comp1, comp2| {
            self.components[*comp1]
                .y
                .partial_cmp(&self.components[*comp2].y)
                .unwrap()
        });
    }
}
#[derive(Deserialize, Default, Debug)]
#[serde(rename_all = "UPPERCASE")]
struct Header {
    app_version: usize,
    #[serde(default)]
    id: ID,
    #[serde(rename = "TYPE", default)]
    circ_type: CircuitType,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "UPPERCASE")]
pub struct Circuit {
    #[serde(skip)]
    uris: HashMap<String, String>,
    header: Header,
    pub components: Vec<Node>,
    #[serde(default)]
    dependencies: HashMap<String, IC>,
    //indicies of inputs
    #[serde(skip)]
    pub inputs: Vec<usize>,
    #[serde(skip)]
    pub outputs: Vec<usize>,
    #[serde(skip)]
    tick_count: u64,
    #[serde(default)]
    wires: Vec<Wire>,
}
fn add_dep<'hm>(dependencies: &'hm mut HashMap<String, IC>, cid: &String, uri: &String) -> &'hm IC {
    //let result: Option<&IC> = self.dependencies.get(cid);
    dependencies.entry(cid.clone()).or_insert_with(|| {
        //open URI
        match File::open(uri) {
            Err(e) => {
                panic!("Couldn't find URI:\"{}\"| {}", uri, e);
            }

            Ok(f) => {
                let c: IC = serde_json::from_reader(std::io::BufReader::new(f)).unwrap();
                println!("adding {}({})", uri, cid);
                c
            }
        }
    })
}
//of IC
fn set_instance(comp: &mut Node, dependencies: &mut HashMap<String, IC>) {
    //let comp = self.components[i].clone();
    let cid = comp.arguments.cid.as_ref().expect("cid");
    let mut uri_result = comp.arguments.uri.as_ref();
    let empty = "".to_string();
    let uri = uri_result.get_or_insert(&empty);
    let original = add_dep(dependencies, &cid, &uri);
    let mut new = IC {
        header: original.header.clone(),
        components: Vec::with_capacity(original.components.len()),
        inputs: Vec::with_capacity(comp.arguments.num_of_in.expect("num_of_in")),
        outputs: Vec::with_capacity(comp.arguments.num_of_out.expect("num_of_out")),
        wires: original.wires.clone(),
    };
    //individualy clone components
    for comp in &original.components {
        let mut new_comp = comp.clone();
        new_comp.outputs = Rc::new(RefCell::new((*comp.outputs.try_borrow().unwrap()).clone()));
        new.components.push(new_comp);
    }
    new.init_ic(dependencies);
    comp.ic_instance = Some(new);
}
impl Circuit {
    fn add_deps(&mut self) {
        //URIs
        if let Ok(s) = std::fs::read_to_string("URIs.json") {
            self.uris = serde_json::from_str(&s).unwrap();
            println!("added URIs.json: {:?}", &self.uris);
        }
        println!("deps:{:#?}", &self.dependencies);
        println!("add sub deps");
        let mut needed: HashMap<String, String> = HashMap::new();
        let mut added = 0;
        let mut first = true;
        while added != 0 || first {
            first = false;
            for dep in self.dependencies.values() {
                println!("dep:{}", &dep.header.name);
                dep.components
                    .iter()
                    .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
                    .for_each(|comp| comp.ic_add_deps(&self.dependencies, &mut needed, &self.uris));
            }
            self.components
                .iter()
                .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
                .for_each(|comp| comp.ic_add_deps(&self.dependencies, &mut needed, &self.uris));
            for (cid, uri) in needed.iter() {
                println!("adding {}({})", uri, cid);
                add_dep(&mut self.dependencies, &cid, &uri);
            }
            added = needed.len();
        }
    }
    fn connect(&mut self) {
        let mut ids = HashMap::new();
        for comp in &self.components {
            ids.insert(comp.id.clone(), Rc::downgrade(&comp.outputs));
        }
        for comp in &mut self.components {
            for input in &mut comp.inputs {
                //find other comp
                input.other_output = ids.get(&input.other_id).unwrap().clone();
            }
        }
        //for new fromat with wires
        for wire in &self.wires {
            self.components
                .iter_mut()
                .find(|comp| comp.id == wire.to.0)
                .unwrap()
                .inputs
                .push(Input {
                    other_output: ids.get(&wire.from.0).unwrap().clone(),
                    other_pin: wire.from.1,
                    other_id: wire.from.0.clone(),
                    in_pin: wire.to.1,
                })
        }
    }
    pub fn init_circ(&mut self) {
        //2: set outputs length by type
        for comp in &mut self.components {
            comp.init_output();
        }
        //add depependencies
        self.add_deps();
        for i in 0..self.components.len() {
            let node_type = self.components[i].node_type.clone();
            if node_type == NodeType::PULSE_BUTTON || node_type == NodeType::TOGGLE_BUTTON {
                self.inputs.push(i);
            } else if node_type == NodeType::LIGHT_BULB
                || node_type == NodeType::SEVEN_SEGMENT_DISPLAY_DECODER
            {
                self.outputs.push(i);
            }
            //also set instances
            else if node_type == NodeType::INTEGRATED_CIRCUIT {
                set_instance(&mut self.components[i], &mut self.dependencies);
            }
        }
        self.inputs.sort_by(|comp1, comp2| {
            self.components[*comp1]
                .y
                .partial_cmp(&self.components[*comp2].y)
                .unwrap()
        });
        self.outputs.sort_by(|comp1, comp2| {
            self.components[*comp1]
                .y
                .partial_cmp(&self.components[*comp2].y)
                .unwrap()
        });
        //coonnect components
        self.connect();
    }
    pub fn tick(&mut self) {
        for i in 0..self.components.len() {
            self.components[i].get_inputs();
            self.components[i].next_output(self.tick_count);
        }
        for i in 0..self.components.len() {
            self.components[i].update_output();
        }
        self.tick_count += 1;
    }
}
