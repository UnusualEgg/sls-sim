use serde::{Deserialize, Deserializer, Serialize};
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::fs::File;
use std::rc::{Rc, Weak};
use std::str::FromStr;
use std::time::Instant;

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

fn default_outputs() -> Rc<RefCell<Vec<bool>>> {
    return Rc::new(RefCell::new(Vec::new()));
}
#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub struct Node {
    #[serde(rename = "TAG")]
    pub node_type: NodeType,
    #[serde(default)]
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
    pub label: Option<String>,
    enabled: Option<bool>,
    uri: Option<String>,
    cid: Option<String>,
    num_of_in: Option<usize>,
    num_of_out: Option<usize>,

    #[serde(skip)]
    pub ic_instance: Option<IC>,
    #[serde(default)]
    pub period: u64,
    #[serde(skip)]
    pub last_cycle: Option<Instant>,
}

impl Node {
    //of IC
    fn set_instance(&mut self, dependencies: &BTreeMap<String, IC>) {
        //let comp = self.components[i].clone();
        if self.ic_instance.is_some() {
            return;
        }
        let cid = self.cid.as_ref().expect("cid");
        let original = &dependencies[cid];
        let mut new = IC {
            header: original.header.clone(),
            components: Vec::with_capacity(original.components.len()),
            inputs: Vec::with_capacity(self.num_of_in.expect("num_of_in")),
            outputs: Vec::with_capacity(self.num_of_out.expect("num_of_out")),
            wires: original.wires.clone(),
            subdeps: BTreeSet::new(),
        };
        //individualy clone components
        for comp in &original.components {
            let mut new_comp = comp.clone();
            new_comp.outputs = Rc::new(RefCell::new((*comp.outputs.try_borrow().unwrap()).clone()));
            new.components.push(new_comp);
        }
        new.init_ic(dependencies);
        self.ic_instance = Some(new);
    }
    #[must_use = "these are the deps needed"]
    fn ic_get_subdeps(
        &self,
        dependencies: &BTreeMap<String, IC>,
        uris: &HashMap<String, String>,
    ) -> HashMap<String, String> {
        let mut needed: HashMap<String, String> = HashMap::new();
        let cid = self.cid.as_ref().expect("cid");
        let uri = match self.uri.as_ref() {
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
            println!("needed {}", uri);
        }
        needed
    }
    fn init_output(&mut self) {
        let output_n = match self.node_type {
            NodeType::INTEGRATED_CIRCUIT => {
                let num_of_out = self.num_of_out.expect("num_of_out");
                println!(
                    "{} outputs for {}",
                    num_of_out,
                    self.label.as_ref().get_or_insert(&"IC".to_string())
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
        //println!("inputs:{:#?} for {:?}", &self.inputs, self.node_type);
    }
    fn get_input(&self, input: &Input, default: bool) -> bool {
        match input.other_output.upgrade() {
            Some(x) => {
                let o = (&x).try_borrow().unwrap();
                if input.other_pin >= o.len() {
                    println!(
                        "{:#?}[pin: {}] id:{}",
                        o, input.other_pin, &input.other_id.0
                    );
                }
                o[input.other_pin]
            }
            None => default,
        }
    }
    fn get_inputs(&mut self) {
        let default = match &self.node_type {
            NodeType::AND_GATE | NodeType::NOR_GATE => true,
            _ => false,
        };
        for (i, input) in self.inputs.iter().enumerate() {
            self.input_states[i] = InputState {
                in_pin: input.in_pin,
                state: self.get_input(input, default),
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
                        self.label.as_ref().unwrap_or(&"IC".to_string())
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
        formatter.write_str("String")
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_str(&v)
    }
    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_str(v)
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
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
fn sort_comps(components: &mut Vec<Node>) {
    components.sort_by(|comp1, comp2| comp1.y.total_cmp(&comp2.y));
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
    #[serde(skip)]
    subdeps: BTreeSet<String>,
}
impl IC {
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
            comp.input_states
                .resize(comp.inputs.len(), InputState::default());
            println!(
                "resizing({:?}): input_states: {:?} inputs:{:?}",
                &comp.node_type, &comp.input_states, &comp.inputs
            );
        }
        //for new fromat with wires
        for wire in &self.wires {
            let comp = self
                .components
                .iter_mut()
                .find(|comp| comp.id == wire.to.0)
                .unwrap();
            comp.inputs.push(Input {
                other_output: ids.get(&wire.from.0).unwrap().clone(),
                other_pin: wire.from.1,
                other_id: wire.from.0.clone(),
                in_pin: wire.to.1,
            });
            comp.input_states
                .resize(comp.inputs.len(), InputState::default());
            println!(
                "resizing({:?}): input_states: {:?} inputs:{:?}",
                &comp.node_type, &comp.input_states, &comp.inputs
            );
        }
    }
    //get input indecies
    fn init_ic(&mut self, deps: &BTreeMap<String, IC>) {
        sort_comps(&mut self.components);
        for comp in self.components.iter_mut() {
            comp.init_output();
        }
        self.connect();
        for comp in self.components {
            //init input indecies Vec
            if comp.node_type == NodeType::PULSE_BUTTON || comp.node_type == NodeType::TOGGLE_BUTTON
            {
                self.inputs.push(i);
            } else if comp.node_type == NodeType::LIGHT_BULB {
                self.outputs.push(i);
            }
        }
        for comp in &mut self.components {
            comp.set_instance(&deps);
        }
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
    dependencies: BTreeMap<String, IC>,
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
/// returns true when added and false when already in hashmap
fn add_dep<'hm>(dependencies: &'hm mut BTreeMap<String, IC>, cid: &String, uri: &String) -> bool {
    //let result: Option<&IC> = self.dependencies.get(cid);
    if !dependencies.contains_key(cid) {
        //open URI
        match File::open(uri) {
            Err(e) => {
                panic!("Couldn't find URI:\"{}\"| {}", uri, e);
            }

            Ok(f) => {
                let c: IC = match serde_json::from_reader(std::io::BufReader::new(f)) {
                    Err(e) => {
                        panic!("Couldn't find URI:\"{}\"| {}", uri, e);
                    }
                    Ok(c) => c,
                };
                println!("adding {}({})", uri, cid);
                dependencies.insert(cid.clone(), c);

                //Some(&dependencies[cid])
                true
            }
        }
    } else {
        //None
        false
    }
}
impl Circuit {
    //go thru each dep and init (which will add more deps)
    fn add_deps(&mut self) {
        //URIs
        if let Ok(s) = std::fs::read_to_string("URIs.json") {
            self.uris = serde_json::from_str(&s).unwrap();
            println!("added URIs.json: {:?}", &self.uris);
        }
        println!("add sub deps");
        //add ones from coomponents
        self.components
            .iter()
            .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
            .for_each(|comp| {
                comp.ic_get_subdeps(&self.dependencies, &self.uris)
                    .iter()
                    .for_each(|(cid, name)| {
                        add_dep(&mut self.dependencies, cid, name);
                    });
            });
        //then add ones from deps
        let mut need_init: Vec<String> = self.dependencies.keys().map(|k| k.clone()).collect();
        while need_init.len() > 0 {
            let mut adding = BTreeMap::new();
            let mut new_need_init: Vec<String> = Vec::new();
            for comp_cid in need_init.iter() {
                let dep = self.dependencies.get(comp_cid).unwrap();
                println!("dep:{}", &dep.header.name);
                dep.components
                    .iter()
                    .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
                    .for_each(|comp| {
                        new_need_init.extend(
                            comp.ic_get_subdeps(&self.dependencies, &self.uris)
                                .iter()
                                .filter(|(cid, _)| !self.dependencies.contains_key(cid.as_str()))
                                .map(|(cid, name)| {
                                    add_dep(&mut adding, cid, name);
                                    cid.clone()
                                }),
                        );
                    });
            }
            self.dependencies.extend(adding);
            need_init = new_need_init;
        }
        //should be y-sorter
        for (_, dep) in self.dependencies.iter_mut() {
            dep.components
                .sort_by(|comp1, comp2| comp1.y.partial_cmp(&comp2.y).unwrap());
        }
        //now set all the ic instances
        //TODO actually set_instance only in init_ic
        /*
        //every IC is a dep
        for (_, dep) in self.dependencies.iter_mut() {
            dep.components
                .iter()
                .filter(|c| c.node_type == NodeType::INTEGRATED_CIRCUIT)
                .for_each(|comp| {
                    dep.subdeps
                        .insert(comp.cid.clone().expect("IC to have CID"));
                });
        }
        let mut need: BTreeMap<String, BTreeSet<String>> = self
            .dependencies
            .iter()
            .map(|(cid, ic)| (cid.clone(), ic.subdeps.clone()))
            .collect();
        let mut more = true;
        while more {
            more = false;
            //loop thru deps
            //if has no subdeps then set comps' instances
            //loop thru subdeps
            //if subdep has instance, then remove subdep from vec
            let deps_clone: BTreeMap<String, IC> = self.dependencies.clone();
            for (cid, _) in need.iter_mut().filter(|(_, subdeps)| subdeps.len() == 0) {
                for comp in &mut self.dependencies.get_mut(cid).unwrap().components {
                    comp.set_instance(&deps_clone);
                }
            }
            need = need
                .into_iter()
                .filter(|(_, subdeps)| subdeps.len() > 0)
                .collect();
        }
        */
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
            comp.input_states
                .resize(comp.inputs.len(), InputState::default());
            println!(
                "resizing({:?}): input_states: {:?} inputs:{:?}",
                &comp.node_type, &comp.input_states, &comp.inputs
            );
        }
        //for new fromat with wires
        for wire in &self.wires {
            let comp = self
                .components
                .iter_mut()
                .find(|comp| comp.id == wire.to.0)
                .unwrap();
            comp.inputs.push(Input {
                other_output: ids.get(&wire.from.0).unwrap().clone(),
                other_pin: wire.from.1,
                other_id: wire.from.0.clone(),
                in_pin: wire.to.1,
            });
            comp.input_states
                .resize(comp.inputs.len(), InputState::default());
            println!(
                "resizing({:?}): input_states: {:?} inputs:{:?}",
                &comp.node_type, &comp.input_states, &comp.inputs
            );
        }
    }
    pub fn init_circ(&mut self) {
        //1: sort components by y for io
        self.components
            .sort_by(|comp1, comp2| comp1.y.partial_cmp(&comp2.y).unwrap());
        //2: init node output vecs
        for comp in &mut self.components {
            comp.init_output();
        }
        //add depependencies
        self.add_deps();
        //init ic's (inclides settimg instamces)
        let deps_clone = self.dependencies.clone();
        for (_, ic) in self.components.iter_mut() {
            ic.init_ic(&deps_clone);
        }
        //find io
        for i in 0..self.components.len() {
            let node_type = self.components[i].node_type.clone();
            if node_type == NodeType::PULSE_BUTTON || node_type == NodeType::TOGGLE_BUTTON {
                self.inputs.push(i);
            } else if node_type == NodeType::LIGHT_BULB
                || node_type == NodeType::SEVEN_SEGMENT_DISPLAY_DECODER
            {
                self.outputs.push(i);
            }
        }
        //coonnect components
        self.connect();
        println!("wires: {:?}", self.wires);
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
