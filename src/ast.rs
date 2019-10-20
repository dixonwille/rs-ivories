#[cfg(test)]
use std::fmt;

pub enum Value {
    Constant(i32, Option<BinaryOp>),
    Operator(BinaryOp),
}

#[cfg(test)]
impl fmt::Debug for Value {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::Value::*;
        match *self {
            Constant(ref val, ref next) => {
                let mut n = String::new();
                if let Some(op) = next {
                    n = format!("{:?}", op);
                }
                write!(format, "{}{}", val, n)
            }
            Operator(ref op) => write!(format, "{:?}", op),
        }
    }
}

pub enum BinaryOp {
    Or(Box<Conditional>),
    And(Box<Conditional>),
}

#[cfg(test)]
impl fmt::Debug for BinaryOp {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::BinaryOp::*;
        match *self {
            Or(ref cond) => write!(format, "|{:?}", cond),
            And(ref cond) => write!(format, "&{:?}", cond),
        }
    }
}

pub enum Conditional {
    LessThan(Value),
    GreaterThan(Value),
    EqualTo(Value),
}

#[cfg(test)]
impl fmt::Debug for Conditional {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::Conditional::*;
        match *self {
            LessThan(ref val) => write!(format, "<{:?}", val),
            GreaterThan(ref val) => write!(format, ">{:?}", val),
            EqualTo(ref val) => write!(format, "={:?}", val),
        }
    }
}

pub enum PoolModifier {
    DropHighest(i32),
    DropLowest(i32),
    Drop(Conditional),
    CapClamp(Conditional),
    Replace(i32, i32),
    NoRepeats,
    ReRoll(Conditional, i32),
    NotSettle(Conditional),
    Explode(Conditional, i32),
    LimitedExplode(Conditional, i32),
    PatternExplode(Vec<i32>),
    Count(Conditional),
}

#[cfg(test)]
impl fmt::Debug for PoolModifier {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::PoolModifier::*;
        match *self {
            DropHighest(ref count) => write!(format, "(DH{})", count),
            DropLowest(ref count) => write!(format, "(DL{})", count),
            Drop(ref cond) => write!(format, "(D{:?})", cond),
            CapClamp(ref cond) => write!(format, "(C{:?})", cond),
            Replace(ref old, ref new) => write!(format, "({}->{})", old, new),
            NoRepeats => write!(format, "(NR)"),
            ReRoll(ref cond, ref count) => write!(format, "(R{:?}:{})", cond, count),
            NotSettle(ref cond) => write!(format, "(R{:?})", cond),
            Explode(ref cond, ref count) => write!(format, "(E{:?}:{})", cond, count),
            LimitedExplode(ref cond, ref count) => write!(format, "(LE{:?}:{})", cond, count),
            PatternExplode(ref patt) => write!(format, "(PE{:?})", patt),
            Count(ref cond) => write!(format, "(#{:?})", cond),
        }
    }
}

pub struct DicePool {
    pub count: i32,
    pub sides: i32,
    pub modifiers: Vec<PoolModifier>,
}

#[cfg(test)]
impl fmt::Debug for DicePool {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        let mut modifiers = String::new();
        for modif in &self.modifiers {
            modifiers += &format!("{:?}", modif);
        }
        write!(format, "({}d{}[{}])", self.count, self.sides, modifiers)
    }
}

impl DicePool {
    pub fn new(count: i32, sides: i32) -> Self {
        DicePool {
            count: count,
            sides: sides,
            modifiers: vec![],
        }
    }
    pub fn append_modifier(&mut self, modifier: PoolModifier) {
        self.modifiers.push(modifier);
    }
}

pub enum Expression {
    Constant(i32),
    Pool(DicePool),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
}

#[cfg(test)]
impl fmt::Debug for Expression {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::Expression::*;
        match *self {
            Constant(ref val) => write!(format, "{}", val),
            Pool(ref dice) => write!(format, "{:?}", dice),
            Multiplication(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Division(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Addition(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Subtraction(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
        }
    }
}

impl Expression {
    pub fn evaluate(self) -> Result<i32, &'static str> {
        Ok(0)
    }
}
