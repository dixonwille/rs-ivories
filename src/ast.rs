#[cfg(test)]
use std::fmt;

pub enum Conditional {
    LessThan(Expression),
    LessThanOrEqualTo(Expression),
    GreaterThan(Expression),
    GreaterThanOrEqualTo(Expression),
    EqualTo(Expression),
    NotEqualTo(Expression),
}

#[cfg(test)]
impl fmt::Debug for Conditional {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::Conditional::*;
        match *self {
            LessThan(ref val) => write!(format, "<{:?}", val),
            LessThanOrEqualTo(ref val) => write!(format, "<={:?}", val),
            GreaterThan(ref val) => write!(format, ">{:?}", val),
            GreaterThanOrEqualTo(ref val) => write!(format, ">={:?}", val),
            EqualTo(ref val) => write!(format, "={:?}", val),
            NotEqualTo(ref val) => write!(format, "!={:?}", val),
        }
    }
}

pub enum PoolModifier {
    DropHighest(i32),                         // Drop the highest count
    DropLowest(i32),                          // Drop the lowest count
    Drop(Vec<Conditional>),                   // What dice to drop based on conditionals
    CapClamp(Vec<Conditional>),               // What is capped and clamped based on conditionals
    Replace(Vec<(Conditional, Expression)>), // List of conditionals and expressions to replace the dice with
    NoRepeats,                               // Only allow unique dice rolls (re roll other dice)
    NotSettle(Vec<Conditional>, Option<i32>), // Conditionals and the max number of re rolls (optional where none means to run forever)
    Explode(Option<Vec<Conditional>>), // Optional Conditionals (None means use max value in pool)
    LimitedExplode(Option<Vec<Conditional>>), // Optional Conditionals (None means use max value in pool)
    PatternExplode(Vec<i32>),                 // Number pattern that if seen, will be exploded
    Count(Option<Vec<Conditional>>), // Optional Conditionals (None means use max value in pool)
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
            Replace(ref cond) => write!(format, "(R{:?})", cond),
            NoRepeats => write!(format, "(NR)"),
            NotSettle(ref cond, ref max) => match max {
                Some(m) => write!(format, "(R{:?}:{})", cond, m),
                None => write!(format, "(R{:?})", cond),
            },
            Explode(ref cond) => match cond {
                Some(v) => write!(format, "(E{:?})", v),
                None => write!(format, "(E)"),
            },
            LimitedExplode(ref cond) => match cond {
                Some(v) => write!(format, "(LE{:?})", v),
                None => write!(format, "(LE)"),
            },
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

    pub fn append_modifiers(&mut self, modifiers: &mut Vec<PoolModifier>) {
        self.modifiers.append(modifiers);
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
