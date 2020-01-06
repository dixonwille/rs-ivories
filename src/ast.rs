#[cfg(test)]
use std::fmt;

use rand::Rng;

trait Evaluatable {
    fn evaluate<E: Rng>(&mut self, rng: &mut E);
}

pub enum ConditionalType {
    LessThan(Expression),
    LessThanOrEqualTo(Expression),
    GreaterThan(Expression),
    GreaterThanOrEqualTo(Expression),
    EqualTo(Expression),
    NotEqualTo(Expression),
}

pub struct Conditional {
    pub conditional: ConditionalType,
    pub evaluated: Option<SimpleConditionalType>,
}

impl Conditional {
    pub fn new(conditional: ConditionalType) -> Self {
        Conditional {
            conditional: conditional,
            evaluated: None,
        }
    }

    fn evaluate_many<E: Rng>(conds: &mut Vec<Conditional>, rng: &mut E) {
        for cond in conds.iter_mut() {
            cond.evaluate(rng);
        }
    }

    fn compare(&self, left: i32) -> bool {
        let e = match &self.evaluated {
            Some(e) => e,
            None => unreachable!("Conditional expression has not been simplified yet"),
        };
        e.compare(left)
    }

    fn compare_many(conds: &Vec<Conditional>, left: i32) -> bool {
        for cond in conds.iter() {
            if cond.compare(left) {
                return true;
            }
        }
        false
    }
}

impl Evaluatable for Conditional {
    fn evaluate<E: Rng>(&mut self, rng: &mut E) {
        match self.conditional {
            ConditionalType::LessThan(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("less than expression was not evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::LessThan(r));
            }
            ConditionalType::LessThanOrEqualTo(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("less than or equal to expression was not evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::LessThanOrEqualTo(r));
            }
            ConditionalType::GreaterThan(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("greater than expression was not evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::GreaterThan(r));
            }
            ConditionalType::GreaterThanOrEqualTo(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("greater than or equal to expression was not evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::GreaterThanOrEqualTo(r));
            }
            ConditionalType::EqualTo(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("equal to expression was not evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::EqualTo(r));
            }
            ConditionalType::NotEqualTo(ref mut e) => {
                e.evaluate(rng);
                let r = match e.evaluated {
                    Some(r) => r,
                    None => unreachable!("not equal to expression was no evaluated"),
                };
                self.evaluated = Some(SimpleConditionalType::NotEqualTo(r));
            }
        }
    }
}

#[cfg(test)]
impl fmt::Debug for Conditional {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match self.conditional {
            ConditionalType::LessThan(ref val) => write!(format, "<{:?}", val),
            ConditionalType::LessThanOrEqualTo(ref val) => write!(format, "<={:?}", val),
            ConditionalType::GreaterThan(ref val) => write!(format, ">{:?}", val),
            ConditionalType::GreaterThanOrEqualTo(ref val) => write!(format, ">={:?}", val),
            ConditionalType::EqualTo(ref val) => write!(format, "={:?}", val),
            ConditionalType::NotEqualTo(ref val) => write!(format, "!={:?}", val),
        }
    }
}

pub enum SimpleConditionalType {
    LessThan(i32),
    LessThanOrEqualTo(i32),
    GreaterThan(i32),
    GreaterThanOrEqualTo(i32),
    EqualTo(i32),
    NotEqualTo(i32),
}

impl SimpleConditionalType {
    fn compare(&self, left: i32) -> bool {
        match *self {
            SimpleConditionalType::LessThan(right) => left < right,
            SimpleConditionalType::LessThanOrEqualTo(right) => left <= right,
            SimpleConditionalType::GreaterThan(right) => left > right,
            SimpleConditionalType::GreaterThanOrEqualTo(right) => left >= right,
            SimpleConditionalType::EqualTo(right) => left == right,
            SimpleConditionalType::NotEqualTo(right) => left != right,
        }
    }
}

pub enum PoolConsolidator {
    Addition,
    Count(Option<Vec<Conditional>>),
}

impl PoolConsolidator {
    fn consolidate(&self, max: u32, rolls: &Vec<u32>) -> i32 {
        match self {
            PoolConsolidator::Addition => {
                let mut sum = 0;
                for roll in rolls.iter() {
                    sum = sum + roll;
                }
                sum as i32
            }
            PoolConsolidator::Count(conds) => match conds {
                Some(cs) => {
                    let mut count = 0;
                    for roll in rolls.iter() {
                        if Conditional::compare_many(cs, *roll as i32) {
                            count = count + 1;
                        }
                    }
                    count
                }
                None => {
                    let mut count = 0;
                    for roll in rolls.iter() {
                        if *roll == max {
                            count = count + 1;
                        }
                    }
                    count
                }
            },
        }
    }
}

impl Evaluatable for PoolConsolidator {
    fn evaluate<E: Rng>(&mut self, rng: &mut E) {
        match self {
            PoolConsolidator::Addition => {}
            PoolConsolidator::Count(conds) => match conds {
                Some(cs) => return Conditional::evaluate_many(cs, rng),
                None => return,
            },
        }
    }
}

#[cfg(test)]
impl fmt::Debug for PoolConsolidator {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::PoolConsolidator::*;
        match *self {
            Addition => write!(format, "+"),
            Count(ref cond) => match cond {
                Some(c) => write!(format, "#{:?}", c),
                None => write!(format, "#[]"),
            },
        }
    }
}

pub enum PoolModifier {
    DropHighest(Option<Expression>), // Drop the highest count
    DropLowest(Option<Expression>),  // Drop the lowest count
    Drop(Vec<Conditional>),          // What dice to drop based on conditionals
    CapClamp(Vec<Conditional>),      // What is capped and clamped based on conditionals
    ValueReplace(Vec<(Conditional, Expression)>), // List of conditionals and expressions to replace the dice with
    Unique, // Only allow unique dice rolls (re roll other dice)
    Reroll(Vec<Conditional>, Option<Expression>), // Conditionals and the max number of re rolls (optional where none means to run forever)
    Explode(Option<Vec<Conditional>>, Option<Expression>), // Optional Conditionals (None means use max value in pool)
    PatternExplode(Vec<Expression>), // Number pattern that if seen, will be exploded
}

#[cfg(test)]
impl fmt::Debug for PoolModifier {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        use self::PoolModifier::*;
        match *self {
            DropHighest(ref count) => match count {
                Some(c) => write!(format, "(DH{:?})", c),
                None => write!(format, "(DH)"),
            },
            DropLowest(ref count) => match count {
                Some(c) => write!(format, "(DL{:?})", c),
                None => write!(format, "(DL)"),
            },
            Drop(ref cond) => write!(format, "(D{:?})", cond),
            CapClamp(ref cond) => write!(format, "(C{:?})", cond),
            ValueReplace(ref cond) => write!(format, "(V{:?})", cond),
            Unique => write!(format, "(U)"),
            Reroll(ref cond, ref max) => match max {
                Some(m) => write!(format, "(R{:?}:{:?})", cond, m),
                None => write!(format, "(R{:?})", cond),
            },
            Explode(ref cond, ref exp) => {
                write!(format, "(E{{")?;
                match cond {
                    Some(c) => write!(format, "{:?}}}", c)?,
                    None => write!(format, "}}")?,
                };
                match exp {
                    Some(e) => write!(format, ":{:?})", e),
                    None => write!(format, ")"),
                }
            }
            PatternExplode(ref patt) => write!(format, "(PE{:?})", patt),
        }
    }
}

pub struct DicePool {
    pub count: u32,
    pub sides: u32,
    pub modifiers: Vec<PoolModifier>,
    pub consolidator: PoolConsolidator,
    pub evaluated: Option<i32>,
    pub original_rolls: Option<Vec<u32>>,
}

impl DicePool {
    pub fn new(count: u32, sides: u32) -> Self {
        DicePool {
            count: count,
            sides: sides,
            modifiers: vec![],
            consolidator: PoolConsolidator::Addition,
            evaluated: None,
            original_rolls: None,
        }
    }

    pub fn append_modifier(&mut self, modifier: PoolModifier) {
        self.modifiers.push(modifier);
    }

    pub fn append_modifiers(&mut self, modifiers: &mut Vec<PoolModifier>) {
        self.modifiers.append(modifiers);
    }
}

impl Evaluatable for DicePool {
    fn evaluate<E: Rng>(&mut self, rng: &mut E) {
        let mut rolled = Vec::with_capacity(self.count as usize);
        let mut index = 0;
        while index < self.count {
            let num: u32 = rng.gen_range(1, self.sides);
            rolled.push(num);
            index = index + 1;
        }
        self.original_rolls = Some(rolled);
        self.consolidator.evaluate(rng);
        self.evaluated = Some(
            self.consolidator
                .consolidate(self.sides, self.original_rolls.as_ref().unwrap()),
        );
    }
}

#[cfg(test)]
impl fmt::Debug for DicePool {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        write!(format, "({}d{}[", self.count, self.sides)?;
        for modif in &self.modifiers {
            write!(format, "{:?}", modif)?;
        }
        write!(format, "]")?;
        match &self.consolidator {
            PoolConsolidator::Addition => write!(format, "+)"),
            PoolConsolidator::Count(count) => match count {
                Some(c) => write!(format, "#{:?})", c),
                None => write!(format, "#)"),
            },
        }
    }
}

pub enum ExpressionType {
    Constant(i32),
    Pool(DicePool),
    Multiplication(Expression, Expression),
    Division(Expression, Expression),
    Addition(Expression, Expression),
    Subtraction(Expression, Expression),
}

pub struct Expression {
    pub expression: Box<ExpressionType>,
    pub evaluated: Option<i32>,
}

impl Expression {
    pub fn new(expression: ExpressionType) -> Self {
        Expression {
            expression: Box::new(expression),
            evaluated: None,
        }
    }

    fn evaluate_lr<E: Rng>(rng: &mut E, left: &mut Expression, right: &mut Expression) {
        left.evaluate(rng);
        right.evaluate(rng);
    }
}

impl Evaluatable for Expression {
    fn evaluate<E: Rng>(&mut self, rng: &mut E) {
        match *self.expression {
            ExpressionType::Constant(i) => {
                self.evaluated = Some(i);
            }
            ExpressionType::Multiplication(ref mut left, ref mut right) => {
                Expression::evaluate_lr(rng, left, right);
                let l = match left.evaluated {
                    Some(l) => l,
                    None => unreachable!("multiplication left was not evaluated"),
                };
                let r = match right.evaluated {
                    Some(r) => r,
                    None => unreachable!("multiplication right was not evaluated"),
                };
                self.evaluated = Some(l * r);
            }
            ExpressionType::Division(ref mut left, ref mut right) => {
                Expression::evaluate_lr(rng, left, right);
                let l = match left.evaluated {
                    Some(l) => l,
                    None => unreachable!("division left was not evaluated"),
                };
                let r = match right.evaluated {
                    Some(r) => r,
                    None => unreachable!("division right was not evaluated"),
                };
                self.evaluated = Some(l / r);
            }
            ExpressionType::Addition(ref mut left, ref mut right) => {
                Expression::evaluate_lr(rng, left, right);
                let l = match left.evaluated {
                    Some(l) => l,
                    None => unreachable!("addition left was not evaluated"),
                };
                let r = match right.evaluated {
                    Some(r) => r,
                    None => unreachable!("addition right was not evaluated"),
                };
                self.evaluated = Some(l + r);
            }
            ExpressionType::Subtraction(ref mut left, ref mut right) => {
                Expression::evaluate_lr(rng, left, right);
                let l = match left.evaluated {
                    Some(l) => l,
                    None => unreachable!("subtraction left was not evaluated"),
                };
                let r = match right.evaluated {
                    Some(r) => r,
                    None => unreachable!("subtraction right was not evaluated"),
                };
                self.evaluated = Some(l - r);
            }
            ExpressionType::Pool(ref mut p) => {
                p.evaluate(rng);
                let v = match p.evaluated {
                    Some(v) => v,
                    None => unreachable!("dice pool was not evaluated"),
                };
                self.evaluated = Some(v);
            }
        }
    }
}

#[cfg(test)]
impl<'a> fmt::Debug for Expression {
    fn fmt(&self, format: &mut fmt::Formatter) -> fmt::Result {
        match *self.expression {
            ExpressionType::Constant(ref val) => write!(format, "{}", val),
            ExpressionType::Pool(ref dice) => write!(format, "{:?}", dice),
            ExpressionType::Multiplication(ref left, ref right) => {
                write!(format, "({:?} * {:?})", left, right)
            }
            ExpressionType::Division(ref left, ref right) => {
                write!(format, "({:?} / {:?})", left, right)
            }
            ExpressionType::Addition(ref left, ref right) => {
                write!(format, "({:?} + {:?})", left, right)
            }
            ExpressionType::Subtraction(ref left, ref right) => {
                write!(format, "({:?} - {:?})", left, right)
            }
        }
    }
}
