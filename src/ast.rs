#[cfg(test)]
use std::fmt;

use rand::Rng;

pub enum Conditional {
    LessThan(Expression),
    LessThanOrEqualTo(Expression),
    GreaterThan(Expression),
    GreaterThanOrEqualTo(Expression),
    EqualTo(Expression),
    NotEqualTo(Expression),
}

impl Conditional {
    fn evaluate<E: Rng>(&self, rng: &mut E, left: i32) -> Result<bool, &'static str> {
        match self {
            Conditional::LessThan(e) => {
                let right = e.evaluate(rng)?;
                Ok(left < right)
            }
            Conditional::LessThanOrEqualTo(e) => {
                let right = e.evaluate(rng)?;
                Ok(left <= right)
            }
            Conditional::GreaterThan(e) => {
                let right = e.evaluate(rng)?;
                Ok(left > right)
            }
            Conditional::GreaterThanOrEqualTo(e) => {
                let right = e.evaluate(rng)?;
                Ok(left >= right)
            }
            Conditional::EqualTo(e) => {
                let right = e.evaluate(rng)?;
                Ok(left == right)
            }
            Conditional::NotEqualTo(e) => {
                let right = e.evaluate(rng)?;
                Ok(left != right)
            }
        }
    }
    fn evaluate_many<E: Rng>(
        conds: &Vec<Conditional>,
        rng: &mut E,
        left: i32,
    ) -> Result<bool, &'static str> {
        for cond in conds.iter() {
            if cond.evaluate(rng, left)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
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

pub enum PoolConsolidator {
    Addition,
    Count(Option<Vec<Conditional>>),
}

impl PoolConsolidator {
    fn evaluate<E: Rng>(
        &self,
        rng: &mut E,
        max: u32,
        rolls: Vec<u32>,
    ) -> Result<u32, &'static str> {
        match self {
            PoolConsolidator::Addition => {
                let mut sum = 0;
                for roll in rolls.iter() {
                    sum = sum + roll;
                }
                Ok(sum)
            }
            PoolConsolidator::Count(conds) => match conds {
                Some(cs) => {
                    let mut count = 0;
                    for roll in rolls.iter() {
                        if Conditional::evaluate_many(cs, rng, *roll as i32)? {
                            count = count + 1;
                        }
                    }
                    Ok(count)
                }
                None => {
                    let mut count = 0;
                    for roll in rolls.iter() {
                        if *roll == max {
                            count = count + 1;
                        }
                    }
                    Ok(count)
                }
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

impl DicePool {
    pub fn new(count: u32, sides: u32) -> Self {
        DicePool {
            count: count,
            sides: sides,
            modifiers: vec![],
            consolidator: PoolConsolidator::Addition,
        }
    }

    pub fn append_modifier(&mut self, modifier: PoolModifier) {
        self.modifiers.push(modifier);
    }

    pub fn append_modifiers(&mut self, modifiers: &mut Vec<PoolModifier>) {
        self.modifiers.append(modifiers);
    }

    fn evaluate<E: Rng>(&self, rng: &mut E) -> Result<u32, &'static str> {
        if self.count < 1 {
            return Err("Must have at least one dice to roll");
        }
        if self.sides < 2 {
            return Err("Must have at least 2 sides to roll");
        }
        let mut rolled = vec![];
        let mut index = 0;
        while index < self.count {
            let num: u32 = rng.gen_range(1, self.sides);
            rolled.push(num);
            index = index + 1;
        }
        self.consolidator.evaluate(rng, self.sides, rolled)
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
    pub fn evaluate<E: Rng>(&self, rng: &mut E) -> Result<i32, &'static str> {
        match self {
            Expression::Constant(i) => Ok(*i),
            Expression::Multiplication(left, right) => {
                let (l, r) = evaluate_lr(rng, left, right)?;
                Ok(l * r)
            }
            Expression::Division(left, right) => {
                let (l, r) = evaluate_lr(rng, left, right)?;
                Ok(l / r)
            }
            Expression::Addition(left, right) => {
                let (l, r) = evaluate_lr(rng, left, right)?;
                Ok(l + r)
            }
            Expression::Subtraction(left, right) => {
                let (l, r) = evaluate_lr(rng, left, right)?;
                Ok(l - r)
            }
            Expression::Pool(p) => {
                let v = p.evaluate(rng)?;
                Ok(v as i32)
            }
        }
    }
}

fn evaluate_lr<E: Rng>(
    rng: &mut E,
    left: &Box<Expression>,
    right: &Box<Expression>,
) -> Result<(i32, i32), &'static str> {
    let l = left.evaluate(rng)?;
    let r = right.evaluate(rng)?;
    Ok((l, r))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{rngs::StdRng, SeedableRng};

    macro_rules! valid_evaluate {
        ($name:expr, $input:expr, $exp:expr, $rng:expr) => {
            assert_eq!($input.evaluate($rng), Ok($exp), "{} failed", $name);
        };
    }

    #[test]
    fn arithmatic() {
        let seed: [u8; 32] = [1; 32];
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        valid_evaluate!(
            "Addition",
            Expression::Addition(
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Constant(11))
            ),
            21,
            &mut rng
        );
        valid_evaluate!(
            "Subtraction",
            Expression::Subtraction(
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Constant(11))
            ),
            -1,
            &mut rng
        );
        valid_evaluate!(
            "Multiplication",
            Expression::Multiplication(
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Constant(11))
            ),
            110,
            &mut rng
        );
        valid_evaluate!(
            "Division",
            Expression::Division(
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Constant(11))
            ),
            0,
            &mut rng
        );
        valid_evaluate!(
            "Multiple Levels",
            Expression::Addition(
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(10)),
                    Box::new(Expression::Constant(11))
                )),
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(10)),
                    Box::new(Expression::Constant(11))
                ))
            ),
            220,
            &mut rng
        );
    }

    #[test]
    fn dice_pool() {
        let seed: [u8; 32] = [1; 32];
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        valid_evaluate!("d20", Expression::Pool(DicePool::new(1, 20)), 3, &mut rng);
        valid_evaluate!("3d20", Expression::Pool(DicePool::new(3, 20)), 31, &mut rng);
        valid_evaluate!(
            "3d20#",
            Expression::Pool(DicePool {
                consolidator: PoolConsolidator::Count(None),
                modifiers: vec![],
                count: 3,
                sides: 20
            }),
            0,
            &mut rng
        );
        valid_evaluate!(
            "3d20#<20",
            Expression::Pool(DicePool {
                consolidator: PoolConsolidator::Count(Some(vec![Conditional::LessThan(
                    Expression::Constant(20)
                )])),
                modifiers: vec![],
                count: 3,
                sides: 20
            }),
            3,
            &mut rng
        );
    }
}
