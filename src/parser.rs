use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char as c, digit1, multispace0},
    combinator::{cut, map, map_res, opt},
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

use crate::ast::*;

enum Operation {
    Multiplication,
    Division,
    Addition,
    Subtraction,
}

// Parse a number
fn parse_digits(input: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse::<i32>())(input)
}

// Parses * or / and returns appropriate Operation
fn parse_operation_md(input: &str) -> IResult<&str, Operation> {
    map(alt((c('*'), c('/'))), |c| {
        if c == '*' {
            Operation::Multiplication
        } else {
            Operation::Division
        }
    })(input)
}

// Parses + or - and returns appropriate Operation
fn parse_operation_as(input: &str) -> IResult<&str, Operation> {
    map(alt((c('+'), c('-'))), |c| {
        if c == '+' {
            Operation::Addition
        } else {
            Operation::Subtraction
        }
    })(input)
}

// Parses a string into an i32. It also looks for negative numbers and adjusts accordingly
fn parse_constant(input: &str) -> IResult<&str, i32> {
    alt((
        parse_digits,
        map(preceded(c('-'), parse_digits), |i: i32| -1 * i),
    ))(input)
}

// Parses the parenthesis with an Expression in the middle
fn parse_parens(input: &str) -> IResult<&str, Expression> {
    delimited(c('('), parse_expression, c(')'))(input)
}

fn parse_conditional(input: &str) -> IResult<&str, Conditional> {
    alt((
        map(
            preceded(tag("<="), parse_limited_factor),
            Conditional::LessThanOrEqualTo,
        ),
        map(
            preceded(tag(">="), parse_limited_factor),
            Conditional::GreaterThanOrEqualTo,
        ),
        map(
            preceded(tag("!="), parse_limited_factor),
            Conditional::NotEqualTo,
        ),
        map(
            preceded(c('>'), parse_limited_factor),
            Conditional::GreaterThan,
        ),
        map(
            preceded(c('<'), parse_limited_factor),
            Conditional::LessThan,
        ),
        map(
            preceded(opt(c('=')), parse_limited_factor),
            Conditional::EqualTo,
        ),
    ))(input)
}

/* Parse the pool modifiers
    HE - Drop the Highest
    LE - Drop the Lowest
    D[C] - Drop based on Conditions
    C[C] - Cap or Clamp based on Conditions
    V[(C,E)] - Replace conditional with expression
    U - Reroll dice until all values are unique
    R{[C]}E - Reroll based on conditional with optional max number of rerolls
    !{[C]}E - Explode based off of optional conditions (defaults to highest value in dice) Max number of times optional
    !~[E]~ - Explode dice based off of a pattern
*/
fn parse_pool_modifier(input: &str) -> IResult<&str, PoolModifier> {
    alt((
        map(
            preceded(alt((c('H'), c('h'))), opt(parse_limited_factor)),
            PoolModifier::DropHighest,
        ),
        map(
            preceded(alt((c('L'), c('l'))), opt(parse_limited_factor)),
            PoolModifier::DropLowest,
        ),
        map(
            preceded(alt((c('D'), c('d'))), many1(parse_conditional)),
            PoolModifier::Drop,
        ),
        map(
            preceded(alt((c('C'), c('c'))), many1(parse_conditional)),
            PoolModifier::CapClamp,
        ),
        map(
            preceded(
                alt((c('V'), c('v'))),
                separated_nonempty_list(
                    c(','),
                    separated_pair(parse_conditional, c('='), parse_limited_factor),
                ),
            ),
            PoolModifier::ValueReplace,
        ),
        map(alt((c('U'), c('u'))), |_| PoolModifier::Unique),
        map(
            preceded(
                alt((c('R'), c('r'))),
                pair(
                    alt((
                        delimited(c('{'), parse_many_conditionals, c('}')),
                        parse_many_conditionals,
                    )),
                    opt(parse_limited_factor),
                ),
            ),
            |(v, m)| PoolModifier::Reroll(v, m),
        ),
        map(
            pair(
                delimited(tag("!{"), opt(parse_many_conditionals), c('}')),
                opt(parse_limited_factor),
            ),
            |(c, e)| PoolModifier::Explode(c, e),
        ),
        map(
            delimited(
                tag("!~"),
                separated_nonempty_list(c(','), parse_limited_factor),
                c('~'),
            ),
            PoolModifier::PatternExplode,
        ),
        map(preceded(c('!'), opt(parse_many_conditionals)), |c| {
            PoolModifier::Explode(c, None)
        }),
    ))(input)
}

fn parse_pool_consolidator(input: &str) -> IResult<&str, PoolConsolidator> {
    map(
        preceded(c('#'), opt(parse_many_conditionals)),
        PoolConsolidator::Count,
    )(input)
}

fn parse_many_conditionals(input: &str) -> IResult<&str, Vec<Conditional>> {
    separated_nonempty_list(c(','), parse_conditional)(input)
}

// Parses the #d# along with all the modifiers for the pool
fn parse_dice_pool(input: &str) -> IResult<&str, Expression> {
    let (i, mut dice) = tuple((
        separated_pair(opt(parse_digits), c('d'), parse_digits),
        many0(parse_pool_modifier),
        opt(parse_pool_consolidator),
    ))(input)?;
    let count = match (dice.0).0 {
        Some(i) => i,
        None => 1,
    };
    let mut pool = DicePool::new(count, (dice.0).1);
    pool.append_modifiers(&mut dice.1);
    if let Some(c) = dice.2 {
        pool.consolidator = c;
    }
    Ok((i, Expression::Pool(pool)))
}

// Don't allow DicePool as it should be in parens and no spacing around it
// Also only allow positive digits if no parens as it doesn't make logical sense to allow negatives here
fn parse_limited_factor(input: &str) -> IResult<&str, Expression> {
    alt((map(parse_digits, Expression::Constant), parse_parens))(input)
}

// Any Expression parser in here should account for space on either side of it
fn parse_factor(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(multispace0, parse_dice_pool, multispace0),
        map(
            delimited(multispace0, parse_constant, multispace0),
            Expression::Constant,
        ),
        delimited(multispace0, parse_parens, multispace0),
    ))(input)
}

fn fold(acc: Expression, (op, val): (Operation, Expression)) -> Expression {
    match op {
        Operation::Multiplication => Expression::Multiplication(Box::new(acc), Box::new(val)),
        Operation::Division => Expression::Division(Box::new(acc), Box::new(val)),
        Operation::Addition => Expression::Addition(Box::new(acc), Box::new(val)),
        Operation::Subtraction => Expression::Subtraction(Box::new(acc), Box::new(val)),
    }
}

fn parse_term(input: &str) -> IResult<&str, Expression> {
    let (i, mut init) = parse_factor(input)?;
    let (i, rights) = many0(alt((
        pair(parse_operation_md, cut(parse_factor)),
        map(parse_parens, |e| (Operation::Multiplication, e)),
        map(parse_dice_pool, |e| (Operation::Multiplication, e)),
    )))(i)?;
    for right in rights {
        init = fold(init, right);
    }
    Ok((i, init))
}

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    let (i, mut init) = parse_term(input)?;
    let (i, rights) = many0(pair(parse_operation_as, parse_term))(i)?;
    for right in rights {
        init = fold(init, right)
    }
    Ok((i, init))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! exhaust_valid {
        ($name:expr, $input:expr, $exp:expr) => {
            assert_eq!(
                $input.map(|(i, x)| (i, format!("{:?}", x))),
                Ok(("", String::from($exp))),
                "{} failed",
                $name
            );
        };
    }

    #[test]
    fn order_of_operations() {
        exhaust_valid!(
            "multiply before add",
            parse_expression("1+2*3"),
            "(1 + (2 * 3))"
        );
        exhaust_valid!(
            "divide before add",
            parse_expression("1+2/3"),
            "(1 + (2 / 3))"
        );
        exhaust_valid!(
            "multiply before subtract",
            parse_expression("1-2*3"),
            "(1 - (2 * 3))"
        );
        exhaust_valid!(
            "divide before subtract",
            parse_expression("1-2/3"),
            "(1 - (2 / 3))"
        );
        exhaust_valid!(
            "parenthesis before multiply",
            parse_expression("(1+2)*3"),
            "((1 + 2) * 3)"
        );
        exhaust_valid!(
            "parenthesis before divide",
            parse_expression("(1+2)/3"),
            "((1 + 2) / 3)"
        );
        exhaust_valid!(
            "left to right multiply and divide",
            parse_expression("1*2/3"),
            "((1 * 2) / 3)"
        );
        exhaust_valid!(
            "left to right add and subtract",
            parse_expression("1+2-3"),
            "((1 + 2) - 3)"
        );
        exhaust_valid!(
            "dice treated as constant on right",
            parse_expression("(1+3)3d10"),
            "((1 + 3) * (3d10[]+))"
        );
        exhaust_valid!(
            "dice treated as constant on left",
            parse_expression("3d10(1+3)"),
            "((3d10[]+) * (1 + 3))"
        );
        exhaust_valid!(
            "random order 1",
            parse_expression("(1+2)/3-4"),
            "(((1 + 2) / 3) - 4)"
        );
        exhaust_valid!(
            "random order 2",
            parse_expression("1/(2+3)*4-5"),
            "(((1 / (2 + 3)) * 4) - 5)"
        );
        exhaust_valid!(
            "random order 3",
            parse_expression("1+2-3*4/5"),
            "((1 + 2) - ((3 * 4) / 5))"
        );
    }

    #[test]
    fn spacing() {
        exhaust_valid!(
            "around operators",
            parse_expression("(1  +  2) *  3  "),
            "((1 + 2) * 3)"
        );
        exhaust_valid!(
            "around parenthesis",
            parse_expression("  (  1+2 )  *3"),
            "((1 + 2) * 3)"
        );
        exhaust_valid!(
            "random",
            parse_expression(" (  1 +   2)*  3 "),
            "((1 + 2) * 3)"
        );
        exhaust_valid!(
            "around dice",
            parse_expression("1+  2d10  +3"),
            "((1 + (2d10[]+)) + 3)"
        )
    }

    #[test]
    fn constants() {
        exhaust_valid!("single negative constant", parse_expression("-3"), "-3");
        exhaust_valid!(
            "negative constants",
            parse_expression("-1+2*-3"),
            "(-1 + (2 * -3))"
        );
    }
    #[test]
    fn parens_as_mult() {
        exhaust_valid!(
            "parenthesis as multiply",
            parse_expression("3(1+2)"),
            "(3 * (1 + 2))"
        );
    }

    #[test]
    fn dice_pool() {
        exhaust_valid!(
            "simple dice without count",
            parse_expression("d20"),
            "(1d20[]+)"
        );
        exhaust_valid!(
            "simple dice with count",
            parse_expression("5d20"),
            "(5d20[]+)"
        );
        exhaust_valid!(
            "dice with arithmatic",
            parse_expression("2d10 + 7"),
            "((2d10[]+) + 7)"
        );
        exhaust_valid!(
            "dice with arithmatic",
            parse_expression("2d10D<2 + 7"),
            "((2d10[(D[<2])]+) + 7)"
        );
        exhaust_valid!(
            "dice with arithmatic in condtional",
            parse_expression("2d10D<(2 + 7)"),
            "(2d10[(D[<(2 + 7)])]+)"
        );
        exhaust_valid!(
            "dice with dice in condtional",
            parse_expression("2d10D<(2d10)"),
            "(2d10[(D[<(2d10[]+)])]+)"
        );
        exhaust_valid!("drop highest", parse_expression("5d10H2"), "(5d10[(DH2)]+)");
        exhaust_valid!("drop lowest", parse_expression("5d10L2"), "(5d10[(DL2)]+)");
        exhaust_valid!(
            "drop highest w/o expression",
            parse_expression("5d10H"),
            "(5d10[(DH)]+)"
        );
        exhaust_valid!(
            "drop lowest w/o expression",
            parse_expression("5d10L"),
            "(5d10[(DL)]+)"
        );
        exhaust_valid!(
            "drop single conditional",
            parse_expression("5d10D<=2"),
            "(5d10[(D[<=2])]+)"
        );
        exhaust_valid!(
            "drop single conditional",
            parse_expression("5d10D<=2"),
            "(5d10[(D[<=2])]+)"
        );
        exhaust_valid!(
            "drop multiple conditional",
            parse_expression("5d10D<=2>7"),
            "(5d10[(D[<=2, >7])]+)"
        );
        exhaust_valid!(
            "cap or clamp single conditional",
            parse_expression("5d10C<=2"),
            "(5d10[(C[<=2])]+)"
        );
        exhaust_valid!("Unique", parse_expression("5d10U"), "(5d10[(U)]+)");
        exhaust_valid!(
            "cap or clamp multiple conditional",
            parse_expression("5d10C<=2>7"),
            "(5d10[(C[<=2, >7])]+)"
        );
        exhaust_valid!(
            "multiple conditions",
            parse_expression("5d10D<=2C>=7"),
            "(5d10[(D[<=2])(C[>=7])]+)"
        );
        exhaust_valid!(
            "conditional with equals",
            parse_expression("5d10D=2"),
            "(5d10[(D[=2])]+)"
        );
        exhaust_valid!(
            "conditional with equals",
            parse_expression("5d10D2"),
            "(5d10[(D[=2])]+)"
        );
        exhaust_valid!(
            "replace",
            parse_expression("5d10V5=2,>7=2"),
            "(5d10[(V[(=5, 2), (>7, 2)])]+)"
        );
        exhaust_valid!(
            "reroll simple",
            parse_expression("5d10R2"),
            "(5d10[(R[=2])]+)"
        );
        exhaust_valid!(
            "reroll conditions",
            parse_expression("5d10R<2"),
            "(5d10[(R[<2])]+)"
        );
        exhaust_valid!(
            "reroll conditions with max",
            parse_expression("5d10R{<2}3"),
            "(5d10[(R[<2]:3)]+)"
        );
        exhaust_valid!(
            "reroll multiple conditionals w/o brackets",
            parse_expression("5d10R<=2,3"),
            "(5d10[(R[<=2, =3])]+)"
        );
        exhaust_valid!(
            "reroll multiple conditionals w/ brackets",
            parse_expression("5d10R{<=2,3}"),
            "(5d10[(R[<=2, =3])]+)"
        );
        exhaust_valid!("Explosion", parse_expression("5d10!"), "(5d10[(E{})]+)");
        exhaust_valid!(
            "Explosion with conditionals",
            parse_expression("5d10!<=2"),
            "(5d10[(E{[<=2]})]+)"
        );
        exhaust_valid!(
            "Explosion with conditionals brackets",
            parse_expression("5d10!{<=2}"),
            "(5d10[(E{[<=2]})]+)"
        );
        exhaust_valid!(
            "Explosion with conditionals and max",
            parse_expression("5d10!{<=2}3"),
            "(5d10[(E{[<=2]}:3)]+)"
        );
        exhaust_valid!(
            "Explosion with max",
            parse_expression("5d10!{}3"),
            "(5d10[(E{}:3)]+)"
        );
        exhaust_valid!(
            "Explosion Pattern",
            parse_expression("5d10!~6,5,2~"),
            "(5d10[(PE[6, 5, 2])]+)"
        );
        exhaust_valid!(
            "Count with conditionals",
            parse_expression("5d10#<=2"),
            "(5d10[]#[<=2])"
        );
        exhaust_valid!(
            "Count with out conditionals",
            parse_expression("5d10#"),
            "(5d10[]#)"
        );
    }
}
