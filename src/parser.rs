use nom::{
    branch::alt,
    character::complete::{char as c, digit1, multispace0},
    combinator::{cut, map, map_res},
    multi::many0,
    sequence::{delimited, pair, preceded},
    IResult,
};

use crate::ast::*;

enum Operation {
    Multiplication,
    Division,
    Addition,
    Subtraction,
}

fn parse_operation_md(input: &str) -> IResult<&str, Operation> {
    map(alt((c('*'), c('/'))), |c| {
        if c == '*' {
            Operation::Multiplication
        } else {
            Operation::Division
        }
    })(input)
}

fn parse_operation_as(input: &str) -> IResult<&str, Operation> {
    map(alt((c('+'), c('-'))), |c| {
        if c == '+' {
            Operation::Addition
        } else {
            Operation::Subtraction
        }
    })(input)
}

fn parse_constant(input: &str) -> IResult<&str, i32> {
    delimited(
        multispace0,
        alt((
            map_res(digit1, |s: &str| s.parse::<i32>()),
            map(preceded(c('-'), digit1), |s: &str| {
                -1 * s.parse::<i32>().unwrap()
            }),
        )),
        multispace0,
    )(input)
}

fn parse_parens(input: &str) -> IResult<&str, Expression> {
    delimited(
        multispace0,
        delimited(c('('), parse_expression, c(')')),
        multispace0,
    )(input)
}

// Any Expression parser in here should account for space on either side of it
fn parse_factor(input: &str) -> IResult<&str, Expression> {
    alt((map(parse_constant, Expression::Constant), parse_parens))(input)
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
}
