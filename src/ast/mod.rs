#[cfg(test)]
mod tests;

use nom::{
    branch::alt,
    bytes::complete::take_till1,
    character::complete::{char as c, digit1, multispace0, one_of},
    combinator::{all_consuming, map, map_res},
    error::VerboseError,
    sequence::{delimited, preceded, terminated},
    IResult,
};

fn parse_constant<'a>(input: &'a str) -> IResult<&'a str, i32, VerboseError<&'a str>> {
    alt((
        map_res(digit1, |s: &str| s.parse::<i32>()),
        map(preceded(c('-'), digit1), |s: &str| {
            -1 * s.parse::<i32>().unwrap()
        }),
    ))(input)
}

enum Operation {
    Multiplication,
    Division,
    Addition,
    Subtraction,
}

fn parse_operation<'a>(input: &'a str) -> IResult<&'a str, Operation, VerboseError<&'a str>> {
    let (i, t) = one_of("*/+-")(input)?;
    Ok((
        i,
        match t {
            '*' => Operation::Multiplication,
            '/' => Operation::Division,
            '+' => Operation::Addition,
            '-' => Operation::Subtraction,
            _ => unreachable!(),
        },
    ))
}

fn is_add_or_sub(c: char) -> bool {
    match c {
        '+' | '-' => true,
        _ => false,
    }
}

fn is_mult_div(c: char) -> bool {
    match c {
        '*' | '/' => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i32),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
}

fn parse_binary_operation<'a, C>(
    comp: &'a C,
) -> impl Fn(&'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>>
where
    C: Fn(char) -> bool,
{
    move |input| {
        let (i, left_str) = take_till1(comp)(input)?;
        let (i, op) = terminated(parse_operation, multispace0)(i)?;
        let (_, left) = all_consuming(terminated(parse_expression, multispace0))(left_str)?;
        let (i, right) = parse_expression(i)?;
        match op {
            Operation::Multiplication => Ok((
                i,
                Expression::Multiplication(Box::new(left), Box::new(right)),
            )),
            Operation::Division => Ok((i, Expression::Division(Box::new(left), Box::new(right)))),
            Operation::Addition => Ok((i, Expression::Addition(Box::new(left), Box::new(right)))),
            Operation::Subtraction => {
                Ok((i, Expression::Subtraction(Box::new(left), Box::new(right))))
            }
        }
    }
}

fn parse_parenthesis<'a>(input: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    delimited(
        preceded(multispace0, c('(')),
        parse_expression,
        terminated(c(')'), multispace0),
    )(input)
}

pub fn parse_expression<'a>(input: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    alt((
        parse_binary_operation(&is_add_or_sub),
        parse_binary_operation(&is_mult_div),
        parse_parenthesis,
        map(parse_constant, Expression::Constant),
    ))(input)
}
