#[cfg(test)]
mod tests;

use nom::{
    branch::alt,
    character::complete::{char as c, digit1, multispace0, one_of},
    combinator::{map, map_res},
    error::VerboseError,
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
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

fn parse_operation_md<'a>(input: &'a str) -> IResult<&'a str, Operation, VerboseError<&'a str>> {
    let (i, t) = one_of("*/")(input)?;
    Ok((
        i,
        match t {
            '*' => Operation::Multiplication,
            '/' => Operation::Division,
            _ => unreachable!(),
        },
    ))
}

fn parse_operation_as<'a>(input: &'a str) -> IResult<&'a str, Operation, VerboseError<&'a str>> {
    let (i, t) = one_of("+-")(input)?;
    Ok((
        i,
        match t {
            '+' => Operation::Addition,
            '-' => Operation::Subtraction,
            _ => unreachable!(),
        },
    ))
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i32),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
}

fn parse_factor<'a>(input: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    alt((
        map(parse_constant, Expression::Constant),
        delimited(
            terminated(c('('), multispace0),
            parse_expression,
            preceded(multispace0, c(')')),
        ),
    ))(input)
}

fn parse_term<'a>(input: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    let (i, (mut left, rights)) = tuple((
        parse_factor,
        many0(tuple((
            delimited(multispace0, parse_operation_md, multispace0),
            parse_factor,
        ))),
    ))(input)?;
    for right in rights {
        left = match right.0 {
            Operation::Multiplication => {
                Expression::Multiplication(Box::new(left), Box::new(right.1))
            }
            Operation::Division => Expression::Division(Box::new(left), Box::new(right.1)),
            _ => unreachable!(),
        };
    }
    Ok((i, left))
}

pub fn parse_expression<'a>(input: &'a str) -> IResult<&'a str, Expression, VerboseError<&'a str>> {
    let (i, (mut left, rights)) = tuple((
        parse_term,
        many0(tuple((
            delimited(multispace0, parse_operation_as, multispace0),
            parse_term,
        ))),
    ))(input)?;
    for right in rights {
        left = match right.0 {
            Operation::Addition => Expression::Addition(Box::new(left), Box::new(right.1)),
            Operation::Subtraction => Expression::Subtraction(Box::new(left), Box::new(right.1)),
            _ => unreachable!(),
        };
    }
    Ok((i, left))
}
