use super::*;

#[test]
fn valid_input_space_ops() {
    assert_eq!(
        parse_expression("(1 + 2) * 3"),
        Ok((
            "",
            Expression::Multiplication(
                Box::new(Expression::Addition(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}

#[test]
fn valid_input_space_parens() {
    assert_eq!(
        parse_expression("( 1+2 ) *3"),
        Ok((
            "",
            Expression::Multiplication(
                Box::new(Expression::Addition(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}

#[test]
fn valid_input_negation() {
    assert_eq!(
        parse_expression("-1 + 2 * -3"),
        Ok((
            "",
            Expression::Addition(
                Box::new(Expression::Constant(-1)),
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(-3))
                ))
            )
        ))
    );
}

#[test]
fn valid_input_mult_before_add() {
    assert_eq!(
        parse_expression("1+2*3"),
        Ok((
            "",
            Expression::Addition(
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                ))
            )
        ))
    );
}

#[test]
fn valid_input_div_before_add() {
    assert_eq!(
        parse_expression("1+2/3"),
        Ok((
            "",
            Expression::Addition(
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Division(
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                ))
            )
        ))
    );
}

#[test]
fn valid_input_mult_before_sub() {
    assert_eq!(
        parse_expression("1-2*3"),
        Ok((
            "",
            Expression::Subtraction(
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                ))
            )
        ))
    );
}

#[test]
fn valid_input_div_before_sub() {
    assert_eq!(
        parse_expression("1-2/3"),
        Ok((
            "",
            Expression::Subtraction(
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Division(
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                ))
            )
        ))
    );
}

#[test]
fn valid_input_paren_before_mult() {
    assert_eq!(
        parse_expression("(1+2)*3"),
        Ok((
            "",
            Expression::Multiplication(
                Box::new(Expression::Addition(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}

#[test]
fn valid_input_paren_before_div() {
    assert_eq!(
        parse_expression("(1+2)/3"),
        Ok((
            "",
            Expression::Division(
                Box::new(Expression::Addition(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}

#[test]
fn valid_input_mutliple_orders() {
    assert_eq!(
        parse_expression("(1+2)/3-4"),
        Ok((
            "",
            Expression::Subtraction(
                Box::new(Expression::Division(
                    Box::new(Expression::Addition(
                        Box::new(Expression::Constant(1)),
                        Box::new(Expression::Constant(2))
                    )),
                    Box::new(Expression::Constant(3))
                )),
                Box::new(Expression::Constant(4))
            )
        ))
    );
}

#[test]
fn valid_input_mult_div_ltr() {
    assert_eq!(
        parse_expression("1*2/3"),
        Ok((
            "",
            Expression::Division(
                Box::new(Expression::Multiplication(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}

#[test]
fn valid_input_add_sub_ltr() {
    assert_eq!(
        parse_expression("1+2-3"),
        Ok((
            "",
            Expression::Subtraction(
                Box::new(Expression::Addition(
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(2))
                )),
                Box::new(Expression::Constant(3))
            )
        ))
    );
}
