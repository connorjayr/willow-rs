use crate::logic::*;
use nom::{
    self,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, char, multispace0, one_of},
    combinator::{opt, recognize},
    error::{Error, ErrorKind, ParseError},
    multi::separated_list1,
    sequence::{delimited, pair, preceded},
    Finish, IResult,
};

enum Operator {
    // And,
    Biconditional,
    Conditional,
    // Negation,
    // Or,
}

impl<'a> TryFrom<&'a str> for Statement<'a> {
    type Error = Error<&'a str>;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let parser_result = expression_generator(s);
        match parser_result.finish() {
            Err(err) => Err(err),
            Ok((leftover_text, statement)) => match leftover_text.len() {
                0 => Ok(*statement),
                _ => Err(Error::new(leftover_text, ErrorKind::Fail)),
            },
        }
    }
}

fn expression_generator(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(or_expression_generator, expression)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some((nested_operation, statement2)) => match nested_operation {
            Operator::Conditional => Ok((
                input,
                Box::new(Statement::Conditional(statement1, statement2)),
            )),
            Operator::Biconditional => Ok((
                input,
                Box::new(Statement::Biconditional(statement1, statement2)),
            )),
        },
    }
}

fn expression(input: &str) -> IResult<&str, Option<(Operator, Box<Statement>)>> {
    let (input, operator) = opt(alt((
        ws(tag("→")),
        ws(tag("->")),
        ws(tag("$")),
        ws(tag("implies")),
        ws(tag("only if")),
    )))(input)?;
    let (input, operation) = match operator {
        Some(_) => (input, Some(Operator::Conditional)),
        None => {
            let (input, operator) = opt(alt((
                ws(tag("↔")),
                ws(tag("<->")),
                ws(tag("%")),
                ws(tag("iff")),
                ws(tag("equiv")),
            )))(input)?;
            match operator {
                Some(_) => (input, Some(Operator::Biconditional)),
                None => (input, None),
            }
        }
    };
    if let None = operation {
        return Ok((input, None));
    }
    let operation = operation.unwrap();

    let (input, (statement1, statement2)) = pair(or_expression_generator, expression)(input)?;
    match statement2 {
        None => Ok((input, Some((operation, statement1)))),
        Some((nested_operation, statement2)) => match nested_operation {
            Operator::Conditional => Ok((
                input,
                Some((
                    operation,
                    Box::new(Statement::Conditional(statement1, statement2)),
                )),
            )),
            Operator::Biconditional => Ok((
                input,
                Some((
                    operation,
                    Box::new(Statement::Biconditional(statement1, statement2)),
                )),
            )),
        },
    }
}

fn or_expression_generator(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(and_expression_generator, or_expression)(input)?;
    if let None = statement2 {
        return Ok((input, statement1));
    }
    let statement2 = statement2.unwrap();
    // Auto-reduce commutative statements
    match *statement2 {
        Statement::Disjunction(mut operands) => {
            operands.insert(0, *statement1);
            Ok((input, Box::new(Statement::Disjunction(operands))))
        }
        _ => Ok((
            input,
            Box::new(Statement::Disjunction(vec![*statement1, *statement2])),
        )),
    }
}

fn or_expression(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∨")), ws(tag("|")), ws(tag("or")))))(input)?;
    if let None = operator {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(and_expression_generator, or_expression)(input)?;
    match statement2 {
        None => Ok((input, Some(statement1))),
        Some(statement2) => match *statement2 {
            // Auto-reduce commutative statements
            Statement::Disjunction(mut operands) => {
                operands.insert(0, *statement1);
                Ok((input, Some(Box::new(Statement::Disjunction(operands)))))
            }
            _ => Ok((
                input,
                Some(Box::new(Statement::Disjunction(vec![
                    *statement1,
                    *statement2,
                ]))),
            )),
        },
    }
}

fn and_expression_generator(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(unary_expression, and_expression)(input)?;
    if let None = statement2 {
        return Ok((input, statement1));
    }
    let statement2 = statement2.unwrap();
    // Auto-reduce commutative statements
    match *statement2 {
        Statement::Conjunction(mut operands) => {
            operands.insert(0, *statement1);
            Ok((input, Box::new(Statement::Conjunction(operands))))
        }
        _ => Ok((
            input,
            Box::new(Statement::Conjunction(vec![*statement1, *statement2])),
        )),
    }
}

fn and_expression(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∧")), ws(tag("&")), ws(tag("and")))))(input)?;
    if let None = operator {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(unary_expression, and_expression)(input)?;
    match statement2 {
        None => Ok((input, Some(statement1))),
        Some(statement2) => match *statement2 {
            // Auto-reduce commutative statements
            Statement::Conjunction(mut operands) => {
                operands.insert(0, *statement1);
                Ok((input, Some(Box::new(Statement::Conjunction(operands)))))
            }
            _ => Ok((
                input,
                Some(Box::new(Statement::Conjunction(vec![
                    *statement1,
                    *statement2,
                ]))),
            )),
        },
    }
}

fn unary_expression(input: &str) -> IResult<&str, Box<Statement>> {
    alt((
        negation_expression,
        universal_expression,
        existence_expression,
        delimited(ws(char('(')), expression_generator, ws(char(')'))),
        ws(predicate),
    ))(input)
}

fn negation_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, inner_statement) = preceded(
        alt((
            ws(tag("¬")),
            ws(tag("!")),
            ws(tag("~")),
            ws(tag("Negation")),
        )),
        unary_expression,
    )(input)?;

    Ok((input, Box::new(Statement::Negation(inner_statement))))
}

fn universal_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (vars, inner_statement)) = preceded(
        alt((ws(tag("∀")), ws(tag("forall")))),
        pair(constant_list, unary_expression),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Universal {
            vars,
            formula: inner_statement,
        }),
    ))
}

fn existence_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (vars, inner_statement)) = preceded(
        alt((ws(tag("∃")), ws(tag("exists")))),
        pair(constant_list, unary_expression),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Existential {
            vars,
            formula: inner_statement,
        }),
    ))
}

fn predicate(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, predicate) = ws(predicate_name)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), term_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((input, Box::new(Statement::Atom { predicate, args }))),
        None => Ok((
            input,
            Box::new(Statement::Atom {
                predicate,
                args: vec![],
            }),
        )),
    }
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, name) = ws(constant)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), term_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((input, Term::new(name, args))),
        None => Ok((input, Term::new(name, vec![]))),
    }
}

fn term_list(input: &str) -> IResult<&str, Vec<Term>> {
    separated_list1(char(','), ws(term))(input)
}

fn predicate_name(input: &str) -> IResult<&str, &str> {
    recognize(pair(uppercase_alpha, alphanumeric0))(input)
}

fn constant(input: &str) -> IResult<&str, &str> {
    recognize(pair(lowercase_alpha, alphanumeric0))(input)
}

fn constant_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(char(','), ws(constant))(input)
}

fn lowercase_alpha(input: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyz")(input)
}

fn uppercase_alpha(input: &str) -> IResult<&str, char> {
    one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[test]
fn test_parser() {
    let statement: Statement = "   P and   Q  ".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Conjunction(vec![
            Statement::Atom {
                predicate: "P",
                args: vec![]
            },
            Statement::Atom {
                predicate: "Q",
                args: vec![]
            }
        ])
        .to_string()
    );

    let statement: Statement = "(A or B) implies (C and D)".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Conditional(
            Box::new(Statement::Disjunction(vec![
                Statement::Atom {
                    predicate: "A",
                    args: vec![]
                },
                Statement::Atom {
                    predicate: "B",
                    args: vec![]
                }
            ])),
            Box::new(Statement::Conjunction(vec![
                Statement::Atom {
                    predicate: "C",
                    args: vec![]
                },
                Statement::Atom {
                    predicate: "D",
                    args: vec![]
                }
            ]))
        )
        .to_string()
    );

    let statement: Statement = "F(x) or G(y)".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Disjunction(vec![
            Statement::Atom {
                predicate: "F",
                args: vec![Term::var("x")]
            },
            Statement::Atom {
                predicate: "G",
                args: vec![Term::var("y")]
            }
        ])
        .to_string()
    );

    let statement: Statement = "Node(x) and Node(parent(x))".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Conjunction(vec![
            Statement::Atom {
                predicate: "Node",
                args: vec![Term::var("x")]
            },
            Statement::Atom {
                predicate: "Node",
                args: vec![Term::new("parent", vec![Term::var("x")])]
            }
        ])
        .to_string()
    );

    let statement: Statement = "forall x, y ( P(x) and P(y) )".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Box::new(Statement::Universal {
            vars: vec!["x", "y"],
            formula: Box::new(Statement::Conjunction(vec![
                Statement::Atom {
                    predicate: "P",
                    args: vec![Term::var("x")]
                },
                Statement::Atom {
                    predicate: "P",
                    args: vec![Term::var("y")]
                }
            ]))
        })
        .to_string()
    );

    let statement: Statement = "forall x exists y ( P(x) and P(y) and Q(x, y) )"
        .try_into()
        .unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Universal {
            vars: vec!["x"],
            formula: Box::new(Statement::Existential {
                vars: vec!["y"],
                formula: Box::new(Statement::Conjunction(vec![
                    Statement::Atom {
                        predicate: "P",
                        args: vec![Term::var("x")]
                    },
                    Statement::Atom {
                        predicate: "P",
                        args: vec![Term::var("y")]
                    },
                    Statement::Atom {
                        predicate: "Q",
                        args: vec![Term::var("x"), Term::var("y")]
                    }
                ]))
            })
        }
        .to_string()
    );

    let statement: Statement = "forall n exists x (GreaterThan(x, n) and LessThan(x, succ(n)))"
        .try_into()
        .unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Universal {
            vars: vec!["n"],
            formula: Box::new(Statement::Existential {
                vars: vec!["x"],
                formula: Box::new(Statement::Conjunction(vec![
                    Statement::Atom {
                        predicate: "GreaterThan",
                        args: vec![Term::var("x"), Term::var("n")]
                    },
                    Statement::Atom {
                        predicate: "LessThan",
                        args: vec![Term::var("x"), Term::new("succ", vec![Term::var("n")])]
                    }
                ]))
            })
        }
        .to_string()
    );

    // Expected errors
    let statement: Result<Statement, _> = "a or b".try_into();
    assert_eq!(
        statement.unwrap_err(),
        Error::new("a or b", ErrorKind::OneOf)
    );
}
