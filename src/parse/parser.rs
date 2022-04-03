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

pub fn parse(text: &str) -> Result<Box<Statement>, Error<&str>> {
    let parser_result = expression_generator(text);
    match parser_result.finish() {
        Err(err) => Err(err),
        Ok((leftover_text, statement)) => match leftover_text.len() {
            0 => Ok(statement),
            _ => Err(Error::new(leftover_text, ErrorKind::Fail)),
        },
    }
}

fn expression_generator(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(or_expression_generator, expression)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some((nested_operation, statement2)) => match nested_operation {
            Operator::Conditional => Ok((
                input,
                Box::new(Statement::Conditional {
                    lhs: statement1,
                    rhs: statement2,
                }),
            )),
            Operator::Biconditional => Ok((
                input,
                Box::new(Statement::Biconditional {
                    lhs: statement1,
                    rhs: statement2,
                }),
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
                    Box::new(Statement::Conditional {
                        lhs: statement1,
                        rhs: statement2,
                    }),
                )),
            )),
            Operator::Biconditional => Ok((
                input,
                Some((
                    operation,
                    Box::new(Statement::Biconditional {
                        lhs: statement1,
                        rhs: statement2,
                    }),
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
        Statement::Disjunction { mut operands } => {
            operands.push(statement1);
            Ok((input, Box::new(Statement::Disjunction { operands })))
        }
        _ => Ok((
            input,
            Box::new(Statement::Disjunction {
                operands: vec![statement1, statement2],
            }),
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
            Statement::Disjunction { mut operands } => {
                operands.push(statement1);
                Ok((input, Some(Box::new(Statement::Disjunction { operands }))))
            }
            _ => Ok((
                input,
                Some(Box::new(Statement::Disjunction {
                    operands: vec![statement1, statement2],
                })),
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
        Statement::Conjunction { mut operands } => {
            operands.push(statement1);
            Ok((input, Box::new(Statement::Conjunction { operands })))
        }
        _ => Ok((
            input,
            Box::new(Statement::Conjunction {
                operands: vec![statement1, statement2],
            }),
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
            Statement::Conjunction { mut operands } => {
                operands.push(statement1);
                Ok((input, Some(Box::new(Statement::Conjunction { operands }))))
            }
            _ => Ok((
                input,
                Some(Box::new(Statement::Conjunction {
                    operands: vec![statement1, statement2],
                })),
            )),
        },
    }
}

fn unary_expression(input: &str) -> IResult<&str, Box<Statement>> {
    alt((
        Negation_expression,
        universal_expression,
        existence_expression,
        delimited(ws(char('(')), expression_generator, ws(char(')'))),
        ws(predicate),
    ))(input)
}

fn Negation_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, inner_statement) = preceded(
        alt((
            ws(tag("¬")),
            ws(tag("!")),
            ws(tag("~")),
            ws(tag("Negation")),
        )),
        unary_expression,
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Negation {
            operand: inner_statement,
        }),
    ))
}

fn universal_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (variables, inner_statement)) = preceded(
        alt((ws(tag("∀")), ws(tag("forall")))),
        pair(object_list, unary_expression),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Universal {
            variables,
            proposition: inner_statement,
        }),
    ))
}

fn existence_expression(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (variables, inner_statement)) = preceded(
        alt((ws(tag("∃")), ws(tag("exists")))),
        pair(object_list, unary_expression),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Existential {
            variables,
            proposition: inner_statement,
        }),
    ))
}

fn predicate(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, predicate) = ws(predicate_name)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), object_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((
            input,
            Box::new(Statement::Atom {
                predicate: String::from(predicate),
                args,
            }),
        )),
        None => Ok((
            input,
            Box::new(Statement::Atom {
                predicate: String::from(predicate),
                args: vec![],
            }),
        )),
    }
}

fn object(input: &str) -> IResult<&str, Object> {
    let (input, name) = ws(function_symbol)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), object_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((input, Object::new(String::from(name), args))),
        None => Ok((input, Object::new(String::from(name), vec![]))),
    }
}

fn object_list(input: &str) -> IResult<&str, Vec<Object>> {
    separated_list1(char(','), ws(object))(input)
}

fn predicate_name(input: &str) -> IResult<&str, &str> {
    recognize(pair(uppercase_alpha, alphanumeric0))(input)
}

fn function_symbol(input: &str) -> IResult<&str, &str> {
    recognize(pair(lowercase_alpha, alphanumeric0))(input)
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
    assert_eq!(
        parse("   P and   Q  ").unwrap().to_string(),
        Box::new(Statement::Conjunction {
            operands: vec![
                Box::new(Statement::Atom {
                    predicate: String::from("P"),
                    args: vec![]
                }),
                Box::new(Statement::Atom {
                    predicate: String::from("Q"),
                    args: vec![]
                })
            ]
        })
        .to_string()
    );
    assert_eq!(
        parse("(A or B) implies (C and D)").unwrap().to_string(),
        Box::new(Statement::Conditional {
            lhs: Box::new(Statement::Disjunction {
                operands: vec![
                    Box::new(Statement::Atom {
                        predicate: String::from("A"),
                        args: vec![]
                    }),
                    Box::new(Statement::Atom {
                        predicate: String::from("B"),
                        args: vec![]
                    })
                ]
            }),
            rhs: Box::new(Statement::Conjunction {
                operands: vec![
                    Box::new(Statement::Atom {
                        predicate: String::from("C"),
                        args: vec![]
                    }),
                    Box::new(Statement::Atom {
                        predicate: String::from("D"),
                        args: vec![]
                    })
                ]
            })
        })
        .to_string()
    );
    assert_eq!(
        parse("F(x) or G(y)").unwrap().to_string(),
        Box::new(Statement::Disjunction {
            operands: vec![
                Box::new(Statement::Atom {
                    predicate: String::from("F"),
                    args: vec![Object::new(String::from("x"), vec![])]
                }),
                Box::new(Statement::Atom {
                    predicate: String::from("G"),
                    args: vec![Object::new(String::from("y"), vec![])]
                })
            ]
        })
        .to_string()
    );
    assert_eq!(
        parse("Node(x) and Node(parent(x))").unwrap().to_string(),
        Box::new(Statement::Conjunction {
            operands: vec![
                Box::new(Statement::Atom {
                    predicate: String::from("Node"),
                    args: vec![Object::new(String::from("x"), vec![])]
                }),
                Box::new(Statement::Atom {
                    predicate: String::from("Node"),
                    args: vec![Object::new(
                        String::from("parent"),
                        vec![Object::new(String::from("x"), vec![])]
                    )]
                })
            ]
        })
        .to_string()
    );
    assert_eq!(
        parse("forall x, y ( P(x) and P(y) )").unwrap().to_string(),
        Box::new(Statement::Universal {
            variables: vec![
                Object::new(String::from("x"), vec![]),
                Object::new(String::from("y"), vec![])
            ],
            proposition: Box::new(Statement::Conjunction {
                operands: vec![
                    Box::new(Statement::Atom {
                        predicate: String::from("P"),
                        args: vec![Object::new(String::from("x"), vec![])]
                    }),
                    Box::new(Statement::Atom {
                        predicate: String::from("P"),
                        args: vec![Object::new(String::from("y"), vec![])]
                    })
                ]
            })
        })
        .to_string()
    );
    println!("{}", parse("forall x, y ( P(x) and P(y) )").unwrap());
}
