use crate::logic::{Statement, Term};
use nom::{
    self,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, one_of},
    combinator::{opt, recognize},
    error::{Error, ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    Finish, IResult,
};

impl<'a> TryFrom<&'a str> for Statement<'a> {
    type Error = Error<&'a str>;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let parser_result = iff_expr(s);
        match parser_result.finish() {
            Err(err) => Err(err),
            Ok((leftover_text, statement)) => match leftover_text.len() {
                0 => Ok(*statement),
                _ => Err(Error::new(leftover_text, ErrorKind::Fail)),
            },
        }
    }
}

fn iff_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(implies_expr, nullable_iff)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some(statement2) => Ok((
            input,
            Box::new(Statement::Biconditional(statement1, statement2)),
        )),
    }
}

fn nullable_iff(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((
        ws(tag("↔")),
        ws(tag("<->")),
        ws(tag("%")),
        ws(tag("iff")),
        ws(tag("equiv")),
    )))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(implies_expr, nullable_iff)(input)?;
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

fn implies_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(or_expr, nullable_implies)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some(statement2) => Ok((
            input,
            Box::new(Statement::Conditional(statement1, statement2)),
        )),
    }
}

fn nullable_implies(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((
        ws(tag("→")),
        ws(tag("->")),
        ws(tag("$")),
        ws(tag("implies")),
        ws(tag("only if")),
    )))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(or_expr, nullable_implies)(input)?;
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

fn or_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(and_expr, nullable_or)(input)?;
    if statement2.is_none() {
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

fn nullable_or(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∨")), ws(tag("|")), ws(tag("or")))))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(and_expr, nullable_or)(input)?;
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

fn and_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(unary_expr, nullable_and)(input)?;
    if statement2.is_none() {
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

fn nullable_and(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∧")), ws(tag("&")), ws(tag("and")))))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(unary_expr, nullable_and)(input)?;
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

fn unary_expr(input: &str) -> IResult<&str, Box<Statement>> {
    alt((
        negation_expr,
        universal_expr,
        existence_expr,
        delimited(ws(char('(')), iff_expr, ws(char(')'))),
        ws(predicate),
    ))(input)
}

fn negation_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, inner_statement) = preceded(
        alt((
            ws(tag("¬")),
            ws(tag("!")),
            ws(tag("~")),
            ws(tag("Negation")),
        )),
        unary_expr,
    )(input)?;

    Ok((input, Box::new(Statement::Negation(inner_statement))))
}

fn universal_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (var, formula)) = preceded(
        alt((ws(tag("∀")), ws(tag("forall")))),
        pair(ws(variable), unary_expr),
    )(input)?;

    Ok((input, Box::new(Statement::Universal { var, formula })))
}

fn existence_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (var, formula)) = preceded(
        alt((ws(tag("∃")), ws(tag("exists")))),
        pair(ws(variable), unary_expr),
    )(input)?;

    Ok((input, Box::new(Statement::Existential { var, formula })))
}

fn predicate(input: &str) -> IResult<&str, Box<Statement>> {
    alt((infix_predicate, prefix_predicate))(input)
}

fn infix_predicate(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (term1, operator, term2)) = tuple((ws(term), ws(operator), ws(term)))(input)?;

    Ok((
        input,
        Box::new(Statement::Atom {
            predicate: operator,
            args: vec![term1, term2],
        }),
    ))
}

fn prefix_predicate(input: &str) -> IResult<&str, Box<Statement>> {
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

fn term_list(input: &str) -> IResult<&str, Vec<Term>> {
    separated_list1(char(','), ws(term))(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    let (input, name) = ws(variable)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), term_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((input, Term::new(name, args))),
        None => Ok((input, Term::new(name, vec![]))),
    }
}

fn predicate_name(input: &str) -> IResult<&str, &str> {
    alt((recognize(pair(uppercase_alpha, acceptable_char0)), operator))(input)
}

fn variable(input: &str) -> IResult<&str, &str> {
    recognize(pair(lowercase_alpha, acceptable_char0))(input)
}

fn acceptable_char0(input: &str) -> IResult<&str, &str> {
    recognize(many0(one_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_",
    )))(input)
}

fn operator(input: &str) -> IResult<&str, &str> {
    recognize(one_of("<>="))(input)
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

    let statement: Statement = "forall x forall y ( P(x) and P(y) )".try_into().unwrap();
    assert_eq!(
        statement.to_string(),
        Box::new(Statement::Universal {
            var: "x",
            formula: Box::new(Statement::Universal {
                var: "y",
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
        })
        .to_string()
    );

    let statement: Statement = "forall x exists y ( P(x) or P(y) or Q(x, y) and Q(y, x) )"
        .try_into()
        .unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Universal {
            var: "x",
            formula: Box::new(Statement::Existential {
                var: "y",
                formula: Box::new(Statement::Disjunction(vec![
                    Statement::Atom {
                        predicate: "P",
                        args: vec![Term::var("x")]
                    },
                    Statement::Atom {
                        predicate: "P",
                        args: vec![Term::var("y")]
                    },
                    Statement::Conjunction(vec![
                        Statement::Atom {
                            predicate: "Q",
                            args: vec![Term::var("x"), Term::var("y")]
                        },
                        Statement::Atom {
                            predicate: "Q",
                            args: vec![Term::var("y"), Term::var("x")]
                        }
                    ])
                ]))
            })
        }
        .to_string()
    );

    let statement: Statement = "forall n exists x (>(x, n) and <(x, succ(n)))"
        .try_into()
        .unwrap();
    assert_eq!(
        statement.to_string(),
        Statement::Universal {
            var: "n",
            formula: Box::new(Statement::Existential {
                var: "x",
                formula: Box::new(Statement::Conjunction(vec![
                    Statement::Atom {
                        predicate: ">",
                        args: vec![Term::var("x"), Term::var("n")]
                    },
                    Statement::Atom {
                        predicate: "<",
                        args: vec![Term::var("x"), Term::new("succ", vec![Term::var("n")])]
                    }
                ]))
            })
        }
        .to_string()
    );

    let statement1: Statement = "forall x exists y x < y".try_into().unwrap();
    assert_eq!(
        statement1.to_string(),
        Box::new(Statement::Universal {
            var: "x",
            formula: Box::new(Statement::Existential {
                var: "y",
                formula: Box::new(Statement::Atom {
                    predicate: "<",
                    args: vec![Term::var("x"), Term::var("y")]
                })
            })
        })
        .to_string()
    );

    let statement2: Statement = "forall x exists y(x < y)".try_into().unwrap();
    assert_eq!(statement1.to_string(), statement2.to_string());

    // Expected errors
    let statement: Result<Statement, _> = "a or b".try_into();
    assert_eq!(
        statement.unwrap_err(),
        Error::new("a or b", ErrorKind::OneOf)
    );
}
