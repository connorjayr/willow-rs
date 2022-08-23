//! # LL(1) Grammar
//!
//! Our FOL parser is written using an [LL(1) grammar](https://en.wikipedia.org/wiki/LL_grammar) to
//! expedite parsing. The parser is implemented using the
//! [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator) library
//! [nom](https://crates.io/crates/nom). Due to the way the grammar is constructed, the parser
//! follows the following precedence:
//!
//! | Operation     | Precedence |
//! | ------------- | ---------: |
//! | Negation      |          1 |
//! | Conjunction   |          2 |
//! | Disjunction   |          3 |
//! | Implication   |          4 |
//! | Biconditional |          5 |
//!
//! Note that this grammar is parameterized by the choice of accepted operators, predicates, and
//! variables. Note that increasing the amount of overlap between the parameters will decrease the
//! total number of statements that can be accepted by the language.
//!
//! ```text
//! Start              -> contradiction
//!                     | tautology
//!                     | Iff.
//! Iff                -> Implies NullableIff.
//! NullableIff        -> iff Implies NullableIff
//!                     | .
//! Implies            -> Or NullableImplies.
//! NullableImplies    -> implies Or NullableImplies
//!                     | .
//! Or                 -> And NullableOr.
//! NullableOr         -> or And NullableOr
//!                     | .
//! And                -> Unary NullableAnd.
//! NullableAnd        -> and Unary NullableAnd
//!                     | .
//! Unary              -> not Unary
//!                     | forall Variable Unary
//!                     | exists Variable Unary
//!                     | ( Iff )
//!                     | Predicate.
//! Predicate          -> predicate TermTail
//!                     | Term symbol Term.
//! TermList           -> Term TermListTail.
//! TermListTail       -> comma Term TermListTail
//!                     | .
//! Term               -> Variable TermTail.
//! TermTail           -> ( TermList )
//!                     | .
//! Variable           -> var.
//! ```

use crate::logic::{Statement, Term};
use nom::{
    self,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, one_of},
    combinator::{all_consuming, opt, recognize},
    error::ParseError,
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

pub fn start(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, statement) = all_consuming(alt((contradiction, tautology, iff_expr)))(input)?;
    Ok((input, statement))
}

pub fn iff_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (statement1, statement2)) = pair(implies_expr, optional_iff)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some(statement2) => Ok((
            input,
            Box::new(Statement::Biconditional(statement1, statement2)),
        )),
    }
}

fn optional_iff(input: &str) -> IResult<&str, Option<Box<Statement>>> {
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

    let (input, (statement1, statement2)) = pair(implies_expr, optional_iff)(input)?;
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
    let (input, (statement1, statement2)) = pair(or_expr, optional_implies)(input)?;
    match statement2 {
        None => Ok((input, statement1)),
        Some(statement2) => Ok((
            input,
            Box::new(Statement::Conditional(statement1, statement2)),
        )),
    }
}

fn optional_implies(input: &str) -> IResult<&str, Option<Box<Statement>>> {
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

    let (input, (statement1, statement2)) = pair(or_expr, optional_implies)(input)?;
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
    let (input, (statement1, statement2)) = pair(and_expr, optional_or)(input)?;
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

fn optional_or(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∨")), ws(tag("|")), ws(tag("or")))))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(and_expr, optional_or)(input)?;
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
    let (input, (statement1, statement2)) = pair(unary_expr, optional_and)(input)?;
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

fn optional_and(input: &str) -> IResult<&str, Option<Box<Statement>>> {
    let (input, operator) = opt(alt((ws(tag("∧")), ws(tag("&")), ws(tag("and")))))(input)?;
    if operator.is_none() {
        return Ok((input, None));
    }

    let (input, (statement1, statement2)) = pair(unary_expr, optional_and)(input)?;
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
        alt((ws(tag("¬")), ws(tag("!")), ws(tag("~")), ws(tag("not")))),
        unary_expr,
    )(input)?;

    Ok((input, Box::new(Statement::Negation(inner_statement))))
}

fn universal_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (var, formula)) = preceded(
        alt((ws(tag("∀")), ws(tag("forall")))),
        pair(ws(variable), unary_expr),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Universal {
            var: var.to_string(),
            formula,
        }),
    ))
}

fn existence_expr(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (var, formula)) = preceded(
        alt((ws(tag("∃")), ws(tag("exists")))),
        pair(ws(variable), unary_expr),
    )(input)?;

    Ok((
        input,
        Box::new(Statement::Existential {
            var: var.to_string(),
            formula,
        }),
    ))
}

fn predicate(input: &str) -> IResult<&str, Box<Statement>> {
    alt((infix_predicate, prefix_predicate))(input)
}

fn infix_predicate(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, (term1, operator, term2)) = tuple((ws(term), ws(operator), ws(term)))(input)?;

    Ok((
        input,
        Box::new(Statement::Atom {
            predicate: operator.to_string(),
            args: vec![term1, term2],
        }),
    ))
}

fn prefix_predicate(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, predicate) = ws(predicate_name)(input)?;
    let (input, args) = opt(delimited(ws(char('(')), term_list, ws(char(')'))))(input)?;

    match args {
        Some(args) => Ok((
            input,
            Box::new(Statement::Atom {
                predicate: predicate.to_string(),
                args,
            }),
        )),
        None => Ok((
            input,
            Box::new(Statement::Atom {
                predicate: predicate.to_string(),
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

fn tautology(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, _) = ws(tag("⊤"))(input)?;
    Ok((input, Box::new(Statement::Tautology)))
}

fn contradiction(input: &str) -> IResult<&str, Box<Statement>> {
    let (input, _) = ws(tag("⊥"))(input)?;
    Ok((input, Box::new(Statement::Contradiction)))
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::{Error, ErrorKind};

    #[test]
    fn test_parser() {
        let statement: Statement = "   P and   Q  ".parse().unwrap();
        assert_eq!(
            statement,
            Statement::Conjunction(vec![
                Statement::Atom {
                    predicate: "P".to_string(),
                    args: vec![]
                },
                Statement::Atom {
                    predicate: "Q".to_string(),
                    args: vec![]
                }
            ])
        );

        let statement: Statement = "(A or B) implies (C and D)".parse().unwrap();
        assert_eq!(
            statement,
            Statement::Conditional(
                Box::new(Statement::Disjunction(vec![
                    Statement::Atom {
                        predicate: "A".to_string(),
                        args: vec![]
                    },
                    Statement::Atom {
                        predicate: "B".to_string(),
                        args: vec![]
                    }
                ])),
                Box::new(Statement::Conjunction(vec![
                    Statement::Atom {
                        predicate: "C".to_string(),
                        args: vec![]
                    },
                    Statement::Atom {
                        predicate: "D".to_string(),
                        args: vec![]
                    }
                ]))
            )
        );

        let statement: Statement = "F(x) or G(y)".parse().unwrap();
        assert_eq!(
            statement,
            Statement::Disjunction(vec![
                Statement::Atom {
                    predicate: "F".to_string(),
                    args: vec![Term::var("x")]
                },
                Statement::Atom {
                    predicate: "G".to_string(),
                    args: vec![Term::var("y")]
                }
            ])
        );

        let statement: Statement = "Node(x) and Node(parent(x))".parse().unwrap();
        assert_eq!(
            statement,
            Statement::Conjunction(vec![
                Statement::Atom {
                    predicate: "Node".to_string(),
                    args: vec![Term::var("x")]
                },
                Statement::Atom {
                    predicate: "Node".to_string(),
                    args: vec![Term::new("parent", vec![Term::var("x")])]
                }
            ])
        );

        let statement: Statement = "forall x forall y ( P(x) and P(y) )".parse().unwrap();
        assert_eq!(
            statement,
            Statement::Universal {
                var: "x".to_string(),
                formula: Box::new(Statement::Universal {
                    var: "y".to_string(),
                    formula: Box::new(Statement::Conjunction(vec![
                        Statement::Atom {
                            predicate: "P".to_string(),
                            args: vec![Term::var("x")]
                        },
                        Statement::Atom {
                            predicate: "P".to_string(),
                            args: vec![Term::var("y")]
                        }
                    ]))
                })
            }
        );

        let statement1: Statement = "forall x exists y x < y".parse().unwrap();
        assert_eq!(
            statement1,
            Statement::Universal {
                var: "x".to_string(),
                formula: Box::new(Statement::Existential {
                    var: "y".to_string(),
                    formula: Box::new(Statement::Atom {
                        predicate: "<".to_string(),
                        args: vec![Term::var("x"), Term::var("y")]
                    })
                })
            }
        );

        let statement2: Statement = "forall x exists y(x < y)".parse().unwrap();
        assert_eq!(statement1, statement2);

        // Expected errors
        let statement: Result<Statement, _> = "a or b".parse();
        assert_eq!(
            statement.unwrap_err(),
            Error::new("a or b".to_string(), ErrorKind::OneOf)
        );
    }
}
