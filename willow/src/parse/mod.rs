//!
//!
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
//!                     | ( Binary )
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

mod parser;
