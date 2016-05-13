# Random ideas for GraCieLa-2

- Count quantifier ([Dijkstra](https://www.cs.utexas.edu/users/EWD/ewd07xx/EWD737.PDF))

counts how many times a boolean expression is true in the given range.

Example:

> (% count x : int | 0 <= x /\ x <= 100 | x mod 7 %) == 14

It can be seen that both the universal and existential quantifiers can be
defined in terms of the count quantifier, as follows:

> (% forall x : <type> | <range> | P(x) %) ===
>       (% count x : <type> | <range> | !P(x) %) == 0

and

> (% exist x : <type> | <range> | P(x) %) ===
>       (% count x : <type> | <range> | !P(x) %) >= 1

# Checked source files:

- [ ] Aborts.hs
- [ ] AST.hs
- [ ] ASTtype.hs
- [ ] Codegen.hs
- [ ] CodegenState.hs
- [ ] Contents.hs
- [ ] Declarations.hs
- [ ] Expression.hs
- [ ] Lexer.hs
- [x] Limits.hs
- [ ] Location.hs
- [x] Main.hs
- [ ] MyParseError.hs
- [ ] MyTypeError.hs
- [ ] ParserError.hs
- [ ] Parser.hs
- [ ] ParserState.hs
- [ ] ParserType.hs
- [ ] ReduceAST.hs
- [ ] State.hs
- [ ] SymbolTable.hs
- [x] Token.hs
- [ ] TokenParser.hs
- [x] Type.hs
- [ ] TypeState.hs
- [ ] VerTypes.hs
