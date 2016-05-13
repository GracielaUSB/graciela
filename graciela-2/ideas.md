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
