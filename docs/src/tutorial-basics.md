# Basics

The essential structures in Argus are `Pattern`s, `CommentPattern`s,
`SyntaxClass`es and `Template`s.

`Pattern`s and `CommentPattern`s are the syntactic constructs used for
matching Julia code. `SyntaxClass`es are abstractions over patterns
that represent syntactic categories. And `Template`s are used for
generating new code using pattern variables bound during pattern
matching.
