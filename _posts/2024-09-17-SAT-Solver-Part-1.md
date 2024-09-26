---
title: Toy SAT Solver (Part 1)
published: true
---

This is the first of hopefully many posts about the trials and tribulations I
face while writing a SAT solver in Rust. This post will cover lexing
strings in [conjunctive normal form](https://en.wikipedia.org/wiki/Conjunctive_normal_form),
while the next post will cover parsing.
We'll then use breadth-first search to write a very slow, very naive solver, before
introducing backtracking and the DPLL algorithm.

## What is SAT?

The boolean satisfiability problem, often known just as BSAT or SAT, essentially
asks, "given a boolean expression, is it possible for the expression to evaluate
to 'true', or will it always evaluate to 'false'?" Practically, the problem is
usually restricted to boolean expressions written in CNF, as any arbitrary
boolean expression can be reduced to CNF.

An expression in CNF looks something like this:

```none
(x1 OR x2) AND (x1 OR NOT x3) AND (x2 OR x3)
```

More formally, a context-free grammar for CNF in EBNF looks something like this:

```none
EXPR := CLAUSE ( "and" CLAUSE )*
CLAUSE := "(" TERM ( "or" TERM )* ")"
TERM := ( "not" )? LIT
LIT := "true" | "false" | [a-zA-Z0-9]+
```

For now, we'll only be accepting expressions that are already in CNF. Later
we'll be extending our parser to accept all kinds of boolean expressions and
write a pass to transform our AST into CNF, but for now we'll keep things simple.

## Representing Tokens

For the following few sections, I'll assume basic familiarity with lexing and
parsing. If
you are completely new to this, I highly recommend reading the first few chapters
of [Crafting Interpreters](https://craftinginterpreters.com/) to get a feel
for these concepts before coming back to this post.

We'll be using recursive descent to parse our strings, so translating our grammar
into code should be pretty straightforward. First, though, we need to lex our
string into some tokens. Fortunately, those, too, are pretty straightforward:
the only operators we expect to see are AND and OR, and any other string will be
treated as a variable name. Besides those, we'll also see parentheses, which
we'll need to check for proper pairing.

Here's our enum representing the different kinds of tokens we'll see:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Dummy,
    Paren(Open),
    Op(OpKind),
    Lit(LitKind),
}
```

We'll have a `Token` struct wrapping around `TokenKind`s which will carry
additional information about tokens, such as their positions within the string,
which will be helpful for error reporting:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub start: usize,
    pub end: usize,
    pub kind: TokenKind,
}
```

Some `TokenKind`s have their own variants, such as boolean operators, which we
represent with the following enums:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Open {
    Open,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpKind {
    And,
    Or,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Var(Ident),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(String);
```

Note that `Ident`s derive `Hash`--this is because we want to keep track of all
unique variables in someting like a hash table for the DPLL algorithm, which
substitutes boolean values into variables.

We'll also have some convenience methods for creating tokens:

```rust
impl Token {
    pub fn from_str(value: &str, start: usize, end: usize) -> Self {
        match value.trim().to_lowercase().as_str() {
            "and" | "or" | "not" => Self::new_op(OpKind::from(value), start, end),
            _ => Self::new_lit(LitKind::from(value), start, end),
        }
    }

    pub fn new_dummy() -> Self {
        Self {
            start: 0,
            end: 0,
            kind: TokenKind::Dummy,
        }
    }

    pub fn new_paren(kind: Open, start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            kind: TokenKind::Paren(kind),
        }
    }

    pub fn new_op(kind: OpKind, start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            kind: TokenKind::Op(kind),
        }
    }

    pub fn new_lit(kind: LitKind, start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            kind: TokenKind::Lit(kind),
        }
    }
}
```

Note that we use conversion methods like `From::from` in the methods above.
These are implemented as follows:

```rust
impl<'a> From<&'a str> for OpKind {
    fn from(value: &'a str) -> Self {
        match value.trim().to_lowercase().as_str() {
            "and" => Self::And,
            "or" => Self::Or,
            "not" => Self::Not,
            _ => panic!("uncaught invalid operand {value}; we should've caught this earlier!"),
        }
    }
}
```

The panic in `OpKind`'s `From` impl is intentional, since
arbitrary strings should generally be treated as variable names, so we shouldn't
be passing them to the `From` impl.

```rust
impl<'a> From<&'a str> for LitKind {
    fn from(value: &'a str) -> Self {
        match value.trim().to_lowercase().as_str() {
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            s => Self::Var(s.into()),
        }
    }
}
```

As stated previously, arbitrary strings are just treated as variables, which are
created when we call `s.into()`, converting our `&str` into an `Ident`. Although
`Into<str>` isn't implemented explicitly for `Ident`, Rust's `core` provides a
blanket impl for `Into<T> for U` for all types `T` that impl `From<U>`. In this
case, since we have `impl From<str> for Ident`, `core`'s impl "automatically
generates" `impl Into<Ident> for str` for us.

```rust
impl<'a> From<&'a str> for Ident {
    fn from(value: &'a str) -> Self {
        Self(value.to_string())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
```

## Lexing

Now that we have our tokens, we have to write something to actually generate
them: a lexer. Our lexer will need to have access to the source text, a way to
keep track of its current position within the source text, some way to keep
track of unclosed parentheses, and a place to store tokens. The struct looks
something like this:

```rust
pub struct Lexer<'src> {
    src_str: &'src str,
    src: Vec<char>,
    start: usize,
    cur: usize,
    tokens: VecDeque<Token>,
    parens: i64,
}
```

Note that we have both a reference to the source string *and* a `Vec` of `char`s.
This is because Rust's strings don't allow indexing, since under the hood they
use UTF-8 to represent strings, and indexing like C++ does it indexes the
underlying bytes of the string. This risks indexing between character boundaries
for non-ASCII text, which would be...really bad. Therefore, we have to use the
`Chars` iterator to index along code points:

```rust
impl<'src> Lexer<'src> {
    pub fn new(src_str: &'src str) -> Self {
        let src = src_str.trim().chars().collect();
        Self {
            src_str,
            src,
            start: 0,
            cur: 0,
            tokens: VecDeque::new(),
            parens: 0,
        }
    }
}
```

We could assume we'll always only deal with ASCII and use `as_bytes()` to speed
things up a little, especially since `collect()` is guaranteed to allocate, but
compared to how slow things will eventually get with our first solver
implementation and how small of a role this one line plays in the grand scheme
of the program, we'll leave it alone for now, especially since we haven't done
any profiling yet.

We use a `VecDeque` to store tokens because we want some kind of queue the lexer
can push tokens into, which the parser will eventually take ownership of and pop
tokens out of. We want both push and pop operations to be efficient, so we'll
use a `VecDeque`.

Next, we'll define our entry point for lexing:

```rust
// `impl Lexer`...
    pub fn lex(mut self) -> Result<VecDeque<Token>> {
        self.lex_()?;
        if self.parens != 0 {
            return Err(anyhow!("{} unclosed parens", self.parens));
        }
        Ok(self.finish())
    }

    fn finish(self) -> VecDeque<Token> {
        self.tokens
    }
```

We don't want to be able to reuse a lexer instance, so the `lex` method consumes
`self`. We'll use [anyhow](https://docs.rs/anyhow/latest/anyhow/) for error reporting
throughout the program.

Now comes the main loop:

```rust
// In `impl Lexer`
    fn lex_(&mut self) -> Result<()> {
        let mut n = 0;
        while n < self.src.len() {
            let c = self.src.get(n).unwrap();
            self.start = n;
            self.cur = self.start;
            match c {
                '(' => {
                    self.parens += 1;
                    self.tokens
                        .push_back(Token::new_paren(Open::Open, self.start, self.cur))
                }
                ')' => {
                    self.parens -= 1;
                    self.tokens
                        .push_back(Token::new_paren(Open::Closed, self.start, self.cur))
                }
                _ if c.is_alphanumeric() => {
                    let tok = self.lex_str()?;
                    self.tokens.push_back(tok);
                }
                _ if c.is_whitespace() => (),
                _ => return Err(anyhow!("unexpected char {c}")),
            }
            n += self.cur - self.start + 1;
        }
        Ok(())
    }
```

Handling single-character tokens, like parens and whitespace, is pretty
straightforward: whenever we encounter one, we just push it to the token queue,
keeping track of open and closed parens. Lexing strings is slightly more
involved: we only want to push a string to the queue when we hit whitespace
--i.e., the end of a string. Additionally, we want to disambiguate between
boolean literals, variables, and operators, all of which are alphanumeric strings.
This is where the conversion methods we defined earlier on our `Token`s come in
really handy.

Also notice how we don't use a `for` loop to iterate over `self.src`. This is because
doing so would either borrow `self.src` or move it out of `self`, both of which
would cause borrow checker errors. The former case is more interesting: although
we don't mutably access `self.src` anywhere, the borrow checker has no way of
knowing this when we call something like `self.lex_str()` within the loop, which
borrows `self` mutably. The
lifetime of the mutable borrow on `self` due to `lex_str()` overlaps with the
lifetime of the immutable borrow on `self.src`, causing an error, even though
we, the programmer, know that we don't actually modify `self.src`. I've seen
[a few proposed changes](https://smallcultfollowing.com/babysteps/blog/2024/06/02/the-borrow-checker-within/)
to the borrow checker that would make it more flexible, possibly accepting cases
such as ours, but nothing concrete exists yet that I know of.

Anyway, here's how we lex strings:

```rust
    fn lex_str(&mut self) -> Result<Token> {
        loop {
            self.bump();

            if self.cur >= self.src.len()
                || self.src[self.cur].is_whitespace()
                || matches!(self.src[self.cur], '(' | ')')
            {
                self.cur -= 1; // Don't skip over the closing paren
                return Ok(Token::from_str(
                    &self.src_str[self.start..=self.cur],
                    self.start,
                    self.cur,
                ));
            }

            if !self.src[self.cur].is_alphanumeric() {
                return Err(anyhow!(
                    "unexpected char {} when lexing string",
                    self.src[self.cur]
                ));
            }
        }
    }

    fn bump(&mut self) {
        self.cur += 1;
    }
```

As you can see, we treat the end of a string as being either whitespace or a
parenthesis. We decrement `self.cur` before returning the token because not
doing so would cause us to skip over the closing paren in the case of something
like `(foo)` when we return to the main loop and evaluate
`n += self.cur - self.start + 1`, causing all sorts of horrible errors. We also
don't accept non-alphanumeric variable names.

With that, our lexer is done. Let's see how it handles some simple cases:

```rust
fn main() -> Result<()> {
    println!("{:?}", run("foo bar baz"));
    println!("{:?}", run("(a and not a)"));
    println!("{:?}", run(":("));
    Ok(())
}

fn run(expr: &str) -> Result<()> {
    let tokens = Lexer::new(expr).lex()?;
    println!("{tokens:?}");
    Ok(())
}
```

Running `cargo run` gives us the following output:

```none
[Token { start: 0, end: 2, kind: Lit(Var(Ident("foo"))) }, Token { start: 4, end: 6, kind: Lit(Var(Ident("bar"))) }, Token { start: 8, end: 10, kind: Lit(Var(Ident("baz"))) }]
Ok(())
[Token { start: 0, end: 0, kind: Paren(Open) }, Token { start: 1, end: 1, kind: Lit(Var(Ident("a"))) }, Token { start: 3, end: 5, kind: Op(And) }, Token { start: 7, end: 9, kind: Op(Not) }, Token { start: 11, end: 11, kind: Lit(Var(Ident("a"))) }, Token { start: 12, end: 12, kind: Paren(Closed) }]
Ok(())
Err(unexpected char :)
```

Not bad!

## Conclusion

In this post, we hacked together a simple lexer for CNF expressions. In the next
post, we'll write a parser that accepts these tokens and generates an AST
representing CNF expressions, which we'll use to determine satisfiability.
