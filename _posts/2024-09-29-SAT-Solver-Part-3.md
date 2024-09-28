---
title: Toy SAT Solver (Part 3)
published: true
---

This is the third post in my SAT Solver blog series. Last time, we defined the
AST and wrote a parser for CNF expressions. Today, we'll be laying the
groundwork for actually solving SAT expressions by defining simplifications and
an evaluator over the AST.

## How SAT Solvers Work

The basic principle behind SAT solvers is pretty simple: brute-force substitute
boolean values into variables until you get a combination of true and false
values that causes the entire expression to evaluate to true. For example, each
variable in the expression `(x1 or x2) and (not x3)` can be either `true` or
`false`. We can first try substituting `x1 = true`, `x2 = true`, and
`x3 = true`, but that causes our expression to evaluate to `false`, so we
continue searching and try `(x1, x2, x3) = (true, true, false)`, which turns out
to be the solution to our expression. If that hadn't been a solution either, we
would have had to keep searching all combinations of `true` and `false` for all
variables in our expression until we found a combination that satisfied the
expression, or until we had searched all combinations and could therefore
conclude that the expression is unsatisfiable. Modern SAT solvers still operate
on this same principle, but they deploy heuristics to avoid or defer evaluating
substitutions that would likely lead to falsehoods.

Notice that this series of substitutions creates a binary tree: for each variable,
you can either substitute true or false, each substitution creating an edge from
the root node--the original expression--to the new expression with `true` or `false`
substituted in place of the variable we selected. This leads to the natural
conclusion that we should employ some basic tree search algorithms--BFS, for instance,
which seems useful when we want to search for a specific node in a tree. We'll do
exactly that for our first attempt before showing why this is, in reality, a
terrible, *terrible* idea.

## Evaluation and Simplification

Before we start searching, though, we need a way to evaluate an expression to
figure out if it's `true` or `false`. In fact, we'll actually have a third
state, `Unknown`, which happens when we evaluate an expression that hasn't been
fully substituted yet, like `(x1 or true) and (not x3)`. Since we don't know
what values `x1` and `x3` have, we can't say what the expression evaluates to.
We'll represent these three states with an `enum`:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Solvable {
    Yes,
    No,
    Unknown,
}
```

We also want to be able to simplify expressions. For instance, in our previous
example, `(x1 or true) and (not x3)` would just simplify to
`(true) and (not x3)`, which would in turn simplify to `(not x3)`.

## The Visitor Pattern

To be able to do either of these things, we need some way to easily traverse our
AST. To do this, we'll call on our trusty friend, the visitor pattern. We'll
first define two visitor traits, one that takes the AST by shared
reference--useful for when we just want to traverse it without modifying it--and
one that takes the AST by mutable reference--useful for when we want to actually
modify the AST.

```rust
pub trait AstVisitor { /* ... */ }
pub trait AstMutVisitor { /* ... */ }
```

What methods should we define on these interfaces? We'll definitely need methods
like `visit_expr()` and `visit_clause()`, but how do we want to handle our
recursion? How much control do we want implementors to have over the
recursion--when to stop, for instance, or in what order (postorder, inorder, or
preorder)?

Our approach
will separate the recursion from the `visit_*()` methods. We'll provide default
implementations of our `visit_*()` methods which call methods called
`super_*()`. These `super_*()` methods recurse one level deeper into the tree.
This is how we'll be able to have fine-grained control over recursion: in our
own implementations of the `visit_*()` methods, we can have our visiting logic,
then either before or after we can call `super_*()` to recurse one level deeper
before returning to possibly finish up the logic. We can also omit the call to
`super_*()` to stop recursing at that level. This is what our `AstVisitor` will
look like:

```rust
pub trait AstVisitor {
    fn visit(&mut self, ast: &Ast) {
        self.visit_expr(&ast.root);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.super_expr(expr);
    }

    fn super_expr(&mut self, expr: &Expr) {
        for clause in &expr.clauses {
            self.visit_clause(clause);
        }
    }

    fn visit_clause(&mut self, clause: &Clause) {
        self.super_clause(clause);
    }

    fn super_clause(&mut self, clause: &Clause) {
        for operand in &clause.operands {
            self.visit_unary(operand);
        }
    }

    fn visit_unary(&mut self, unary: &Unary) {
        self.super_unary(unary);
    }

    fn super_unary(&mut self, unary: &Unary) {
        let (Unary::Just(term) | Unary::Not(term)) = unary;
        self.visit_term(term);
    }

    fn visit_term(&mut self, term: &Term) {
        self.super_term(term);
    }

    fn super_term(&mut self, term: &Term) {
        match term {
            Term::Bool(b) => self.visit_bool(b),
            Term::Var(var) => self.visit_var(var),
        }
    }

    fn visit_bool(&mut self, _b: &bool) {}

    fn visit_var(&mut self, _var: &Ident) {}
}
```

For the sake of brevity, I've omitted `AstMutVisitor`, which looks much the
same, except it accepts mutable references to the AST and its nodes rather than
the shared references accepted by `AstVisitor`s.

## Evaluation and Simplification (cont.)

Now that we have our `Ast(Mut)?Visitor` interfaces, it'd be a shame not to use
them. We'll first define our AST simplification pass, then move onto evaluation,
as doing it in that order will make evaluation much simpler.

Our first simplification pass will simplify logical operations containing boolean
literals. Specifically, it will transform `x or true` into `true`, `x or false`
into `x`, `x and true` into `x`, and `x and false` into `false`, following the
definitions of the respective operators. Additionally, it will simplify `not true`
into `false` and `not false` into `true`. We'll call this pass `InstSimplify`.
Since it will modify the AST, we'll use an `AstMutVisitor`.

```rust
struct InstSimplify;

impl AstMutVisitor for InstSimplify {
    fn visit_clause(&mut self, clause: &mut Clause) {
        self.super_clause(clause);
        if clause
            .operands
            .iter()
            .any(|operand| matches!(operand, Unary::Just(Term::Bool(true))))
        {
            // true OR x = true
            clause.operands = vec![Unary::Just(Term::Bool(true))];
        } else {
            // false OR x = x
            clause
                .operands
                .retain(|operand| !matches!(operand, Unary::Just(Term::Bool(false))));
        }
    }

    fn visit_unary(&mut self, unary: &mut Unary) {
        self.super_unary(unary);
        match unary {
            Unary::Not(Term::Bool(val)) => *unary = Unary::Just(Term::Bool(!*val)),
            Unary::Just(_) | Unary::Not(_) => (),
        }
    }
}
```

As you can see, the logic is pretty straightforward. We walk the AST and look for
subexpressions that match the patterns we're looking to simplify. We start from
the "smallest" subexpressions: unary expressions, since simplifying those before
simplifying clauses will mean less logic and fewer cases to consider when we
simplify clauses. For instance, `Not(true)` and `Just(false)` mean the same
thing, and `visit_unary()` will simplify them to the same thing, but if we
simplified clauses before simplifying unary expressions, we'd need logic
to check for both cases. But since we simplify unary expressions first, we only
have to handle the "canonicalized" case.

Additionally, we "extend" the simplification beyond just binary operators,
simultaneously making our logic simpler: notice that something like
`true or x or y` simplifies to `true or y`, which then simplifies to just `true`.
The `true` value "propagates," meaning that if there's one `true` in a clause,
the entire clause must be `true`. Similarly, all `false` values just "disappear"
from logical `or` expressions, so we just `filter()` them out. That means if we
have a clause like `(false)`, it becomes an empty clause, a property we'll use
during evaluation. Indeed, our grammar disallows empty clauses, so the only way
we could have an empty clause in our AST is if it evaluates to false.

Although we could just leave things here and move onto evaluation, in the future
we'd like to be able to easily plug in and test new simplification passes. To do
that, we'll have a central `Simplify` struct that stores and runs all simplification
passes. To do this, all of our passes will need to have a common interface through
which we can poll if they are enabled, and if they are, run them. We'll call this
trait `AstPass`:

```rust
pub trait AstPass {
    fn should_run(&self) -> bool;
    fn run(&mut self, expr: &mut Expr);
}
```

`impl`s of this trait will look pretty simple--for instance, here's the `impl`
for `InstSimplify`:

```rust
impl AstPass for InstSimplify {
    fn should_run(&self) -> bool {
        true
    }

    fn run(&mut self, expr: &mut Expr) {
        self.visit_expr(expr);
    }
}
```

Our main `Simplify` struct will then use dynamic dispatch to keep track of and
run our passes:

```rust
pub struct Simplify {
    passes: Vec<Box<dyn AstPass>>,
}

impl Simplify {
    pub fn new() -> Self {
        Self {
            passes: vec![Box::new(InstSimplify)],
        }
    }

    pub fn simplify(&mut self, expr: &mut Expr) {
        for p in self.passes.iter_mut() {
            if p.should_run() {
                p.run(expr);
            }
        }
    }
}
```

Right now, we only have one pass, but we'll add more later. Generally, when
adding passes, we want to keep `InstSimplify` toward the end of the pipeline,
since earlier passes will create boolean literals that `InstSimplify` can then
simplify away.

Now let's move onto evaluating expressions.

Evaluation will always come after simplification, so we can take advantage of
some assumptions about simplifications done on our AST that'll make our
evaluation logic much simpler. Remember the `is_true()` and `is_false()` methods
from when we defined our AST?

```rust
// impl Clause
    pub fn is_true(&self) -> bool {
        // We can't just do !self.is_false() because a clause may still contain
        // unevaluated variables, in which case it is neither true nor false.
        !self.operands.is_empty()
            && (self.operands == vec![Unary::Just(Term::Bool(true))]
                || self
                    .operands
                    .iter()
                    .all(|operand| *operand == Unary::Just(Term::Bool(true))))
    }

    pub fn is_false(&self) -> bool {
        // Our grammar disallows empty clauses in the input string, so we know all empty clauses
        // are the result of `false` being filtered out.
        self.operands.is_empty()
            || self.operands == vec![Unary::Just(Term::Bool(false))]
            || self
                .operands
                .iter()
                .all(|operand| *operand == Unary::Just(Term::Bool(false)))
    }
```

`InstSimplify` creates empty clauses if all the terms come out to be `false`,
which we take advantage of in these methods. Notice how clauses that contain
variables are neither `true` nor `false`--that is, both `is_true()` and
`is_false()` return `false` due to the respective lines
`all(|operand| *operand == Unary::Just(Term::Bool(true))))` and
`all(|operand| *operand == Unary::Just(Term::Bool(false)))`. Thanks to our
simplification passes and these two convenience methods, our evaluator won't
even have to recurse--it'll just walk the top level expression and check a) if
any clause evaluates to false, in which case `false and x = false` and the whole
expression therefore evaluates to false, and b) if all clauses evaluate to
`true`, since `true and x = true` if and only if `x = true`. Our evaluator will
then return the result of evaluation: whether the expression evaluates to `true`
or `false`, or whether there are still unsubstituted variables and the
expression's value is therefore still `Unknown`.

```rust
struct Eval {
    solvable: Solvable,
}

impl Eval {
    pub fn new() -> Self {
        Self {
            solvable: Solvable::Unknown,
        }
    }

    pub fn eval(mut self, expr: &Expr) -> Solvable {
        self.visit_expr(expr);
        self.solvable
    }
}

impl AstVisitor for Eval {
    fn visit_expr(&mut self, expr: &Expr) {
        if expr.clauses.iter().any(|clause| clause.is_false()) {
            // false AND x = false
            self.solvable = Solvable::No;
        } else if expr.clauses.iter().all(|clause| clause.is_true()) {
            self.solvable = Solvable::Yes;
        }
    }
}
```

Surprisingly simple, isn't it? Thanks to all the work we did before, evaluation
is actually some of the easiest code we'll have to write, which is hardly ever
the case.

## Conclusion

Today, we leveraged the visitor pattern to simplify and evaluate CNF expressions.
Although I originally planned to also cover our first attempt at solving SAT today,
this post is already getting a little too long, so I'll save that for next time,
where we'll finally write the
BFS-based solver I alluded to earlier in this post before exploring its many drawbacks.
