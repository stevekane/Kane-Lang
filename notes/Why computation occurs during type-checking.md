# Why computation occurs during type-checking

Checking the types for soundness in a dependently-typed language
may require that some terms be evaluated during the process. This means
that computation (the rules that define how eliminations and introductions
interact) may be performed.

We would like to see a small illustrative example of when this is needed.

We need a type to check that requires evaluation.
Let's annotate a function with a signature whose value is a computation.

    T : (π1 (Top * Bot))

We see that the type is a computation.
We can ask what the typechecking rules are for the first projection.

We see that we expect the body e in π1 e to be of type σ × τ.
We see that the body is indeed a pair whose type is Product.
We know the value of the first projection is Top therefore we can ask
T : Top and that is true.

    T : (π1 ((π1 (Top * Bot)) * Bot))

We see the body of π1 is a itself another π1 computation. In order to know
the type of the outer projection we need to know that the inner projection
is of type × and that we can access its first projection.

    π1 (Top * Bot) ↓ Top

    π1 (Top * Bot) ↓ Top

    T : Top

The key insight here is that the inputs to our inference in this case
were a term T and an annotation which was itself a computation. Thus,
we should just perform the computation once and return the result as
the output of the inference algorithm.

    Infer T : π1 (π1 (Top * Bot) * Bot) = Top

I wish I could think of something more convincing.