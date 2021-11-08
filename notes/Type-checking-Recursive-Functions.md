# On Type-checking recursive functions

In general, there are two approaches to implementing recursive functions.
The first option is to allow self-reference in the body of a function
by forward-declaring a unique name for the function. The other option is
to provide a recursive binder that supplies a name for the function.

## Self-reference with forward declaration

This style is a little strange in a DeBruijn index setting because it uses
a user-defined name which of course could globally-conflict with other user-
defined names elsewhere in the context. The advantage of this style is that
the name of the function is stored as a free variable in the context and
therefore it may also be used to define other functions or in general be used
elsewhere in the code.

    factorial : N → N := [const (S Z) | λ.(S 0) * factorial 0]

## Fixpoint binder

    μ.[const (S Z) | λ.(S 0) * f 0]

## The "third" option

You could also use a standard let-in syntax with DeBruijn indices as follows:

    let factorial := μ.[λ.(S Z) | λ.(S 0) * 1 0] : N → N in
    factorial (S (S Z))

Let's refresh our memories about what this is sugar for:

    (λ.0 (S (S Z))) (μ.[λ.(S Z) | λ.(S 0) * 1 0] : N → N)

## In any of these cases, we need to decide how type-checking works

As is the case with recursive types, the binder μ.e says that the variable
bound by the binder stands for the entire infinite expression e. We expect the
function to have some consistent return type but we need to handle the fact
that its body may itself include a reference to the recursive type we are defining.

    (μ.[const (S Z) | λ.(S 0) * 1 0]) : N → N

How do we type-check this function? We have a type-signature to use N → N that we claim should cause the resulting type of the expression inside the μ to also be N → N.

Let's check it:

    The inner form we find is e | u:

Check both sides and we find:
  
    e : T → N
    u : N → N

Therefore, we know that the the type of the overall function is T + N → N which is the same as the definition of N itself:

    N := μ.T + 0

## Typing rule

  