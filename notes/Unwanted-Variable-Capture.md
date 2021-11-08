# On un-wanted variable capture during substitution:

    [[ p[t/x] ]] = "substitute every occurrence of x in p with t"

If you have quantifiers such as this:

    p = ∀y.x + y = 3
    t = y + y
    p[t/x] = ∀y.y + y + y = 3

Our intended "free variable" y in t was "captured" when it
was substituted under the binder in p. Now the meaning of the
resulting expression quantifies over the locally-bound variable
y which changes the meaning of the expression.

This is solvable by α-conversion of p such that its meaning 
is un-changed. This requires you to observe the bound variables
in t and convert the bound variables and occurrences in p 
to avoid conflicting with these names.

    free_variables(t) = {y}
    free_variables(p) = {x}
    bound_variables(p) = {y}
    bound_variables(p[t/x]) = {y}
    free_variables(p[t/x]) = ∅
    bound_variables(p) ∩ free_variables(t) /= ∅ 
    ∴
    p[y/z] = ∀z.x + z = 3
    t = y + y
    p[y/z][t/x] = ∀z.y + y + z = 3

    free_variables(p[t/x]) = {x}
    bound_variables(p[t/x]) = ∅

We have avoided capture by finding suitable variables under
α-conversion such that the bound and free variables of the
two expressions do not vary.

Important!

Computing α-equivalence is made complex by the issues discussed
above. The most common solution for dealing with this issue
is to convert all bound variables and their occurrences into
a representation called DeBruijn indices. In this system, a
variable occurrence is denoted by its distance from the abstraction
where it was defined. This binding distance metric is defined as
a natural number, beginning at 0 that increases
by one for every new binder encountered on the path to the occurrence.

    λx.λy.x converts to λ.λ.1

Here are several exercises from Pierce's book to practice converting
to DeBruijn indices:

    c0 = λx.λz.z
       = λλ0
    c2 = λs.λz.s (s z)
       = λλ1 (1 0)
    plus = λm.λn.λs.λz.m s (n z s)
         = λλλλ3 1 (2 0 1)
    fix = λf.(λx.f (λy.(x x) y)) (λx.f (λy. (x x) y))
        = λ(λ1 (λ(1 1) 0)) (λ1 (λ(1 1) 0))
    foo = (λx.λx.x)) (λx.x)
        = λλ0 λ0