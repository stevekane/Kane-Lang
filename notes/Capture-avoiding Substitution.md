# Capture-avoiding substitution

Implementing capture-avoiding substitution
solves the following problem:

In e, x and y are bound variables

    e = λx.λy.x y

In u, y is a free variable

    u = y

In e u, y is a bound variable

    λy.y y

This is a problem because it changes the meaning of y
from referring to a free variable to the bound variable
after substitution.

If we enforce that no free variables may ever be named
the same as bound variables then we could make the following
change:

    e   = λx.λy.x y
    u   = z
    e u = λy.u y

Alternatively, we could change the names of bound variables
to not conflict with incoming free variables during substitution.

    e   = λx.λy.x y
    u   = y
    e u = λy1.y y1

Here is a complicated problem with picking a new name:

    e      = λx.λy.x y
    u      = x
    e[u/x] = λy.λy.y y

Here, be naively choosing a fresh variable name that isn't "x"
we have just selected "y" which then conflicts! You can see then
that selecting a new fresh variable that is GUARANTEED to be
unique is tedious. You need to check every variable bound in e.

    e      = λx.λy.x y
    u      = x
    {- x conflicts with x -}
    {- try y -}
    {- y conflicts with inner bound y -}
    {- rewind try z -}
    {- z works -}
    e[u/x] = λz.λy.z y

This algorithm says:

    x ∈ free(u)
    z = fresh(u,x)
    -----------
    λx.e(x)[]