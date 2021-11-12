# On eliminators for common types

Introduction rules tell you what must be true for a term of some type
to be introduced. Elimination rules tell you what must be true for a
type to be eliminated. The process of eliminating and introction is
called computation.

## Coproducts

For coproducts we need then the following introductions:

    (i1 a):(A + B) when a:A
    (i2 b):(A + B) when b:B

We need the following elimination:

    ([f | g] p):C when f:(A → C) and g:(B → C) and p:(A + B)

This yields the obvious computational meaning:

    ([f | g] (i1 a):(A + B)) ↓ f(a)
    ([f | g] (i2 b):(A + B)) ↓ g(b)

## Dependent Products

We can introduce dependent products (a:A) × B(a) as follows:

    (a * b):(A × B) when a:A Γ+A |- b:B

We can eliminate them with the familiar forms:

    (π1 p):A  when p:(A × B)
    (π2 p):B' when p:(A × B) and B[(π1 p)/0] == B'

Computation is performed as follows:

    (π1 (a * b)) ↓ a
    (π2 (a * b)) ↓ b

## Dependent Functions

We can also introduce a dependent function type similarly:

    (λe):(A → B) when Γ+A |- e:B

We can eliminate these things using a dependent form of function application:

    ((λe):(A → B) u):B' when u:A and B[u/0] == B'