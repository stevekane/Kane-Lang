Recursion is generally encoded using a special binder (often μ)
that introduces a free variable which represents everything in
the body of the binder:

  μa.P(a)

Here, P(a) is what "a" refers to.
For example, if you had the following expression:

  μa.1 + a

Then you have encoded an infinite tree whose unrollings would look
like the following expressions:

  1 + a
  1 + (1 + a)
  1 + (1 + (1 + a))
  etc...

In ΠΣ, they choose to use dependent pairs to encode recursion as follows:

  Nat:U = (l:{zero suc}) * case l of {
      zero -> Unit
    | suc -> Rec [Nat]
  }

This constructs a dependent pair whose first projection is an element of the
finite set {zero suc} and whose secend element depends on that value to
determine which function is run. If the element is zero then it returns Unit.
If the element is suc then it returns a Recursive call to [Nat]. [Nat] is used
because the expression inside must have a controlled unrolling since it
encodes an infinite type.

We can then define the two familiar constructors of natural numbers:

  zero : Nat = ('zero,'unit)
  suc  : Nat → Nat = λi.('suc,fold i)

This approach uses general recursion and has no syntactic recursive binder.
However, you DO seem to require Rec and [] or "boxing" in order to handle
types that have infinite expansions.

With these elements, as is done in the paper, you can formulate Natural Number
addition as follows:

  let add : Nat → Nat → Nat =
    m n ->
      split m with (l,r) ->
        !
          case l of {
            zero -> [n]
            succ -> [suc (add (unfold r) n)]
          }

This frightful shitshow uses several elements:

  split _ with (l,r) :: decompose a pair and bind two local variables
    case _ of {e} :: dependent case split on finite set e
      [e] :: put some value in a box
    ! [e] :: take a boxed value out the box

I don't really like this whole strategy very much and am going to ignore it now.

Instead, I think it's better to think of EVERY type as being under a binder
which enables self-reference. Thus, the shape of any type defined in the language
becomes μα.P(α) where the special case that P does not contain α is the familiar
case where no recursion is found. For example, the type of a triple of values
is:

  μα.T × T × T

There is a custom in some dependently-typed languages to omit the explicit
presence of binders when the expressions under those binders do not contain
the bound variable.

  Π(α:U) P(α) where P does not contain α = T → P
  Σ(α:U) P(α) where P does not contain α = T × P
  μα.P(α) where P does not contain α = P
  λα.P(α) where P does not contain α = const P

I would like to consider an infix style for all binders that will be familiar
to both programmers and proof-assistant users while helping to reduce the
cognitive complexity of reading and understanding types.

  Π(a:U) P(a) => (a:U) → P(a) "For all a of type U P(a)"
  Σ(a:U) P(a) => (a:U) × P(a) "There exists a of type U such that P(a)"
  μa.P(a)     => a ★ P(a)     "a is recursively equal to P(a)"
  λa.P(a)     => a ↦ P(a)     "map a to P(a)"

Here are a few examples:

  N = a ★ T + a
  Z : N = Inl(T) : N
  S : N → N = n ↦ Inr(n) : N
  add : N → N → N = μf.l ↦ [ n ↦ l ∥ n ↦ S (f n l) ]

Here are some possible syntaxes for functions:

  add : N → N → N = x y ↦ x + y

    vs

  add (x:N) → (y:N) → N = x + y

  add: (x:N, y:N) → N = x+y

    for a linear and dependent function

  id (τ:Type) → (t:1 τ) → τ = t

If you want to synthesize a term satisfying some type
it could look like this for the polymorphic identity function:

  id : (τ:Type, 1 t:τ) → τ

What if we wanted to synthesize the addition function itself?

  add : (a:N, b:N) → (c:N × (c ≡ a+b))

This only makes any sense if we're defining some NEW add function
but + is already defined somewhere for Natural Numbers. We are then
specifying a function that takes two natural numbers and returns
a dependent pair of a natural number c and a proof that c = a+b.
We may not actually want this proof to stay around though...
We could do this in two ways:

  add : (a:N, b:N) → N
  add_constraint : (a:N, b:N) → (add(a,b) ≡ a+b)

This style is not bad but there is a bit of repetition to it that 
seems... slightly un-necessary. Maybe there could be a syntax sugar
for these sorts of proofs/constraints/refinements:

  add : (a:N) → (b:N) → (c:N) | (c ≡ a + b)

You could even further standardize the use of binders in the following way:

  Function Type    Π(a:U) P(a)    (a:U) => P(a)
  Function Term    λ(a:U) P(a)    (a:U) -> P(a)
  Product Type     Σ(a:U) P(a)    (a:U) × P(a)
  Product Term     (a,(P(a))      (a:U) * P(a)
  CoProduct Type   P + Q          (a:U) + Q
  CoProduct Term   inl(p):P       inl(p):P
  CoProduct Term   inr(q):Q       inr(q):Q
  Recursive Type   μ(a:U) P(a)    (a:U) ★ P(a)
  Recursive Term   μ(a:U) P(a)    (a:U) ☆ P(a)

We could use these as follows:

  N = a ★ T + a

Additionally, I am inclined to prefer equirecursive types because I think
they enable a richer notion of generic programming wherein any types with
the same infinite expansions are equal.

  μα.1 + α = μβ.1 + (1 + β)

Note, this assumes α-equivalence and the ability to algorithmically compare
two potentially-different trees to discover whether they are similar or not.
This equirecursive comparison algorithm may not be easy to implement and
may have a cost on the expected run-time of the type-checker in general.