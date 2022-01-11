The paper on LambdaPi introduces this as follows:
   id : ∀a.a → a
   id = λ(τ:U).λ(t:τ).t

in a language with implicit arguments you might see this:
   id : {a:U} → (x:a) → a = x

imagine using a c-like language: 
   id(τ:U,t:τ) → τ = t

in a terse, debruijn syntax, we might expect this:
   id : U → 0 → 1 = 0

Currently, we have this:
   λλ0:(U → 0 → 1)

Let's clearly identify the problem by finding a near-minimal case that fails.

So far, I have this case: 
  
  (λλ0:U → 0 → 1)

  two parameter function. 
  first argument is a type. 
  second argument is a term of that type.
  result is a term of that type.

This is the polymorphic identity function except that it currently does not type-check.
Strangely, this seemingly-incorrect similar function DOES type check:

  (λλ0:U → 0 → 0)

  two parameter function.
  first argument is a type.
  second argument is a term of that type.
  third argument is a term of the term of the type...


Intuitively, here is the description of what should happen:

  λλ0:U → 0 → 1

We want to know if this function's body satisfies the stated type-signature.
We can check this by analyizing the function's structure while including the
types from the provided signature.

We see that the function's body is itself another function: 

  λλ0

We know the outer domain is of type U and the codomain is 0 → 1.
In a context [U] we know the type 0 is U.
In a context [U,0] we know the type 1 is the type 

"The type of codomain is the provided type in a context extended by the domain"

  Γ+U |- λ0:(0 → 1)

"The type of the codomain is the provided type in a context extended by the domain"

  Γ+U+0 |- 0:1

"The type of variable 1 is now 

Formally, here is the deduction chain:

Γ+U |- λ0:0 → 1
--------------------
Γ   |- λλ0:U → 0 → 1

FROM PAPER

  Γ,x::τ |- e:τ'
  ----------------------
  Γ |- λx.e:((x:τ) → τ')

  Γ+a |- e:b
  Γ   |- λe:(a → b)

  λλ0:(U→(0→1))

  "free variables are found in the context"

  λλ0 is a term with binders and variables

  U → 0 → 1 this is a type signature with binders and variables

  

  [0,U] |-   0:1
  [U]   |-  λ0:0→1
  []    |- λλ0:U→0→1

  Γ,(x => τ, τ => U) |-       x:τ
  Γ,(τ => U)         |-    λx.x:τ → τ
  Γ                  |- λτ.λx.x:(τ:U) → τ → τ

  We add the type variable τ to the context and say that its type is U.
  We then add x to the context and we say that its type is τ.
  Notice, τ is NOT a typical "concrete type" but is itself a type variable
  which only has a meaning or is considered valid if it is found in the context.
  In this case, we do find τ in the context and additionally, we know that its
  type is U meaning that it indeed stands for a type.

  I think this logic specifically is what is needed in our DeBruijn implementation
  to fully deal with variables in type signatures.

  In the derivation below, the variable U is prepended on the context.
  The variable 0 is then prepended onto the context.
  The variable 0 then is expected to have the type variable 1.
  This is checked by asking if 0 is in fact a variable with a type defined
  in the context.

  In our explicit mapping below, we see that it is.
  0 has the type 1 which is exactly what we are expecting.

  Finally, 1 is indeed a type as it is found in the context with type U.

  Γ,0,U |-   0:1
  Γ,U   |-  λ0:0 → 1
  Γ     |- λλ0:U → 0 → 1

  []        |- λλλ0:A → B → C → C
  shift off a binder
  [A]       |- λλ0:B → C → C
  shift off a binder
  [B,A]     |- λ0:C → C
  shift off a binder
  [C,B,A]   |- 0:C
  Here we see that 0 does indeed have the type C in this context

  []    |- λλ0:U→0→1
  [U]   |- λ0:0→1
  [0,U] |- 0:1
  Here the variable "0" refers to the next variable right of it
  in the context which is U.
  So we ask "does 0 have the type 1?"
  The type 1 is U and 0 has the type 0 which refers to the variable
  to its right in the context which is U.
  Therefore, variable 0 has type U in this context.

  check type of body in context extended by domain
    check type of body in context extended by domain
      check type of body has type of codomain
        lookup 0 in context
          if variable n lookup type n+1 entries to the right of the variable
          if type then return the type
          else return no type found

  []    |- λλ0:U→0→1
  [U]   |- λ0:0→1
  [0,U] |- 0:1
  lookup 0 in context and find 0
    lookup 0+1 in context and find U

  Γ+A |- e:B
  -------------
  Γ |- λe:A → B

  I have a nagging suspicion here that the inference rules they claim
  to be using in the LambdaPi paper are in fact fucking horseshit and
  assume you are using named variables as shown above as opposed to
  deBruijn indices for local variables.

  In their implementation, they substitute 0 into the body of the lambda
  AND into codomain of the type signature when performing type-checking
  (in addition to adding the variable to the context).