import Stlc
import PrettyPrinting

{-
Test the following key properties of the language:

Capture-avoiding Substitution
Type-checking
Evaluation
-}

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test msg actual expected = 
  if expected == actual
    then print ("Passed: " ++ msg)
    else print ("Failed: " ++ msg ++ ". Expected " ++ show expected ++ ". Found " ++ show actual)

main :: IO ()
main = do
  -- Important to use typechecking with annotations as well!
  test "U:U" (infer [] U) (Just U)
  test "Bot:U" (infer [] Bot) (Just U)
  test "Top:U" (infer [] Top) (Just U)
  test "T:Top" (infer [] T) (Just Top)
  test "(UxTop):U" (infer [] (U :×: Top)) (Just U)
  -- the left element of a product is a binder whose value is available to the right
  test "(UxVar_0):U" (infer [] (U :×: Var 0)) (Just U)
  -- this is not valid because the rightside would have type "Top" but must have type U
  test "(TopxVar_0) does not typecheck" (infer [] (Top :×: Var 0)) Nothing
  test "(T*Top):(TopxU)" (infer [] (T :*: Top)) (Just (Top :×: U))
  test "Exl (T*Bot)" (infer [] (Exl (T :*: Bot))) (Just Top)
  test "Exr (Bot*T)" (infer [] (Exr (Bot :*: T))) (Just Top)
  -- all the following should type-check as valid possible types for Top * T
  test "(Top*T):(TxTop)" (infer [] (Top :*: T)) (Just (U :×: Top))
  test "(Top*T):::(UxVar_0)" (infer [] ((Top :*: T) ::: (U :×: Var 0))) (Just (U :×: Top))
  test "(Top*T):::(UxTop)" (infer [] ((Top :*: T) ::: (U :×: Top))) (Just (U :×: Top))

  -- Evaluation tests
  test "eval U = U" (eval U) U
  test "eval Bot = Bot" (eval Bot) Bot
  test "eval Top = Top" (eval Top) Top
  test "eval T = T" (eval T) T
  test "eval (a + b) = a + b" (eval (Top :+: Bot)) (Top :+: Bot)
  test "eval (Inl a) = Inl a" (eval (Inl T)) (Inl T)
  test "eval (Inr a) = Inr a" (eval (Inr T)) (Inr T)
  test "eval Case f g (Inl a) = f a when valid" (eval (Case (Λ (Var 0)) (Λ Bot) (Inl Top))) Top
  test "eval Case f g (Inr a) = g a when valid" (eval (Case (Λ (Var 0)) (Λ Bot) (Inr Top))) Bot
  test "eval (a x b) => (eval a x eval b)" (eval (Top :×: Bot)) (Top :×: Bot)
  test "eval (a * b) => (eval a * eval b)" (eval (T :*: T)) (T :*: T)
  test "eval (Exl (a * b)) = a" (eval (Exl (Top :*: Bot))) Top
  test "eval (Exr (a * b)) = b" (eval (Exr (Top :*: Bot))) Bot
  test "eval (a + b) => (eval a + eval b)" (eval (T :+: T)) (T :+: T)

  -- pairs annotated with a type compute a new type given the term being annotated
  -- test condition input output
  --   where
  --     condition = "(Top*T):::(Ux0) => (Top*T):::(UxTop)"
  --     input = eval ((Top :*: T) ::: (U :×: Var 0))
  --     output = (Top :*: T) ::: (U :×: Top)

  -- Π(n:(Σ(u:U) u)) u

  -- (n:(U×0)
  -- test condition input output
  print fn
    where
      condition = "Function application refines a dependently-typed domain"
      -- fn = Λ (Exr (Var 0)) ::: (U :×: Top) :→: Top
      -- fn = Λ (Exr (Var 0)) ::: (U :×: Var 0) :→: Var 1
      -- fn = Λ (Var 0) ::: U :→: Var 0
      fn = Λ (Λ (Var 0)) ::: (U :→: (Var 0 :→: Var 1))
      -- fn = Λ (Var 0) ::: (Top :→: Top)
      input = infer [] fn
      output = Just $ (U :×: Var 0) :→: Var 1
  {-
  The paper on type-checking dependent-types says that typechecking must perform
  evaluation because types may depend on values. 

  Let's consider the case from above:

  (Top*T):(U×0)

  We want to know what this expression SHOULD be and if it is valid.
  We know the expected type is U×0 but we must ask what this evaluates to.
  Firstly, we can reduce the expression on the left and then typecheck it which 
  should yield back the normalized expression and its type.

  In this case, we get back (Top*T) ~> (Top*T):(U×Top)

  The syntactic form τ*σ:ρ implies a computation in a dependently-typed setting:
    Namely, we have a concrete value τ*σ that we must use to refine the type ρ.
    This is similar to the way that function application allows us to refine the 
    type of the function's codomain because we now have the actual input that the 
    function was called with. An annotated value is a similar notion but requires
    no "call" per se as the value simply exists and has been annotated allowing its
    annotated type to be refined in the context of the term it is annotating.

  (Top*T):(U×0) ↓ (Top*T):(U×Top)

  Then, we compare the type-checking result of the expression with this normalized
  annotated term for equality:

  (Top*T):(U×Top) ≡ (Top*T):(U×Top)

  Therefore, we return the final result of this operation as (Top*T):(U×Top)

  This means that the type of a pair a*b is always easily inferable as infer(a) × infer(b).

  The act of annotating a pair means that the annotated type now has additional information
  available to be normalized resulting in a computation that performs this normalization.

  The key takeaway is this: Even an annotated term may itself undergo evaluation to yield
  the result of typechecking it!

  Therefore, in the general case, type-checking should always yield annotated terms
  when checking is valid and a null case when it is not satisfied.

  a*b ~> (eval(a) * eval(b)):(infer(eval(a)) × infer(eval(b)))

  Alternatively, let's get more specific about what should happen w/ annotated expressions. 

  If we assume that we have an evaluation rule that will handle the case where you have
  a pair annotated with a product type then you know that some computation is possible on
  this composite term.

  This is defined by the following evaluation rule:

  e ↓ (a*b)   τ ↓ (σ×ρ)
  ----------------------
  e:τ ↓ (a*b):(σ×ρ[a/0])

  How then do we define type-checking of a term like this such that it uses this
  evaluation rule to correctly identify the type of the expression?

  the paper has type checking for every form:
    Universe
    Π
    Variable
    Lambda
    Application
  
  Applications are 

  ((Top*T):(U×0)):((Top*T):(U×Top))

  τ ↓ τ'
  e:τ'
  -------------
  (e:τ):(e':τ')


  Γ |- e:τ ↓ u:ρ
  Γ |- u:ρ
  ----------------
  Γ |- (e:τ):(u:ρ)


  Let's look at an example of something we expect to type-check:
  Let's create a function that expects a dependent pair and then
  simply returns the term itself.

  In a typical language this might look like this:

    right : ((t:U) × t) → t

  In our stupid language with DeBruijn indices it looks like this:

    right := (λ.π1 0):((U × 0) → 1)

  In english, this function expects a pair of a type τ and a term
  of that type τ and it returns that term of type τ.

  If we call this function with the following value we expect it to 
  typecheck:

    right (Top * T)

  At a glance, this is because we expect that the type  
  -}