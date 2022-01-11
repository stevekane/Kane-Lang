import Stlc
import PrettyPrinting

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test msg actual expected = 
  if expected == actual
    then print ("PASSED  " ++ msg)
    else print ("FAILED  " ++ msg ++ ". Expected " ++ show expected ++ ". Found " ++ show actual)

main :: IO ()
main = do
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
  test "eval continues after case left" (eval (Case (Λ (Exl (Var 0 :*: T))) (Λ Bot) (Inl Top))) Top
  test "eval continues after case right" (eval (Case (Λ Bot) (Λ (Exr (T :*: Var 0))) (Inr Top))) Top
  test "eval (a x b) => (eval a x eval b)" (eval (Top :×: Bot)) (Top :×: Bot)
  test "eval (a * b) => (eval a * eval b)" (eval (T :*: T)) (T :*: T)
  test "eval (Exl (a * b)) = a" (eval (Exl (Top :*: Bot))) Top
  test "eval (Exr (a * b)) = b" (eval (Exr (Top :*: Bot))) Bot
  test "evaluation continues after left projection" (eval (Exl (Exl (Top :*: Bot) :*: Bot))) Top
  test "evaluation continues after right projection" (eval (Exr (Exr (Bot :*: (Bot :*: Top))))) Top
  test "eval (a -> b) = a -> b" (eval (Top :→: Bot)) (Top :→: Bot)
  test "eval fn.u = fn.u" (eval (Λ T)) (Λ T)
  test "eval (fn.u e) = eval u[e/0]" (eval (Λ (Var 0 :*: Var 0) :@: T)) (T :*: T)
  test "function application of annotated functions is correct" (eval ((Λ (Var 0) ::: (Top :→: Top)) :@: T)) T
  test "evaluation continues after application" (eval (Λ (Exl (Var 0 :*: Var 0)) :@: T)) T
  test "eval (Var_0) = Var_0" (eval (Var 0)) (Var 0)

  -- Type-checking tests
  test "U:U" (infer [] U) (Just U)
  test "Bot:U" (infer [] Bot) (Just U)
  test "Top:U" (infer [] Top) (Just U)
  test "T:Top" (infer [] T) (Just Top)
  test "(a+b):U when a and b are types" (infer [] (Top :+: Bot)) (Just U)
  test "(a+b) has no type when a does not evaluate to a type" (infer [] (Exl (T :*: Top) :+: Bot)) Nothing
  test "(a+b) has no type when b does not evaluate to a type" (infer [] (Bot :+: Exr (Top :*: T))) Nothing
  test "(inl a):(A + B) has type A when a has type A" (infer [] (Inl T ::: (Top :+: Bot))) (Just (Top :+: Bot))
  test "(inr b):(A + B) has type B when b has type B" (infer [] (Inr T ::: (Bot :+: Top))) (Just (Bot :+: Top))
  test "inl a:A has no type if evaluating A is not a coproduct" (infer [] (Inl T ::: Exl (Top :*: Bot))) Nothing
  test "inl a has no valid type" (infer [] (Inl T)) Nothing
  test "inr a has no valid type" (infer [] (Inr T)) Nothing

  let f = Λ T ::: (Top :→: Top)
  let g = Λ T ::: (Top :→: Top)
  let h = Λ (Var 0 :*: Var 0) ::: (Top :→: (Top :×: Top))
  let j = Λ (Var 0) ::: (U :→: U)
  let fg = Λ (Λ (Var 0 :*: Var 1)) ::: (Top :→: Top :→: (Top :×: Top))
  let depid = Λ (Λ (Var 0)) ::: (U :→: Var 0 :→: Var 1)
  let x = Inl T ::: (Top :+: Top)
  let fbad = Λ T ::: (U :→: Top)
  let gbad = Λ T ::: (U :→: Top)
  let xbad = T

  test "case f g x has type c when f:a->c and g:b-> and x:a+b" (infer [] (Case f g x)) (Just Top)
  test "case f g x has no type when x is not a coproduct" (infer [] (Case f g xbad)) Nothing
  test "case f g x has no type when f is not of type a->c" (infer [] (Case fbad g xbad)) Nothing
  test "case f g x has no type when g is not of type b->c" (infer [] (Case f gbad xbad)) Nothing

  test "(a x b):U when a and b are types" (infer [] (Top :×: Top)) (Just U)
  test "(a x b):U when a and b(a) are types" (infer [] (U :×: Var 0)) (Just U)
  test "(a x b):U when a and eval b(a) are types" (infer [] (U :×: (j :@: Top))) (Just U)
  test "(a x b) has no type when b(a) is not a type" (infer [] (Top :×: Var 0)) Nothing
  test "(a * b):(type a) x (type b)" (infer [] (T :*: T)) (Just (Top :×: Top))
  test "exl (a * b):type a" (infer [] (Exl (T :*: Top))) (Just Top)
  test "exr (a * b):type b" (infer [] (Exr (Top :*: T))) (Just Top)

  test "(a -> b):U when a and b are types" (infer [] (Top :→: Bot)) (Just U)
  test "(fn.fn.u):(a -> b -> c) as convenient sugar" (infer [] fg) (Just (Top :→: Top :→: (Top :×: Top)))
  test "(fn.u):(a -> b) has type a -> b when u has type b in a context where 0 -> a" (infer [] h) (Just (Top :→: (Top :×: Top)))
  test "simple dependent function" (infer [] j) (Just (U :→: U))
  test "dependent identity function" (infer [] depid) (Just (U :→: Var 0 :→: Var 1))
  test "(fn.u e):(u[0/e])" (infer [] (h :@: T)) (Just (Top :×: Top))

  let bdy = Λ (Λ (Var 0))
  let tpe = U :→: (Var 0 :→: Var 1)
  let identity = bdy ::: tpe

  print (infer [] identity)

  {-
  I think I have found the problem. 

  When you add the named variable 0 => U into the context you would like it
  to stay bound in this way.

  When you add the named variable 1 => 0 you would like it to stay this way.

  Γ,1,U |-   0:1
  "prepend 0+1 to the context because variable 0 after removing the binder is now variable 1"
  Γ,U   |-  λ0:0 → 1
  "prepend U to the context"
  Γ     |- λλ0:U → 0 → 1

  -}
  {-
  Adding Π and Σ types introduces variables in type expressions. 

  Π(x:τ) denotes a bound variable x of type τ which may appear
  anywhere in the body of the Π.

  Σ(x:τ) is the same thing but captures dependent pairs as opposed
  to functions.

  If we use DeBruijn indices instead of named variables, we can capture
  the same idea using this slightly ammended notation:

  Π(x:α) β becomes α → β where β may contain the variable "0" which
  is the "name" for what used to be the bound variable "x".

  Σ(x:α) β becomes α × β where β may contain the variables "0" for the
  reasons specified above.

  This means that when we are checking the type of a variable, it is 
  now possible for a type to be itself a bound variable and not always
  some concrete value.

  For example, let's look at the classic existential quantifier as
  modeled in dependent type theory by the product:

    (a * b) : (U × 0)

  This sentence declares that a * b is a witness of the type U × 0.
  In english, this says that the type of a is U (a is a therefore
  a type) and that the type of b is the type bound to variable 0.

  Γ   |- a:U
  Γ,U |- b:0
  --------------------
  Γ |- (a * b):(U × 0)

  Γ   |- a:α
  Γ,α |- b:β
  --------------------
  Γ   |- (a * b):(α × β)




  The typing rules as stated in The Little Typer for pairs are as follows:

  Γ |- a:α ~> a'
  Γ |- b:β[a'/0] ~> b'
  -----------------------------------
  Γ |- (a * b) : (α × β) ~> (a' * b')

  These rules are read aloud as:

  "To check that a*b has the type α×β yielding value a'*b' you require
  a have the type α yielding value a' and b have the type β with capture-avoiding
  substitution of 0 for a' yielding value b'"

  In the meta-language:
    Checking is function of type : Term * Term → Term
    Inferring is a function of type : Term → Term * Term

  I don't really care for their syntax however so let's rewrite it:

  (_ |- _ : _) : 
    (Γ : Context) → 
    (a * b) : (Term × Term) →
    (α × β) : (Term × Term) → 
    (c : Term) * (Γ |- a : α = c) → 
    (d : Term) * (Γ |- b : β[a'/0] = d) →
    (Term × Term) = 
      (c * d)

  This is itself syntax of a dependently-typed programming language (Agda)!
  We could use this, along with all other valid definitions of typing judgements
  to define a datatype of typechecked expressions.


  What about functions? How does TLT handle them?

  Γ,x:Arg |- r ∈ R ~> r'
  --------------------------------------------
  Γ |- (λ (x) r) ∈ (Π (x Arg) R) ~> (λ (x) r')

  Let's convert this to an indexed style matching our langauge:

  Γ+α |- e : β ~> u
  --------------------------------------------
  Γ |- λe : (α → β) ~> λu

  Aloud, this is read as follows:

  "To check that in context Γ λe has type α → β yielding λu you must check that
  in context Γ+α e has type β yielding u"

  There is a second rule listed in TLT for n-ary functions. Let's look at that.

  Γ+α |- λλe : β ~> u
  -------------------------
  Γ |- λλλe : (α → β) ~> λu

  In english:

  "This is the same as the rule above applied recursively to itself."

  Going back to our horrible no-good problem:

  Γ+0+U |- 0:1 ~> 0
  Γ+U   |- λ0:0 → 1 ~> λ0
  Γ     |- λλ0:U → 0 → 1 ~> λλ0
  -}