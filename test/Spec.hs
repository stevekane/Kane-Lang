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
  -- id : Π(τ:U) Π(t:τ) → τ
  -- id τ x = x
  -- (λλ0):(U → 0 → 1)
  let depid = Λ (Λ (Var 0)) ::: (U :→: Var 0 :→: Var 0)
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
  test "dependent identity function" (infer [] depid) (Just (U :→: Var 0 :→: Var 0))
  test "(fn.u e):(u[0/e])" (infer [] (h :@: T)) (Just (Top :×: Top))

  let fn = Λ (Λ (Var 1))
  let gn = Λ (Λ (Var 0))
  let tau = U :→: Var 0 :→: Var 1

  -- The paper on LambdaPi introduces this as follows:
  --    id : ∀a.a → a
  --    id = λ(τ:U).λ(t:τ).t

  -- in a language with implicit arguments you might see this:
  --    id : {a:U} → (x:a) → a = x

  -- imagine using a c-like language: 
  --    id(τ:U,t:τ) → τ = t

  -- in a terse, debruijn syntax, we might expect this:
  --    id : U → 0 → 1 = 0

  -- Currently, we have this:
  --    λλ0:(U → 0 → 1)

  -- λλ1 T => λT
  print $ eval $ fn :@: T
  -- λλ0 T => λ0
  print $ eval $ gn :@: T
  print depid
  print $ eval depid
  print $ infer [] depid