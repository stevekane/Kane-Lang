import Stlc
import PrettyPrinting

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test msg actual expected = 
  if expected == actual
    then print ("PASSED  " ++ msg)
    else print ("FAILED  " ++ msg ++ ". Expected " ++ show expected ++ ". Found " ++ show actual)

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

  test "(fn.u e):(u[0/e])" (infer [] (h :@: T)) (Just (Top :×: Top))