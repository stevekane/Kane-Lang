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