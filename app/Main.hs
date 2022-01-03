{-# LANGUAGE DeriveTraversable #-}

module Main where

import Stlc
import PrettyPrinting
import System.IO
import Data.Traversable (Traversable)

λ = Λ

idx = λ (Var 0) 
  ::: (Top :→: Top)
idpair = λ (Var 0) 
  ::: ((Top :×: Top) :→: (Top :×: Top))
polymorphicid = λ (λ (Var 0))
  ::: U :→: (Var 0 :→: Var 1)
dup = λ (Var 0 :*: Var 0)
  ::: (Top :→: (Top :×: Top))
pair = λ (λ (Var 1 :*: Var 0))
  ::: (Top :→: (Top :→: (Top :×: Top)))
takepath = λ (Case dup idpair (Var 0))

wikiexp = 
  λ (λ ((Var 3 :@: Var 1) :@: λ (Var 0 :@: Var 2))) :@: λ (Var 4 :@: Var 0)

newtype Mu f = InF { outF :: f (Mu f) } 
data Lang a 
  = Variable Int 
  | Abstraction a
  | Application a a
  | Sum a a
  | Constant Int
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Result
  = Val Int
  | Expr (Mu Lang)
  | Failure String

-- These would need to be implemented by any calculus
data VarType 
  = Bound Int
  | Free Int
  | Match Int
  | NotVariable

class Calculus c where
  binding :: c -> Maybe Int
  binder  :: c -> Maybe Int

toVarType :: Int -> Mu Lang -> VarType
toVarType n InF { outF = Variable n' } | n' == n = Match n'
toVarType n InF { outF = Variable n' } | n'  > n = Free n'
toVarType n e                                    = NotVariable

decVar :: Int -> Mu Lang
decVar n = μ (Variable (n-1))

substitute :: Int -> Mu Lang -> Mu Lang -> Mu Lang
substitute n e b = case toVarType n b of
  Free n'  -> decVar n'
  Match n' -> e
  _        -> b

μ = InF 
cata :: Functor f => (f a -> a) -> Mu f -> a 
cata f = f . fmap (cata f) . outF

evaluate :: Mu Lang -> Result
evaluate = cata eval
  where
    eval :: Lang Result -> Result
    eval (Variable n) = Expr (μ (Variable n))
    eval (Constant n) = Val n
    eval (Sum (Val n) (Val m)) = Val (n+m)
    eval (Abstraction (Expr e)) = Expr (μ (Abstraction e))
    eval (Abstraction (Val n)) = Expr (μ (Abstraction (μ (Constant n))))
    eval (Application (Expr InF { outF = Abstraction b }) n) = n
    eval e = Failure "Unrecognized thing"

summ :: Mu Lang
summ =
  μ (Sum
    (μ (Constant 3))
    (μ (Constant 5)))

program :: Mu Lang
program = 
  μ (Application 
    (μ (Abstraction (μ (Constant 5))))
    (μ (Constant 3)))

run :: Mu Lang -> String
run p = case evaluate p of
  Val v -> show v
  Expr e -> "failed to evaluate completely"
  Failure s -> s

main :: IO ()
main = do
  print (infer [] U)
  print (infer [] Bot)
  print (infer [] Top)
  print (infer [] T)
  print (infer [] (T ::: Top)) -- correct
  print (infer [] (T ::: Bot)) -- should not typecheck
  print (infer [] (U :×: Top)) -- correct both are type U
  print (infer [] (U :×: Var 0)) -- correct (this is existential quantification)
  print (infer [] (Top :×: Var 0)) -- No. Var 0 will have type Top but must be U
  print (infer [] (Top :*: T)) -- correct should have type U × Top
  print (infer [] ((Top :*: T) ::: (U :×: Var 0))) -- works
  print (infer [] ((Top :*: T) ::: (U :×: Top))) -- should work
  print $ eval (λ (Var 0) :@: T)
  print $ eval (λ (Var 0 :@: Var 0) :@: T)
  print $ eval $ λ (λ (Var 0 :@: Var 1)) :@: T
  print $ eval $ λ (λ (Var 1 :@: Var 0)) :@: λ (Var 0)
  print $ eval $ λ (λ (Var 1)) :@: Var 0
  print wikiexp
  print $ eval wikiexp
  print $ run summ
  print $ run program