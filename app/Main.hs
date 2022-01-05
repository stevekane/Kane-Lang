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

main :: IO ()
main = do
  print (infer [] ((Top :*: T) ::: (U :×: Top))) -- should work
  print $ eval (λ (Var 0) :@: T)
  print $ eval (λ (Var 0 :@: Var 0) :@: T)
  print $ eval $ λ (λ (Var 0 :@: Var 1)) :@: T
  print $ eval $ λ (λ (Var 1 :@: Var 0)) :@: λ (Var 0)
  print $ eval $ λ (λ (Var 1)) :@: Var 0
  print wikiexp
  print $ eval wikiexp