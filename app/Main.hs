module Main where

import Stlc
import PrettyPrinting
import System.IO

λ = Λ
-- μ = Rec

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
takepath = dup :|: idpair

readsource :: String -> IO String
readsource s = do
  handle <- openFile s ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  hClose handle
  return contents

prun name e = do
  print name
  print e
  print (infer [] e)
  print "-----------------------"

wikiexp = 
  λ (λ ((Var 3 :@: Var 1) :@: λ (Var 0 :@: Var 2))) :@: λ (Var 4 :@: Var 0)

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