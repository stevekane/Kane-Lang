module Main where

import Stlc
import System.IO

idx = Λ "x" (Var "x") 
  ::: (Top :→: Top)
idpair = Λ "p" (Var "p") 
  ::: ((Top :×: Top) :→: (Top :×: Top))
dup = Λ "x" (Var "x" :*: Var "x")
  ::: (Top :→: (Top :×: Top))
pair = Λ "l" (Λ "r" (Var "l" :*: Var "r"))
  ::: (Top :→: (Top :→: (Top :×: Top)))
takepath = dup :|: idpair

-- here is a quick reference implementation of factorial in Haskell
data N = Z | S N deriving (Eq, Show)
add :: N -> N -> N
add a Z = a
add a (S n) = S (a `add` n)
mul :: N -> N -> N
mul a Z = Z
mul a (S n) = (a `mul` n) `add` a
fac :: N -> N
fac Z = S Z
fac (S n) = S n `mul` fac n

-- here is an implementation that does not use pattern-matching
(.|.) :: (() -> a) -> (N -> a) -> (N -> a)
(f .|. g) Z = f ()
(f .|. g) (S n) = g n

instance Num N where
  a + b = (const a .|. \n -> S (a + n)) b
  a * b = (const Z .|. \n -> a * n + a) b
  fromInteger 0 = Z
  fromInteger n = S (fromInteger n-1)

factorial = const (S Z) .|. \n -> S n * factorial n

nats = Mu "α" (Top :+: TVar "α")
z = Inl T ::: nats
s = (Λ "x" (Inr (Var "x")) ::: nats) ::: (nats :→: nats)
constz = Λ "n" z ::: nats
consts = Λ "n" (Inr (Var "n")) ::: nats
-- add = Rec "f" $ Λ "x" (Var "x")

readsource :: String -> IO String
readsource s = do
  handle <- openFile s ReadMode
  hSetEncoding handle utf8
  hGetContents handle

-- tryeval :: String -> Either String Term
-- tryeval s = case snd (run pterm s) of
--   Left e  -> Left e
--   Right t -> case check [] t of
--     Nothing -> Left "Did not typecheck"
--     Just τ  -> Right (evaluate t)
prun name e = do
  print name
  print e
  print (infer [] e)
  print (evaluate e)

main :: IO ()
main = do
  prun "idx" idx
  prun "dup" dup
  prun "pair" pair
  prun "appidx" $ idx :@: T
  prun "appdup" $ dup :@: T
  prun "apppairpartial" $ pair :@: T
  prun "apppair" $ pair :@: T :@: T
  prun "takepath" takepath
  prun "takepath(inl(T))" $ takepath :@: (Inl T ::: (Top :+: (Top :×: Top)))
  prun "takepath(inr(T))" $ takepath :@: (Inr (T :*: T) ::: (Top :+: (Top :×: Top)))
  print nats
  print z 
  print s
  print constz
  print consts
  print $ mul (S (S Z)) (S (S Z))
  print $ fac (S (S (S Z)))
  print $ factorial (S (S (S Z)))

  -- print $ run pType "T"
  -- print $ run pType "⊥"
  -- print $ run pType "(T × T)"
  -- print $ run pType "(T → ⊥)"
  -- print $ run pterm "1"
  -- print $ run pterm "<1,1>"
  -- print $ run pterm "π1"
  -- print $ run pterm "π2"
  -- print $ run pterm "[π1 x]"
  -- print $ run pterm "[y x]"
  -- print $ run pterm "[1 1]"
  -- print $ run pterm "(λx.x):(T → T)"
  -- print $ run pterm "[(λx.x):(T → T) 1]"

  -- testsrc <- readsource "programs/test.kane"
  -- print $ tryeval testsrc

  -- letsrc <- readsource "programs/let.kane"
  -- print letsrc
  -- let parsed = run pterm letsrc
  -- print parsed

  -- print $ tryeval letsrc