{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DeriveFunctor     #-}

module Parser where

import Prelude hiding (any)
import Control.Applicative
import Data.Char

newtype Parser a = Parser { 
  run:: String -> (String, Either String a) 
} deriving (Functor)

instance Applicative Parser where
  pure a = Parser (, Right a)
  pf <*> pa = Parser (\s -> case run pf s of
    (s', Right f) -> fmap (fmap f) (run pa s')
    (s', Left e)  -> (s', Left e))

instance Alternative Parser where
  empty = Parser (, Left "Always fails")
  pl <|> pr = Parser (\s -> case run pl s of
    (s', Left e)  -> run pr s
    (s', Right a) -> (s', Right a))

instance Monad Parser where
  pa >>= f = Parser (\s -> case run pa s of 
    (s', Right a) -> run (f a) s'
    (s', Left e)  -> (s', Left e))

any :: Parser Char
any = Parser (\s -> case s of 
  "" -> (s, Left "Empty") 
  _  -> (tail s, Right (head s)))

eof :: Parser ()
eof = Parser (\s -> case s of
  "" -> (s, Right ())
  _  -> (s, Left "Expected EOF"))

try :: Parser a -> Parser a
try p = Parser (\s -> case run p s of
  (s', Left err) -> (s, Left err)
  (s', Right a)  -> (s', Right a))

satisfy :: 
  String -> 
  (Char -> Bool) -> 
  Parser Char
satisfy d p = try $ do
  c <- any
  if p c
    then return c
    else Parser (, Left d)
  
pchar :: Char -> Parser Char
pchar c = satisfy [c] (== c)

palpha :: Parser Char
palpha = satisfy "isAlpha" isAlpha

pspace :: Parser Char
pspace = satisfy "isSpace" isSpace

pnewline :: Parser Char
pnewline = pchar '\n'

pstring :: String -> Parser String
pstring = traverse pchar