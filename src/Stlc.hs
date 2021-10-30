{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stlc where

import Control.Monad
import Control.Monad.Fail
import Data.Bool
import Data.Maybe
import Data.Either
import Data.String
import Data.List hiding (any)
import Data.Eq (Eq, (==), (/=))
import GHC.Show (Show, show)
import Parser 


type Context = [(String,Type)]

-- TODO: include recursive types 
data Type where
  Bot   :: Type
  Top   :: Type
  (:+:) :: Type -> Type -> Type
  (:×:) :: Type -> Type -> Type
  (:→:) :: Type -> Type -> Type
  Mu    :: String -> Type -> Type
  TVar  :: String -> Type
  deriving (Eq)

data Term where
  Var   :: String -> Term
  T     :: Term
  Inl   :: Term -> Term
  Inr   :: Term -> Term
  (:|:) :: Term -> Term -> Term
  (:*:) :: Term -> Term -> Term
  Exl   :: Term -> Term
  Exr   :: Term -> Term
  Λ     :: String -> Term -> Term
  Rec   :: String -> Term -> Term
  (:@:) :: Term -> Term -> Term
  (:::) :: Term -> Type -> Term
  deriving (Eq)










-- BEGIN PRINTING
binary :: 
  (Show a, Show b) => 
  String -> 
  a -> 
  String -> 
  b -> 
  String -> 
  String
binary ol l m r or = 
  ol ++ show l ++ m ++ show r ++ or

between :: 
  Show s => 
  String -> 
  s -> 
  String -> 
  String
between ol m or = 
  ol ++ show m ++ or

instance Show Type where
  show Bot = 
    "⊥"
  show Top = 
    "⊤"
  show (τ :+: σ) = 
    binary "(" τ " + " σ ")"
  show (τ :×: σ) = 
    binary "(" τ " × " σ ")"
  show (τ :→: σ) = 
    binary "(" τ " → " σ ")"
  show (Mu x e) =
    "(μ" ++ x ++ "." ++ show e ++ ")"
  show (TVar x) =
    x

instance Show Term where
  show (t ::: τ) = 
    binary "" t ":" τ ""
  show (Var s) = 
    s
  show T = 
    "T"
  show (Inl l) = 
    between "inl(" l ")"
  show (Inr r) = 
    between "inr(" r ")"
  show (l :|: r) = 
    binary "[" l " | " r "]"
  show (l :*: r) = 
    binary "<" l "," r ">"
  show (Exl l) = 
    between "exl(" l ")"
  show (Exr r) = 
    between "exr(" r ")"
  show (Λ x e) = 
    "(λ" ++ x ++ "." ++ show e ++ ")"
  show (Rec x e) =
    "(μ" ++ x ++ "." ++ show e ++ ")"
  show (f :@: e) = 
    show f ++ between "(" e ")"
-- END PRINTING










-- BEGIN PARSING
-- pType, pTop, pBottom, pProduct, pArrow :: Parser Type
-- pTop = pchar 'T' >> pure Top
-- pBottom = pchar '⊥' >> pure Bottom
-- pProduct = do
--   pchar '('
--   l <- pType
--   pstring " × "
--   r <- pType
--   pchar ')'
--   return (Product l r)
-- pArrow = do
--   pchar '('
--   l <- pType
--   pstring " → "
--   r <- pType
--   pchar ')'
--   return (Arrow l r)
-- pType = 
--   pProduct <|> 
--   pArrow <|>
--   pBottom <|>
--   pTop
-- 
-- punit, ppair, pexl, pexr, pvariable, papplication, pfunction, plet :: Parser Term
-- pvariable = Inferable . Variable <$> some palpha
-- punit = pchar '1' >> pure (Inferable Unit)
-- pexl = pstring "π1" >> pure (Inferable Exr)
-- pexr = pstring "π2" >> pure (Inferable Exl)
-- ppair = do 
--   pchar '<' 
--   l <- pterm
--   pchar ','
--   r <- pterm
--   pchar '>'
--   return (Inferable (Pair l r))
-- papplication = do 
--   pchar '[' 
--   f <- pterm 
--   pspace 
--   e <- pterm 
--   pchar ']' 
--   return (Inferable (Application f e))
-- pfunction = do
--   pchar '(' >> pchar 'λ'
--   x <- some palpha
--   pchar '.'
--   e <- pterm
--   pchar ')' >> pchar ':'
--   τ <- pType
--   return (Annotated (Function x e τ))
-- plet = do
--   pstring "let "
--   x <- some palpha
--   pstring " be "
--   v <- pterm
--   pstring " in" 
--   pspace <|> pnewline
--   e <- pterm
--   return ((x ⇒ v) e)
-- pterm = 
--   punit <|>
--   pexr <|>
--   pexl <|>
--   ppair <|>
--   pfunction <|>
--   papplication <|>
--   plet <|>
--   pvariable
-- END PARSING









-- BEGIN TYPE-CHECKING
infer :: Context -> Term -> Maybe Type
infer γ (t ::: τ) = do
  check γ t τ
infer γ (Var s) = do
  lookup s γ 
infer γ T = do
  return Top
infer γ (a :*: b) = do
  σ <- infer γ a
  τ <- infer γ b
  return (σ :×: τ)
infer γ (a :|: b) = do
  α :→: τ  <- infer γ a
  β :→: τ' <- infer γ b
  True     <- return (τ == τ')
  return ((α :+: β) :→: τ)
infer γ (Exl t) = do
  σ :×: _ <- infer γ t
  return σ
infer γ (Exr t) = do
  _ :×: τ <- infer γ t
  return τ
infer γ (f :@: e) = do
  σ :→: τ <- infer γ f
  σ'      <- infer γ e
  True    <- return (σ == σ')
  return τ
infer _ _ = 
  fail "Failed to infer type"

check :: Context -> Term -> Type -> Maybe Type
check γ (Inl l) τ@(σ :+: _) = do 
  σ'   <- infer γ l 
  True <- return (σ == σ')
  return τ
check γ (Inr r) τ@(_ :+: ρ) = do 
  ρ'   <- infer γ r
  True <- return (ρ == ρ')
  return τ
-- Allows (λx.λy.<x,y>):(T → (T → (T × T)))
check γ (Λ x e@(Λ y u)) τ@(σ :→: ρ) = do
  ρ'   <- check ((x,σ) : γ) e ρ
  True <- return (ρ == ρ')
  return τ
check γ (Λ x e) τ@(σ :→: ρ) = do
  ρ'   <- infer ((x,σ) : γ) e
  True <- return (ρ == ρ')
  return τ
check _ _ _ = 
  fail "Failed to check type" 
-- END TYPECHECKING










(/) :: Term -> String -> Term -> Term
(e/x) (Exl l) = 
  Exl ((e/x) l)
(e/x) (Exr r) = 
  Exr ((e/x) r)
(e/x) (a :*: b) = 
  (e/x) a :*: (e/x) b
(e/x) (l :|: r) = 
  (e/x) l :|: (e/x) r
(e/x) (Inl l) = 
  Inl ((e/x) l)
(e/x) (Inr r) = 
  Inr ((e/x) r)
(e/x) (Λ x' b) | x /= x' = 
  Λ x' ((e/x) b)
(e/x) (a :@: b) = 
  (e/x) a :@: (e/x) b
(e/x) (Var x') | x == x' = 
  e
(e/x) u = 
  u

evaluate :: Term -> Term
evaluate (Exl e) = 
  case evaluate e of 
    l :*: _ -> l
    _       -> Exl e
evaluate (Exr e) = 
  case evaluate e of 
    _ :*: r -> r
    _       -> Exr e
evaluate (f :|: g :@: b) =
  case evaluate b of 
    Inl e ::: _ -> evaluate (f :@: e)
    Inr e ::: _ -> evaluate (g :@: e)
    Inl e       -> evaluate (f :@: e)
    Inr e       -> evaluate (g :@: e)
    _           -> f :|: g :@: b
evaluate (f :@: e) = 
  case evaluate f of
    Λ x b ::: _ -> (e/x) b
    Λ x b       -> (e/x) b
    _           -> f :@: e
evaluate t = 
  t
-- END EVALUATION