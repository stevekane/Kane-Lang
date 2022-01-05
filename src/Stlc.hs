{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stlc where

import Control.Monad
import Control.Monad.Fail
import Data.Bool
import Data.Int (Int)
import Data.Maybe
import Data.Either
import Data.String
import Data.List hiding (any, lookup)
import Data.Eq (Eq, (==), (/=))
import Data.Ord ((<),(>),(>=),(<=))
import GHC.Num ((-),(+))


type Variable = Int
type Context = [Term]

data Term where
  U      :: Term
  Bot    :: Term
  Top    :: Term
  T      :: Term
  (:+:)  :: Term -> Term -> Term
  Inl    :: Term -> Term
  Inr    :: Term -> Term
  Case   :: Term -> Term -> Term -> Term
  (:×:)  :: Term -> Term -> Term
  (:*:)  :: Term -> Term -> Term
  Exl    :: Term -> Term
  Exr    :: Term -> Term
  Λ      :: Term -> Term
  (:→:)  :: Term -> Term -> Term
  (:@:)  :: Term -> Term -> Term
  -- Rec    :: Term -> Term
  Var    :: Variable -> Term
  (:::)  :: Term -> Term -> Term
  deriving (Eq)

sub :: Int -> Term -> Term -> Term
sub n e (u ::: τ) =
  sub n e u ::: sub n e τ
sub n e U =
  U
sub n e Bot =
  Bot
sub n e Top =
  Top
sub n e T =
  T
sub n e (τ :×: σ) =
  sub n e τ :×: sub (n+1) e τ
sub n e (l :*: r) =
  sub n e l :*: sub n e r
sub n e (Exl u) =
  Exl (sub n e u)
sub n e (Exr u) =
  Exr (sub n e u)
sub n e (τ :+: σ) =
  sub n e τ :+: sub n e σ
sub n e (Inl u) =
  Inl (sub n e u)
sub n e (Inr u) =
  Inr (sub n e u)
sub n e (Case f g x) =
  Case (sub n e f) (sub n e g) (sub n e x)
sub n e (Λ u) =
  Λ (sub (n+1) e u)
sub n e (τ :→: σ) =
  sub n e τ :→: sub (n+1) e σ
sub n e (f :@: u) =
  sub n e f :@: sub n e u
sub n e (Var n') 
  -- Sub in e while incrementing its bound variables
  | n == n'   = inc n 0 e
  -- This is a "free variable"
  | n' > n    = Var (n'-1)
  -- A bound variable so leave it alone
  | otherwise = Var n'

inc :: Int -> Int -> Term -> Term
inc h n (u ::: τ) =
  inc h n u ::: inc h n τ
inc h n U =
  U
inc h n Bot =
  Bot
inc h n Top =
  Top
inc h n T =
  T
inc h n (τ :×: σ) =
  inc h n τ :×: inc h (n+1) σ
inc h n (l :*: r) =
  inc h n l :*: inc h n r
inc h n (Exl u) =
  Exl (inc h n u)
inc h n (Exr u) =
  Exr (inc h n u)
inc h n (τ :+: σ) =
  inc h n τ :+: inc h n σ
inc h n (Inl u) =
  Inl (inc h n u)
inc h n (Inr u) =
  Inr (inc h n u)
inc h n (Case f g x) =
  Case (inc h n f) (inc h n g) (inc h n x)
inc h n (Λ u) =
  Λ (inc h (n+1) u)
inc h n (τ :→: σ) =
  inc h n τ :→: inc h (n+1) σ
inc h n (f :@: u) =
  inc h n f :@: inc h n u
inc h n (Var n') 
  -- free variable must be incremented to stay free
  | n' >= n   = Var (n'+h)
  -- A bound variable so leave it alone
  | otherwise = Var n'

eval :: Term -> Term
eval U = 
  U
eval Bot = 
  Bot
eval Top = 
  Top
eval T = 
  T
eval (a :+: b) = 
  a :+: b
eval (Inl x) = 
  Inl x
eval (Inr x) = 
  Inr x
eval (Case f g x) = case eval x of
  Inl a -> eval (f :@: a)
  Inr a -> eval (g :@: a)
  _     -> Case f g x
eval (a :×: b) = 
  a :×: b
eval (a :*: b) = 
  a :*: b
eval (Exl x) = case eval x of
  a :*: b -> eval a
  _       -> Exl x
eval (Exr x) = case eval x of
  a :*: b -> eval b
  _       -> Exr x
eval (a :→: b) = 
  a :→: b
eval (Λ e) = 
  Λ e
eval (f :@: e) = case eval f of 
  Λ u -> eval (sub 0 e u)
  _   -> f :@: e
eval (Var n) = 
  Var n
eval (e ::: τ) = case (eval e,eval τ) of
  (a :*: b, σ :×: ρ) -> (a :*: b) ::: (σ :×: sub 0 a ρ)
  (e',τ')            -> e' ::: τ'

infer :: Context -> Term -> Maybe Term
infer γ U = do
  return U
infer γ Bot = do
  return U
infer γ Top = do
  return U
infer γ T = do
  return Top
infer γ (τ :+: σ) = do
  U <- infer γ τ
  U <- infer γ σ
  return U
infer γ (Inl a ::: τ) = do
  ρ :+: σ <- return (eval τ)
  ρ'      <- infer γ a
  True    <- return (ρ == ρ')
  return (ρ :+: σ)
infer γ (Inr b ::: τ) = do
  ρ :+: σ <- return (eval τ)
  σ'      <- infer γ b
  True    <- return (σ == σ')
  return (ρ :+: σ)
infer γ (Case f g x) = do
  α :→: τ  <- infer γ f
  β :→: τ' <- infer γ g
  α :+: β  <- infer γ x
  True     <- return (τ == τ')
  return τ
infer γ (τ :×: σ) = do
  U <- infer γ τ
  U <- infer (τ:γ) σ
  return U
infer γ (a :*: b) = do
  τ <- infer γ a
  σ <- infer γ b
  return (τ :×: σ)
infer γ (Exl t) = do
  τ :×: _ <- infer γ t
  return τ
infer γ (Exr t) = do
  _ :×: σ <- infer γ t
  return σ
infer γ (τ :→: σ) = do
  U <- infer γ τ
  U <- infer (τ:γ) σ
  return U
-- Allows (λx.λy.<x,y>):(T → (T → (T × T)))
infer γ (Λ e@(Λ u) ::: τ@(σ :→: ρ)) = do
  ρ'   <- infer (σ:γ) (e ::: ρ)
  True <- return (ρ == ρ')
  return τ
infer γ (Λ e ::: τ) = do
  ρ :→: σ <- return τ
  σ'      <- infer (ρ:γ) e
  True    <- return (σ == σ')
  return (ρ :→: σ)
infer γ (f :@: e) = do
  ρ :→: σ <- infer γ f
  ρ'      <- infer γ e
  True    <- return (ρ == ρ')
  return σ
infer γ ((a :*: b) ::: (τ :×: σ)) = do
  τ' <- infer γ a
  ρ  <- infer γ b
  let ρ' = sub 0 a σ 
  True <- return (ρ == ρ')
  return (τ :×: ρ)
infer γ (t ::: τ) = do
  τ'   <- infer γ t 
  True <- return (τ == τ')
  return τ
infer γ (Var s) = do
  return (γ !! s)
infer _ _ = 
  fail "Failed to infer type"