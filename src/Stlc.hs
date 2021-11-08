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
import Prelude ((-), (+), (>), (<), (>=), (<=))


type Variable = Int
type Context = [Term]

data Term where
  U      :: Term
  Bot    :: Term
  Top    :: Term
  T      :: Term
  (:×:)  :: Term -> Term -> Term
  (:*:)  :: Term -> Term -> Term
  Exl    :: Term -> Term
  Exr    :: Term -> Term
  (:+:)  :: Term -> Term -> Term
  Inl    :: Term -> Term
  Inr    :: Term -> Term
  (:|:)  :: Term -> Term -> Term
  Λ      :: Term -> Term
  (:→:)  :: Term -> Term -> Term
  (:@:)  :: Term -> Term -> Term
  -- Rec    :: Term -> Term
  Var    :: Variable -> Term
  (:::)  :: Term -> Term -> Term
  deriving (Eq)

eval :: Term -> Term
eval (f :@: e) = case f of 
  Λ u -> sub 0 e u
  _   -> f :@: e
eval e = e

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
sub n e (f :|: g) =
  sub n e f :|: sub n e g
sub n e (Λ u) =
  Λ (sub (n+1) e u)
sub n e (τ :→: σ) =
  sub n e τ :→: sub (n+1) e σ
sub n e (f :@: u) =
  sub n e f :@: sub n e u
sub n e (Var n') 
  | n == n'   = inc n 0 e  -- Sub in e while incrementing its bound variables
  | n' > n    = Var (n'-1) -- This is a "free variable"
  | otherwise = Var n'     -- A bound variable so leave it alone

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
inc h n (f :|: g) =
  inc h n f :|: inc h n g
inc h n (Λ u) =
  Λ (inc h (n+1) u)
inc h n (τ :→: σ) =
  inc h n τ :→: inc h (n+1) σ
inc h n (f :@: u) =
  inc h n f :@: inc h n u
inc h n (Var n') 
  | n' >= n   = Var (n'+h) -- free variable must be incremented to stay free
  | otherwise = Var n'     -- A bound variable so leave it alone

infer :: Context -> Term -> Maybe Term
infer γ U = do
  return U

infer γ Bot = do
  return U

infer γ Top = do
  return U
infer γ T = do
  return Top

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

infer γ (τ :+: σ) = do
  U <- infer γ τ
  U <- infer γ σ
  return U
infer γ (Inl a ::: τ) = do
  ρ :+: σ <- return τ
  ρ'      <- infer γ a
  True    <- return (ρ == ρ')
  return (ρ :+: σ)
infer γ (Inr b ::: τ) = do
  ρ :+: σ <- return τ
  σ'      <- infer γ b
  True    <- return (σ == σ')
  return (ρ :+: σ)
infer γ (a :|: b) = do
  α :→: τ  <- infer γ a
  β :→: τ' <- infer γ b
  True     <- return (τ == τ')
  return ((α :+: β) :→: τ)

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

infer γ (Var s) = do
  return (γ !! s)

infer γ ((a :*: b) ::: (τ :×: σ)) = do
  τ' <- infer γ a
  ρ  <- infer γ b
  ρ' <- infer (a:γ) σ
  True <- return (ρ == ρ')
  return (τ :×: ρ)

infer γ (t ::: τ) = do
  τ'   <- infer γ t 
  True <- return (τ == τ')
  return τ

infer _ _ = 
  fail "Failed to infer type"