module PrettyPrinting where

import Stlc

-- BEGIN PRINTING
binary ol l m r or = 
  ol ++ show l ++ m ++ show r ++ or

between ol m or = 
  ol ++ show m ++ or

instance Show Term where
  show U =
    "U"
  show Bot = 
    "Bot"
  show Top = 
    "Top"
  show T = 
    "T"
  show (τ :×: σ) = 
    binary "(" τ " x " σ ")"
  show (l :*: r) = 
    binary "(" l " * " r ")"
  show (Exl l) = 
    between "exl(" l ")"
  show (Exr r) = 
    between "exr(" r ")"
  show (τ :+: σ) = 
    binary "(" τ " + " σ ")"
  show (Inl l) = 
    between "inl(" l ")"
  show (Inr r) = 
    between "inr(" r ")"
  show (Case f g x) = 
    binary "[" f " | " g "] " ++ show x
  show (τ :→: σ) = 
    binary "" τ " -> " σ ""
  show (f :@: e) = 
    binary "(" f " " e ")"
  -- show (Rec e) =
  --  "μ" ++ show e
  show (Λ e) = 
    "fn." ++ show e
  show (Var s) = 
    show s
  show (t ::: τ) = 
    binary "" t ":" τ ""
-- END PRINTING
