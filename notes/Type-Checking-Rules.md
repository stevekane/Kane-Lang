# Type-checking deductions

    "Universe is a Universe"
    --------
    Γ |- U:U

    "Bottom is a Universe"
    --------
    Γ |- ⊥:U

    "Top is a Universe"
    --------
    Γ |- ⊤:U

    "1 is a Top"
    --------
    Γ |- 1:⊤

    "Product is a Universe"
    Γ   |- τ:U    
    Γ+τ |- σ:U
    --------------
    Γ |- (τ × σ):U

    "Pair is a Product"
    Γ    |- a↓a'
    Γ    |- a:τ    
    Γ+a' |- b:σ
    --------------------
    Γ |- (a * b):(τ × σ)

    "Left projection is a left element"
    Γ |- e:(τ × σ)
    --------------
    Γ |- (π1 e):τ

    "Right projection is a right element"
    Γ |- e:(τ × σ)
    --------------
    Γ |- (π2 e):σ

    "Coproduct is a Universe"
    Γ |- a:U    
    Γ |- b:U
    --------------
    Γ |- (a + b):U

    "Left injection (annotated) is a Coproduct"
    Γ |- τ↓(ρ + σ)
    Γ |- a:ρ
    -----------------------
    Γ |- ((i1 a):τ):(ρ + σ)

    "Right injection (annotated) is a Coproduct"
    Γ |- τ↓(ρ + σ)
    Γ |- b:σ
    -----------------------
    Γ |- ((i2 b):τ):(ρ + σ)

    "Case split A → C and B → C is (A + B) → C"
    Γ |- f:(σ → τ)    
    Γ |- g:(ρ → τ)
    --------------------------
    Γ |- (f | g):((σ + ρ) → τ)

    "Functions are Universes"
    Γ   |- τ:U
    Γ+τ |- σ:U
    --------------
    Γ |- (τ → σ):U

    "Lambda abstractions (annotated) are Functions"
    Γ   |- τ ↓ (ρ → σ)    
    Γ+ρ |- e:σ
    ---------------------
    Γ |- ((λe):τ):(ρ → σ)

    "Function application is the type of the functions codomain"
    Γ |- f:(ρ → σ)    
    Γ |- e:ρ
    --------------
    Γ |- (f e):σ

    "Recursive types are Universes"
    Γ+U |- τ:U
    ------------
    Γ |- (μ.τ):U

    "Recursive binders are Functions"
    Γ+τ |- e:τ
    ----------------
    Γ |- ((μ.e):τ):τ

    "Variables are defined in a context"
    Γ(n):τ
    --------------
    Γ |- (Var n):τ

    "Annotated terms are judgements"
    Γ |- e:τ
    ------------
    Γ |- (e:τ):τ