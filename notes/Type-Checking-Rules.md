# Type-checking deductions

## Universe

    "Universe is a Universe"
    Γ |- U:U

## Bottom

    "Bottom is a Universe"
    Γ |- ⊥:U

## Top

    "Top is a Universe"
    Γ |- ⊤:U

    "1 is a Top"
    Γ |- 1:⊤

## Coproduct

    "Coproduct is a Universe"
    Γ |- a:U    
    Γ |- b:U
    Γ |- (a + b):U

    "Left injection (annotated) is a Coproduct"
    Γ |- τ ↓ (ρ + σ)
    Γ |- a:ρ
    Γ |- ((i1 a):τ):(ρ + σ)

    "Right injection (annotated) is a Coproduct"
    Γ |- τ ↓ (ρ + σ)
    Γ |- b:σ
    Γ |- ((i2 b):τ):(ρ + σ)

    "Case split has common codomain's type"
    Γ |- f:(σ → τ)    
    Γ |- g:(ρ → τ)
    Γ |- e:(σ + ρ)
    Γ |- (f | g) e:τ

    "Computation by first case"
    (f | g) (i1 a) ↓ f a

    "Computation by second case"
    (f | g) (i2 b) ↓ g b

## Product

    "Product is a Universe"
    Γ   |- τ:U    
    Γ+τ |- σ:U
    Γ   |- (τ × σ):U

    "Pair is a Product"
    Γ |- a:τ    
    Γ |- b:σ
    Γ |- (a * b):(τ × σ)

    "Left projection"
    Γ |- e:(τ × σ)
    Γ |- (π1 e):τ

    "Right projection"
    Γ |- e:(τ × σ)
    Γ |- (π2 e):σ

    "Computation by left projection"
    Γ |- (π1 (a * b)) ↓ a
    
    "Computation by right projection"
    Γ |- (π2 (a * b)) ↓ b

## Function

    "Functions are universes"
    Γ    |- ρ ↓ ρ'
    Γ    |- ρ:U
    Γ+ρ' |- σ:U
    Γ    |- ρ → σ:U

    "Abstraction is a Function"
    Γ    |- ρ ↓ ρ'
    Γ+ρ' |- e:σ
    Γ    |- λe:(ρ → σ)

    "Application is the codomain"
    Γ    |- f:(ρ → σ)
    Γ    |- e:ρ
    Γ    |- σ[e/0] ↓ τ
    Γ    |- (f e):τ

    "Function computation via application"
    Γ |- f ↓ λu
    Γ |- e ↓ e'
    Γ |- (f e) ↓ u[e'/0]

## Recursion

    "Recursive types are Universes"
    Γ+U |- τ:U
    Γ   |- (μ.τ):U

    "Recursive binders are Functions"
    Γ+τ |- e:τ
    Γ   |- ((μ.e):τ):τ

## Variable

    "Variables are defined in a context"
    Γ(n):τ
    Γ |- (Var n):τ

## Annotation

    "Annotated terms are judgements"
    Γ |- e:τ
    Γ |- (e:τ):τ