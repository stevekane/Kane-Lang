This is what a partially applied pair would then look like.

  Pair a : (B → (A × B))

This is what a partially applied case expression would look like.

  Case f g : ((A + B) → C)

This is possibly not necessary though as you can always elect
to construct these functions yourself via lambda abstractions.

  let pair be (λa.λb.<a,b>):(A → B → (A × B)) in MY_PROGRAM...

IMPORTANT!!!!!

The program above currently does not type-check because every
λ must come with its own annotation currently even when it is
immediately followed by another λ. I think it should be possible
to only annotate the single "outer" function but this requires
type-checking of a function-like thing to be recursive until
you encounter a non-λ term.