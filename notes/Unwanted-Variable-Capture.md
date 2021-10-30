On un-wanted variable capture during substitution:

  [[ p[t/x] ]] = "substitute every occurrence of x in p with t"

If you have quantifiers such as this:

  p = ∀y.x + y = 3
  t = y + y
  p[t/x] = ∀y.y + y + y = 3

Our intended "free variable" y in t was "captured" when it
was substituted under the binder in p. Now the meaning of the
resulting expression quantifies over the locally-bound variable
y which changes the meaning of the expression.

This is solvable by α-conversion of p such that its meaning 
is un-changed. This requires you to observe the bound variables
in t and convert the bound variables and occurrences in p 
to avoid conflicting with these names.

  free_variables(t) = {y}
  free_variables(p) = {x}
  bound_variables(p) = {y}
  bound_variables(p[t/x]) = {y}
  free_variables(p[t/x]) = ∅
  bound_variables(p) ∩ free_variables(t) /= ∅ 
  ∴
  p[y/z] = ∀z.x + z = 3
  t = y + y
  p[y/z][t/x] = ∀z.y + y + z = 3

  free_variables(p[t/x]) = {x}
  bound_variables(p[t/x]) = ∅

We have avoided capture by finding suitable variables under
α-conversion such that the bound and free variables of the 
two expressions do not vary.