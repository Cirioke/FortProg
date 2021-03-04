module Unification where

import Type

import Substitutions

ds :: Term -> Term -> Maybe (Term, Term)
ds t0 t1 = const (Var (VarName "TODO"))

unify :: Term -> Term -> Maybe Subst
unify t0 t1 = const (single (VarName "TODO") (Var (VarName "")))

