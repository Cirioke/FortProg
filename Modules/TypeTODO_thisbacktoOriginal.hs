module Type
  ( VarName(VarName), CombName, Term(Var, Comb), Rule(Rule), Prog(Prog)
  , Goal(Goal)
  ) where

import Control.Monad

import Test.QuickCheck

-- Data type for variable names
data VarName = VarName String
  deriving (Eq, Ord, Show)

-- Generator for variable names
instance Arbitrary VarName where
  arbitrary = VarName <$> elements ["A", "B", "_0", "_"]

-- Alias type for combinators
type CombName = String

-- Data type for terms
data Term = Var VarName | Comb CombName [Term]
  deriving (Eq, Show)

-- Generator for terms
instance Arbitrary Term where
  arbitrary = do
    arity <- choose (0, 2)
    frequency [ (2, Var <$> arbitrary)
              , (3, Comb <$> elements ["f", "g"] <*> replicateM arity arbitrary)
              ]

-- Data type for program rules
data Rule = Rule Term [Term]
  deriving Show

-- Generator for rules
instance Arbitrary Rule where
  arbitrary =
    Rule <$> arbitrary <*> (choose (0, 2) >>= \n -> replicateM n arbitrary)

-- Data type for programs
data Prog = Prog [Rule]
  deriving Show

-- Data type for goals
data Goal = Goal [Term]
  deriving Show