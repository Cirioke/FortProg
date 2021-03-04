module Main where

import Type
import PrettyPrint
-- import Renaming
-- import SetsAsOrderedList
-- import SLDResolution
-- import Substitutions
-- import Unification
--  import AnonymVars
import Variables


a :: Term
a = Comb "f" [Var (VarName "A"), Var (VarName "B")]

main :: IO ()
main = do putStrLn (pretty a)
          print (allVars a)
