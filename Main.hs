module Main where

import Type
import PrettyPrint
import Renaming
import SetsAsOrderedList
--import SLDResolution
import Substitutions
import Unification
import AnonymVars
import Variables


set :: SetsAsOrderedList.Set Int
set = SetsAsOrderedList.empty

subst :: Subst
subst = empty

anonym :: Bool
anonym = isAnonym (VarName "XZY")

term :: Term
term = Comb "f" [Var (VarName "A"), Var (VarName "B")]

renamed_term::Term
renamed_term = rename [] term

unificator :: Maybe Subst
unificator = unify term renamed_term

main :: IO ()
main = do putStrLn (pretty renamed_term)
          print (allVars renamed_term)
