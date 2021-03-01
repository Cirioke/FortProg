module Main where

import Type

import Variables

a :: Term
a = Comb "f" [Var (VarName "A"), Var (VarName "B")]

main :: IO ()
main = do putStrLn (show a)
          print (allVars a)
