module PrittyPrint where

import Type

class Pretty a where
  pretty :: a -> String

instance Pretty VarName where
  pretty (VarName a) = a
