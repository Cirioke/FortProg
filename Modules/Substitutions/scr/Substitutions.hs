module Substitutions 
    ( Subst
    , domain
    , empty
    , single
    , apply
    , compose
    , restrictTo
    )
  where

import Test.QuickCheck
import Type
import SetsAsOrderedList (Set, isElem,fromList,toList)
import PrettyPrint
import Variables


-- 1.
data Subst = Subst [(VarName, Term)]
  
-- 2.
filtSelfImage :: Subst -> Subst
filtSelfImage (Subst lst) = Subst (filter noSelfImage lst)
 where
  noSelfImage :: (VarName, Term) -> Bool
  noSelfImage (a,Var b) = a /= b
  noSelfImage (_,_) = True

domain :: Subst -> [VarName]
domain subst = (\(Subst lst) -> fst (unzip lst)) (filtSelfImage subst)


-- 3.
empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single a b = filtSelfImage (Subst [(a, b)])


-- 4.
apply :: Subst -> Term -> Term
apply (Subst [])         a          = a
apply a                  (Comb b c) = Comb b (map (apply a) c)  
apply (Subst ((a, b):c)) (Var d)    = if a == d
                                        then b
                                        else apply (Subst c) (Var d)


-- 5.
compose :: Subst -> Subst -> Subst
compose (Subst a) (Subst b) = filtSelfImage (Subst ((filtA a b) ++ (appB a b)))
 where
  filtA :: [(VarName,Term)] -> [(VarName,Term)] -> [(VarName,Term)]
  filtA a b = filter (\(x,_) -> not (isElem x (fromList (fst (unzip b))))) a
  appB :: [(VarName,Term)] -> [(VarName,Term)] -> [(VarName,Term)]
  appB a b = zip (fst (unzip b)) (map (apply (Subst a)) (snd (unzip b)))

  


-- 6.
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst lst) names = Subst (filter (\(x,_) -> isElem x (fromList names)) lst)


-- 7.
join :: String -> [String] -> String
join s [] = ""
join s (h:l) = h ++ foldl (++) "" (map (\x -> s++x) l)

instance Pretty Subst where
  pretty s = "{" ++ intern (filtSelfImage s) ++ "}"
   where
    intern :: Subst -> String
    intern (Subst l) = join ", " (map str l) 
    str :: (VarName,Term) -> String
    str (VarName n,t) = n ++ " -> " ++ (pretty t)



-- 8.
instance Vars Subst where
  allVarsSet (Subst substRules) = allVarsSet substRules


-- 9.
instance Arbitrary Subst where
  arbitrary = do
    arity  <- elements [1 .. 10]
    domain <- replicateM arity arbitrary :: Gen [VarName]
    image  <- replicateM arity arbitrary :: Gen [Term]
    return (Subst (makeSubstList domain image))
   where
    makeSubstList :: [VarName] -> [Term] -> [(VarName, Term)]
    makeSubstList dom img = 
      filter
      (\ (v,t) -> Var v /= t)           -- list must not contain self images
      (zip ((toList.fromList) dom) img)   -- domain must not contain duplicates

    replicateM :: Int -> Gen c -> Gen [c]
    replicateM 0 _ = do return []
    replicateM m f = do elem <- f
                        rest <- replicateM (m-1) f
                        return (elem : rest)

instance Show Subst where
  show = pretty
