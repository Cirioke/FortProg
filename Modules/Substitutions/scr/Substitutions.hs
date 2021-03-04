module Substitutions 
    ( Subst
    , domain
    , empty
    , single
    , apply
    , compose
    , restrictTo
    , Substitutable
    )
  where

import Test.QuickCheck
import Type
import SetsAsOrderedList (Set, isElem,fromList,toList)
import PrettyPrint
import Variables


-- 1.
-- \ Datatype representing substitutions.
data Subst = Subst [(VarName, Term)]

-- 2.
-- \ Helperfunction returning wether the tuple 
-- represents a identity substitution.
noSelfImage :: (VarName, Term) -> Bool
noSelfImage (a,Var b) = a /= b
noSelfImage (_,_) = True

-- \ Helperfuntion filtering all idetity substitutions out.
filtSelfImage :: Subst -> Subst
filtSelfImage (Subst lst) = Subst (filter noSelfImage lst)

-- \ Returns the variable names that will be effected by the substitution.
domain :: Subst -> [VarName]
domain subst = (\(Subst lst) -> fst (unzip lst)) (filtSelfImage subst)

-- 3.
-- \ Constructor for an the identity substiution.
empty :: Subst
empty = Subst []

-- \ Constructor for a substitution only manipulating one variable.
single :: VarName -> Term -> Subst
single a b = filtSelfImage (Subst [(a, b)])


-- 4.
-- \ A class for objects to whitch a substitution can be applyed
class Vars a => Substitutable a where
  -- \ Apply a substitution to an object (containing variables).
  apply :: Subst -> a -> a

instance Substitutable Term where
  apply (Subst [])         a          = a
  apply a                  (Comb b c) = Comb b (map (apply a) c)  
  apply (Subst ((a, b):c)) (Var d)    = if a == d
                                          then b
                                          else apply (Subst c) (Var d)

instance Substitutable Rule where
  apply s (Rule conc assumps) = Rule (apply s conc) (apply s assumps)

instance Substitutable Prog where
  apply s (Prog rules) = Prog (apply s rules)

instance Substitutable Goal where
  apply s (Goal terms) = Goal (apply s terms)

instance Substitutable a => Substitutable [a] where
  apply s lst = map (apply s) lst


-- 5.
-- \ Composing tow substitutions together in the manner of function composing,
--  so the result is a substition that has the same effect as if the two 
--  components would be applyed one after another.
compose :: Subst -> Subst -> Subst
compose (Subst a) (Subst b) = filtSelfImage (Subst ((filtA a b) ++ (appB a b))) 
 where
  filtA :: [(VarName,Term)] -> [(VarName,Term)] -> [(VarName,Term)]
  filtA la lb = filter (\(x,_) -> not (isElem x (fromList (fst (unzip lb))))) la
  appB :: [(VarName,Term)] -> [(VarName,Term)] -> [(VarName,Term)]
  appB la lb = zip (fst (unzip lb)) (map (apply (Subst la)) (snd (unzip lb)))


  
-- 6.
-- \ Resticting a substitution means reducing the domain
--  to the given list of variable names.
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst lst) names = Subst (filter (\(x,_) -> isElem x (fromList names)) lst)


-- 7.
-- \ Helper function to join a list of string together.
join :: String -> [String] -> String
join _ [] = ""
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
    arity <- elements [1 .. 10]
    domai <- replicateM arity arbitrary :: Gen [VarName]
    image <- replicateM arity arbitrary :: Gen [Term]
    return (Subst (makeSubstList domai image))
   where
    makeSubstList :: [VarName] -> [Term] -> [(VarName, Term)]
    makeSubstList dom img = 
      filter
      (\ (v,t) -> Var v /= t)            -- list must not contain self images
      (zip ((toList.fromList) dom) img)  -- domain must not contain duplicates

    -- \Helper fuction to generate a arbirary list of given lenght
    replicateM :: Int -> Gen c -> Gen [c]
    replicateM 0 _ = do return []
    replicateM m f = do e    <- f
                        rest <- replicateM (m-1) f
                        return (e : rest)

instance Show Subst where
  show = pretty
