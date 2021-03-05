module AnonymVars 
    ( AnonymVars
    , isNamed
    , isAnonym
    , nameAnonym
    , unnameAnonym
    , freshAnonym
    ) 
  where

import Type

import Variables


-- \ Returns a version of a variable marked as anonym.
makeNamed :: VarName -> VarName
makeNamed (VarName str) = VarName ("0%$" ++ str)

-- \ Returns wether a variable is a named anonym variable.
isNamed :: VarName -> Bool
isNamed (VarName ('0':('%':('$':_))) ) = True
isNamed (VarName _                   ) = False

-- \ returns wether a variable is a named or unnamed anonym Variable
isAnonym :: VarName -> Bool
isAnonym v =   v == (VarName "_") 
            || isNamed v

-- \ A infinite list for new anonym variables.
freshAnonym::[VarName]
freshAnonym = map makeNamed freshVars

-- \ A class for objects where variable renaming is sensable.
class (Vars a) => AnonymVars a where
  -- \ Relplace all underscore variables by a unique anonyme variable
  nameAnonym :: a -> a 
  nameAnonym = _nameAnonym "0"

  -- \ Replace all underscores by 
  -- (anonyme) variables starting with a given prefix.
  _nameAnonym :: String  -- ^ Prefix to use for naming underscores
              -> a       -- ^ Object where to replace underscores
              -> a       -- ^ Object with named anoym variables

  -- \ Replace all anonym variables within by underscores.
  unnameAnonym :: a -> a

 

instance AnonymVars Term where
  _nameAnonym prefix (Var (VarName "_") ) = Var (makeNamed (VarName prefix))
  _nameAnonym _      (Var  vName        ) = Var vName
  _nameAnonym prefix (Comb cName      ts) = Comb cName (_nameAnonym prefix ts)  
    
  unnameAnonym var@(Var vName) | isNamed vName = Var (VarName "_")
                               | otherwise      = var
  unnameAnonym (Comb cName terms) = Comb cName (unnameAnonym terms)


instance AnonymVars Rule where
  _nameAnonym p (Rule conc assumps) =
    Rule (_nameAnonym (p ++ ".c") conc) (_nameAnonym (p ++ ".a") assumps)

  unnameAnonym (Rule conc assumps) = 
    Rule (unnameAnonym conc) (unnameAnonym assumps)


instance AnonymVars Prog where
  _nameAnonym p (Prog rules) = Prog (_nameAnonym (p ++ ".p") rules)

  unnameAnonym (Prog rules) = Prog (unnameAnonym rules)


instance AnonymVars Goal where
  _nameAnonym p (Goal terms) = Goal (_nameAnonym (p ++ ".g") terms)

  unnameAnonym (Goal terms) = Goal (unnameAnonym terms)


instance AnonymVars a => AnonymVars [a] where
  _nameAnonym prefix = zipWith _nameAnonym nodeNames
    where nodeNames = (map ((prefix ++ ).(show :: Integer -> String)) [0..])    

  unnameAnonym = map unnameAnonym