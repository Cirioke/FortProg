module Sessioner
  ( Session
  , newSession
  , getAlive
  , getPath
  , getProg
  , getStrategy
  , getStrategyString
  , getDebug
  , getShow
  , setAlive
  , setPath
  , setProg
  , setStrategy
  , setDebug
  , toggleDebug
  , isAlive
  , kill
  ) 
  where

import Type

import PrettyPrint
import SLDResolution

-- /Holds the session state for the interactive prolog environment
data Session = Session 
                Bool             -- is alive
                (Maybe String)   -- programm path
                Prog             -- programm
                String           -- strategy
                Bool             -- debug
  deriving Show

-- Constructors
newSession :: String -> Session
newSession strat = Session True Nothing (Prog []) strat False

-- Getters
getAlive :: Session -> Bool
getAlive (Session alive _ _ _ _) = alive

getPath :: Session -> Maybe String
getPath (Session _ path _ _ _) = path

getProg :: Session -> Prog
getProg (Session _ _ prog _ _) = prog

getStrategy :: Session -> Strategy
getStrategy (Session _ _ _ "dfs" _) = dfs
getStrategy (Session _ _ _ "bfs" _) = bfs
-- getStrategy (Session _ _ _ "iddfs" _) = iddfs
getStrategy (Session _ _ _  _    _) = const []

getStrategyString :: Session -> String
getStrategyString (Session _ _ _ sstr _) = sstr

getDebug :: Session -> Bool
getDebug (Session _ _ _ _ debug) = debug

getShow ::(Show a, Pretty a) => Session -> a -> String
getShow (Session _ _ _ _ True )  = show
getShow (Session _ _ _ _ False)  = pretty

-- Setters
setAlive :: Session -> Bool -> Session
setAlive (Session _   path prog strat debug) new = 
          Session new path prog strat debug

setPath :: Session -> String -> Session
setPath (Session alive _          prog strat debug) new = 
         Session alive (Just new) prog strat debug

setProg :: Session -> Prog -> Session
setProg (Session alive path _   strat debug) new = 
         Session alive path new strat debug

setStrategy :: Session -> String -> Session
setStrategy (Session alive path prog _   debug) new =
             Session alive path prog new debug

setDebug :: Session -> Bool -> Session
setDebug (Session alive path prog strat _   ) new =
          Session alive path prog strat new

toggleDebug :: Session -> Session
toggleDebug (Session alive path prog strat debug      ) =
             Session alive path prog strat (not debug)

-- Alias
isAlive :: Session -> Bool
isAlive = getAlive

kill :: Session -> Session
kill = (flip setAlive) False


instance Pretty Session where
  pretty (Session alive path _ strat debug) = 
    "Programm Path : " ++ show path  ++ "\n\
    \Strategy      : " ++ show strat  ++ "\n\
    \Debug mode    : " ++ show debug  ++ "\n\
    \Is alive      : " ++ show alive

