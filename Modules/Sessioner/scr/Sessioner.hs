module Sessioner
  ( Session
  , newSession
  , getAlive
  , getPath
  , getProg
  , getStrategy
  , setAlive
  , setPath
  , setProg
  , setStrategy
  , isAlive
  , kill
  ) 
  where

import Type
import SLDResolution

-- /Holds the session state for the interactive prolog environment
data Session = Session Bool (Maybe String) Prog Strategy

-- Constructors
newSession :: Strategy -> Session
newSession strat = Session True Nothing (Prog []) strat

-- Getters
getAlive :: Session -> Bool
getAlive (Session alive _ _ _) = alive

getPath :: Session -> Maybe String
getPath (Session _ path _ _) = path

getProg :: Session -> Prog
getProg (Session _ _ prog _) = prog

getStrategy :: Session -> Strategy
getStrategy (Session _ _ _ strat) = strat

-- Setters
setAlive :: Session -> Bool -> Session
setAlive (Session _   path prog strat) new = 
          Session new path prog strat

setPath :: Session -> String -> Session
setPath (Session alive _          prog strat) new = 
         Session alive (Just new) prog strat

setProg :: Session -> Prog -> Session
setProg (Session alive path _   strat) new = 
         Session alive path new strat

setStrategy :: Session -> Strategy -> Session
setStrategy (Session alive path prog _  ) new =
             Session alive path prog new

-- Alias
isAlive :: Session -> Bool
isAlive = getAlive

kill :: Session -> Session
kill = (flip setAlive) False



