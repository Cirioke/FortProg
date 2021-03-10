module Sessioner where

--   ( Session
--   , getAlive
--   , getPath
--   , getProg
--   , getStrategy
--   , setAlive
--   , setPath
--   , setProg
--   , setStrategy
--   , isAlive
--   , kill
--   ) 


import Type
import SLDResolution

-- /Holds the session state for the interactive prolog environment
data Session = Session Bool String Prog Strategy

-- Getters
getAlive :: Session -> Bool
getAlive (Session alive _ _ _) = alive

getPath :: Session -> String
getPath (Session _ path _ _) = path

getProg :: Session -> Prog
getProg (Session _ _ prog _) = prog

getStrategy :: Session -> Strategy
getStrategy (Session _ _ _ strat) = strat

-- Setters
setAlive :: Session -> Bool -> Session
setAlive (Session alive path prog strat) new = (Session new path prog strat)

setPath :: Session -> String -> Session
setPath (Session alive path prog strat) new = (Session alive new prog strat)

setProg :: Session -> Prog -> Session
setProg (Session alive path prog strat) new = (Session alive path new strat)

setStrategy :: Session -> Strategy -> Session
setStrategy (Session alive path prog strat) new = (Session alive path prog new)

-- Alias
isAlive = getAlive
kill = (flip setAlive) False



