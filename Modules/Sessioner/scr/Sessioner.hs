module Sessioner
  ( Session
  , getAlive
  , getPath
  , getProg
  , getStrategy
  , setAlive
  , setPath
  , setProg
  , setStrategy
  ) 
 where

import Type
import SLDResolution


data Session = Session Bool String Prog Strategy


getAlive    :: Session -> Bool
getAlive    (Session alive _    _    _    ) = alive

getPath     :: Session -> String
getPath     (Session _     path _    _    ) = path

getProg     :: Session -> Prog
getProg     (Session _     _    prog _    ) = prog

getStrategy :: Session -> Strategy
getStrategy (Session _     _    _    strat) = strat


setAlive    :: Session -> Bool -> Session
setAlive    (Session alive path prog strat) new = (Session new path prog strat)

setPath     :: Session -> String -> Session
setPath     (Session alive path prog strat) new = (Session alive new prog strat)

setProg     :: Session -> Prog -> Session
setProg     (Session alive path prog strat) new = (Session alive path new strat)

setStrategy :: Session -> Strategy -> Session
setStrategy (Session alive path prog strat) new = (Session alive path prog new)


isAlive = getAlive
kill = (flip setAlive) False



