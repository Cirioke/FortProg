import Type
import Parser

import PrettyPrint
import Variables
import AnonymVars
import Substitution
import Unification
import Renaming
import SLDResolution




parse 
main :: IO ()
main = do 
  <- getLine


helpString :: String
helpString =
  """Commands available from the prompt:
    <goal>      Solves/proves the specified goal.
    :h          Shows this help message.
    :l <file>   Loads the specified file.
    :q          Exits the interactive environment.
    :r          Reloads the last loaded file.
    :s <strat>  Sets the specified search strategy
                where <strat> is one of 'dfs', 'bfs', or 'iddfs'"""

expectoCommando :: IO ()
expectoCommando = do putStr "?- "
                     cmd <- getLine
                     case cmd of 
                       (':':(sc:arg) -> (getDirective sc) parsearg
                       _       ->  



registerCommando :: IO()
registerCommando  "<>"




parse "a(0,B,_),b." :: Either String Goal

parseFile "./PrologTestExamples/anon.pl" :: IO (Either String Prog)