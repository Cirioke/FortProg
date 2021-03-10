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
  line <- getLine
  case line of 
    (':'('h':arg) -> do putStrLn helpString
    (':'('l':arg) -> do prog <- parseFile (trimWitheSpace arg) :: IO (Either String Prog)
                        case prog of
                          Left  e <- putStrLn edges
                          Right p <- expectoCommando (trimWitheSpace arg, p)
    (':'('q':arg) -> do putStrLn "Bye…"
    (':'('r':arg) -> do prog <- parseFile currProgPath :: IO (Either String Prog)
                        case prog of
                          Left  e <- putStrLn edges
                          Right p <- expectoCommando (currProgPath, p)
    (':'('s':arg) -> do
    _             -> case (parse line :: Either String Goal) of
                       
                       
data Session = Session 
                 Prog
                 String
                 Strategy

instance Monad Session

helpString :: String
helpString =
 "Commands available from the prompt:
    <goal>      Solves/proves the specified goal.
    :h          Shows this help message.
    :l <file>   Loads the specified file.
    :q          Exits the interactive environment.
    :r          Reloads the last loaded file.
    :s <strat>  Sets the specified search strategy
                where <strat> is one of 'dfs', 'bfs', or 'iddfs'"


expectoCommando :: Session -> IO ()
expectoCommando session = 
  do putStr "?- "
     cmd <- getLine
     case cmd of 
       (':':(sc:arg) -> do newSession <- (getDirective session sc) (strip arg)
                           expectoCommando newSession
        goal         -> do s <- directiveSolve session goal


executeDirective :: IO ()
executeDirective session = 
  do newSession <- (getDirective session sc) (strip arg)
     expectoCommando newSession

registerDirective :: String -> IO()
registerDirective  "<>"

getDirective :: Char -> String -> Session -> IO Session
getDirective 


directiveHelp :: String -> Session -> IO Session
directiveLoad :: String -> Session ->IO Session
directiveQuit :: String -> Session ->IO Session
directiveReload :: String -> Session ->IO Session
directiveStrategy :: String -> Session ->IO Session


parse "a(0,B,_),b." :: Either String Goal

parseFile "./PrologTestExamples/anon.pl" :: IO (Either String Prog)















-- / Helper Funktion
strip :: String -> String
strip str =  reverse (stripFront (reverse (stripFront str)))
 where
  stripFront :: String -> String
  stripFront (' ':r) = stripFront r
  stripFront ('\t':r) = stripFront r
  stripFront str      = str

-- / A Session for the commandline interpreter
data Session = Session 
                 Maybe String  -- ^Path to current Program
                 Prog          -- ^Current loaded Programm
                 Strategy      -- ^Evaluating strategy
                 Bool          -- ^is alive

startSession :: Session
startSession = Session Nothing (Prog []) dfs

main :: IO ()
main = do printHallo
          repl startSession


repl :: Maybe Session -> IO ()
repl Nothing = putStrLn "Bye bye."
repl Just  s = do putStr "?- "
                inp <- getLine -- read
                case strip inp of 
                  -- inp is a command
                  (':':(c:arg)) -> do newSession <- getCommand c (strip arg) session  --evaluate and print
                                      repl newSession -- loop
                  -- inp is a query
                  goalStr       -> do evalQuery goalStr session
                                      repl s
                  
getCommand :: Char -> String -> Session -> IO Session
getCommand 'q' _ _ = 



evalQuery :: String -> Session -> IO ()
evalQuery goalStr session@(Session _ prog strat) = nextSol solutions
  where 
    _goal = parse goalStr :: Either String Goal -- TODO Fehlerbehandlung
    case _goal of
      Left  errorStr   -> …
      Right (Goal lst) -> …
    solutions = solveWith prog goal strat

    nextSol :: [Subst] -> IO ()
    nextSol []         = putStrLn "No more solutions."
    nextSol (sol:sols) = do putStr (show sol)
                            str <- getLine  
                            if strip str == ";" -- TODO Proper condition ?
                              then nextSol sols
                              else return ()


              





































