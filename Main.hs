import System.IO

import Type
import Parser

import Sessioner
import Substitutions
import SLDResolution



-- -----------------------------------------------------------------------------
-- ********************** MAIN SECTION *****************************************
-- _____________________________________________________________________________

main :: IO ()
main = do putStrLn helloString
          repl (newSession "dfs")

-- / Read-Evaluate-Print-Loop function for the commandline Interpreter.
repl :: Session -> IO ()
repl session | (not.isAlive) session = putStrLn "Bye bye."
             | otherwise             =
  do putStr "?- "
     hFlush stdout
     -- read
     inp <- getLine 
     case strip inp of 
        ""            -> repl session
        -- inp is a command
        (':':(c:arg)) -> do --evaluate and print
                            newSess <- command c session (strip arg) 
                            -- loop
                            repl newSess 
        -- inp is a query
        goalStr       -> do --evaluate and print
                            newSess <- evalQuery goalStr session
                            -- loop
                            repl newSess 
  

-- -----------------------------------------------------------------------------
-- ********************** COMMAND SECTION **************************************
-- _____________________________________________________________________________


-- \ This funktion handles different commands.
-- each command has a character as identifyer.
-- A command might be (or might not be) interessted 
-- in an argument given as a string.
-- The context of the Programm is given in the third Parameter
command :: Char        -- ^identifyer for the command
        -> Session     -- ^Session to execeute the command within
        -> String      -- ^optional argument for the command
        -> IO Session  -- ^command as IO-action itself

-------------------------- QUIT ------------------------------------------------
command 'q' session _    = pure (kill session)

-------------------------- HELP ------------------------------------------------
command 'h' session _    = 
  do putStrLn helpString
     return session

-------------------------- SET STRATEGEY ---------------------------------------
command 's' session sstr =
  case sstr of
    "dfs"   -> do putStrLn "Strategy set to deepth-first search."
                  return (setStrategy session "dfs")
    "bfs"   -> do putStrLn "Strategy set to breadth-first search."
                  return (setStrategy session "bfs")
    -- "iddfs" -> do return (setStrategy session iddfs) -- Bonus exercise
    -- Error Handling
    other   -> putError (other ++ " is not a registered strategy.") session

-------------------------- LOAD PROGRAMM ---------------------------------------
command 'l' session path =
  do _prog <- parseFile path :: IO (Either String Prog)
     case _prog of
       Right prog     -> 
         do s  <- return (setPath session path)
            s' <- return (setProg s       prog)
            putStrLn "Loaded."
            return s'
        -- error Handling
       Left  errorStr -> 
         putError ("Programm could not be parsed: "++ errorStr) session

-------------------------- RELOAD PROGRAMM -------------------------------------
command 'r' session _    = 
  case getPath session of
    (Just path) -> command 'l' session path
    -- error Handling
    Nothing     -> putError ("No Program loaded to reaload.") session

-------------------------- CURRENT PROGRAMM PATH -------------------------------
command 'c' session _    = 
  do putStrLn ((getShow session) (getPath session))
     return session

-------------------------- CURRENT PROGRAMM ------------------------------------
command 'p' session _    = 
  do putStrLn ((getShow session) (getProg session))
     return session

-------------------------- SESSION SETTINGS ------------------------------------
command 'a' session _    = 
  do putStrLn ((getShow session) session)
     return session

-------------------------- TOGGLE DEBUG  ---------------------------------------
command 'd' session _    = 
  do newSess <- return (toggleDebug session)
     putStrLn ("Debug mode is set to " ++ show (getDebug newSess))
     return newSess

-------------------------- ADD RULE --------------------------------------------
command '+' session progStr = 
  case _prog of 
    Right (Prog rules) -> do 
                          putStrLn ("Rules added.")
                          return (setProg session (Prog (currRules ++ rules)))
    -- error handling
    Left  errorStr     ->putError ("Parse error: " ++ errorStr) session
  where _prog = parse progStr
        Prog currRules = getProg session

-------------------------- IGNORE COMMENT --------------------------------------
command '%' session _ = do return session


-------------------------- NOT DEFINED COMMAND ---------------------------------
-- Error Handling
command c   session _    = 
  putError (':':(c:" is not a defined command.\n\
                     \Type \":h\" for help.")) session


-------------------------- EVALUATING QUERY ------------------------------------
-- / Try to evaluate the given string as a query within the given session
evalQuery :: String -> Session -> IO Session
evalQuery goalStr session =
  case _goal of
    Right goal     -> case solveWith programm goal strategy of
                        []   -> do putStrLn "No solutions."
                                   return session
                        sols -> nextSol sols
    -- Error Handling
    Left  errorStr -> putError ("ParseError: " ++ errorStr) session
  where 
    _goal = parse goalStr :: Either String Goal
    programm = getProg session
    strategy = getStrategy session

    nextSol :: [Subst] -> IO Session
    nextSol []         = do putStrLn "No more solutions."
                            return session
    nextSol (sol:sols) = do putStr ((getShow session) sol)
                            hFlush stdout
                            str <- getLine  
                            if strip str == ";" -- TODO Proper condition ?
                              then nextSol sols
                              else do return session


-- -----------------------------------------------------------------------------
-- ********************** HELPER SECTION ***************************************
-- _____________________________________________________________________________

-- / String to welcome user.
helloString :: String
helloString = "Welcome!\n\
               \Type \":h\" for help."

-- / String to be printed when user asks for help.
helpString :: String
helpString =    "Commands available from the prompt:\n\
                \  <goal>      Solves/proves the specified goal.\n\
                \  :h          Shows this help message.\n\
                \  :l <file>   Loads the specified file.\n\
                \  :q          Exits the interactive environment.\n\
                \  :r          Reloads the last loaded file.\n\
                \  :s <strat>  Sets the specified search strategy\n\
                \              where <strat> is one of 'dfs', 'bfs', or 'iddfs'\n\
                \  :c          Shows the current path to the loaded program.\n\
                \  :p          Shows the current loaded programm.\n\
                \  :a          Shows the current session settings.\n\
                \  :d          Toggles the debug mode on or off.\n\
                \              In debug mode output will use show instead of pretty.\n\
                \  :+ <rules>  Adds the rules to the current programm.\n\
                \  :%<comment> This line will be ignored"

-- / Helper Funktion, to trim white spaces on the begin and end of a string.
strip :: String -> String
strip str =  reverse (stripFront (reverse (stripFront str)))
 where
  stripFront :: String -> String
  stripFront (' ':r) = stripFront r
  stripFront ('\t':r) = stripFront r
  stripFront s        = s

-- / Helper Funktion to print errors and keep old state (session).
putError ::  String -> Session -> IO Session
putError errorStr oldSession = do putStrLn errorStr
                                  return oldSession























