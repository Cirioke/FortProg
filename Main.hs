import System.IO

import Type
import Parser

import PrettyPrint
import Sessioner
import Substitutions
import SLDResolution



-- -----------------------------------------------------------------------------
-- ********************** MAIN SECTION *****************************************
-- _____________________________________________________________________________

main :: IO ()
main = do putStrLn helloString
          repl (newSession dfs)

-- / Read-Evaluate-Print-Loop function for the commandline Interpreter.
repl :: Session -> IO ()
repl session | (not.isAlive) session = putStrLn "Bye bye."
             | otherwise             =
  do putStr "?- "
     hFlush stdout
     -- read
     inp <- getLine 
     case strip inp of 
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
                  return (setStrategy session dfs)
    "bfs"   -> do putStrLn "Strategy set to breadth-first search."
                  return (setStrategy session bfs)
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
  do putStrLn (show (getPath session))
     return session

-------------------------- CURRENT PROGRAMM ------------------------------------
command 'c' session _    = 
  do putStrLn (pretty (getProg session))
     return session  

-------------------------- NOT DEFINED COMMAND ---------------------------------
-- Error Handling
command c   session _    = 
  putError (':':(c:" is not a defined command.")) session


-------------------------- EVALUATING QUERY ------------------------------------
-- / Try to evaluate the given string as a query within the given session
evalQuery :: String -> Session -> IO Session
evalQuery goalStr session =
  case _goal of
    Right goal     -> nextSol (solveWith programm goal strategy)
    -- Error Handling
    Left  errorStr -> putError ("ParseError: " ++ errorStr) session
  where 
    _goal = parse goalStr :: Either String Goal
    programm = getProg session
    strategy = getStrategy session

    nextSol :: [Subst] -> IO Session
    nextSol []         = do putStrLn "No more solutions."
                            return session
    nextSol (sol:sols) = do putStr (show sol)
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
helpString ="Commands available from the prompt:\n\
            \  <goal>      Solves/proves the specified goal.\n\
            \  :h          Shows this help message.\n\
            \  :l <file>   Loads the specified file.\n\
            \  :q          Exits the interactive environment.\n\
            \  :r          Reloads the last loaded file.\n\
            \  :s <strat>  Sets the specified search strategy\n\
            \              where <strat> is one of 'dfs', 'bfs', or 'iddfs'"

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























