import Type
import Parser

import Session
import Substitutions
import SLDResolution



-- -----------------------------------------------------------------------------
-- ********************** MAIN SECTION *****************************************
-- _____________________________________________________________________________

main :: IO ()
main = do putHello
          repl defaultSession

-- / Read-Evaluate-Print-Loop function for the commandline Interpreter.
repl :: Session -> IO ()
repl session | (not.isAlive) session = putStrLn "Bye bye."
             | otherwise             =
  do putStr "?- "
    -- read
    inp <- getLine 
    case strip inp of 
      -- inp is a command
      (':':(c:arg)) -> do --evaluate and print
                          newSession <- command c session (strip arg) 
                          -- loop
                          repl newSession 
      -- inp is a query
      goalStr       -> do evalQuery goalStr session
                          repl session
  

-- -----------------------------------------------------------------------------
-- ********************** COMMAND SECTION **************************************
-- _____________________________________________________________________________


-- \ This funktion handles different commands.
-- each command has a character as identifyer.
-- A command might be (or might not be) interessted 
-- in an argument given as a string.
-- The context of the Programm is given in the third Parameter
command :: Char -> String -> Session -> IO Session

-------------------------- QUIT ------------------------------------------------
command 'q' _ session    = pure (kill session)

-------------------------- HELP ------------------------------------------------
command 'h' session _    = 
  do putStrLn helpString
     return session

-------------------------- SET STRATEGEY ---------------------------------------
command 's' session sstr = do handle getStrategy sstr
  case sstr of
    "dfs"   -> do return (setStrategy session dfs)
    "bfs"   -> do return (setStrategy session bfs)
    "iddfs" -> do return (setStrategy session iddfs)
    -- Error Handling
    other   -> putError (other ++ " is not a registered strategy.") session

-------------------------- LOAD PROGRAMM ---------------------------------------
command 'l' session path =
  do _prog <- parseFile path :: IO (Either String Prog)
     case _prog of
       Right prog     -> 
         do s  <- return (setPath session path)
            s' <- return (setProg s       prog)
            return s'
        -- error Handling
       Left  errorStr -> 
         putError ("Programm could not be parsed: "++ errorStr) session

-------------------------- RELOAD PROGRAMM -------------------------------------
command 'r' session _    = getCommand 'l' session (getPath session)

-------------------------- NOT DEFINED COMMAND ---------------------------------
-- Error Handling
command c   session _    = 
  putError (':':(c:" is not a defined command.")) session


-------------------------- EVALUATING QUERY ------------------------------------
-- / Try to evaluate the given string as a query within the given session
evalQuery :: String -> Session -> IO ()
evalQuery goalStr session =  do
  either handleError continue 
  nextSol
  case _goal of
    Right goal     -> nextSol (solveWith programm goal strategy)
    -- Error Handling
    Left  errorStr -> putError ("ParseError: " ++ errorStr) session
  where 
    _goal = parse goalStr :: Either String Goal
    programm = getProg session
    strategy = getStrat session

    nextSol :: [Subst] -> IO ()
    nextSol []         = putStrLn "No more solutions."
    nextSol (sol:sols) = do putStr (show sol)
                            str <- getLine  
                            if strip str == ";" -- TODO Proper condition ?
                              then nextSol sols


-- -----------------------------------------------------------------------------
-- ********************** HELPER SECTION ***************************************
-- _____________________________________________________________________________

-- / String to welcome user.
helloString :: String
helloString = """Welcome!
                 Type \":h\" for help."""

-- / String to be printed when user asks for help.
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

-- / Helper Funktion, to trim white spaces on the begin and end of a string.
strip :: String -> String
strip str =  reverse (stripFront (reverse (stripFront str)))
 where
  stripFront :: String -> String
  stripFront (' ':r) = stripFront r
  stripFront ('\t':r) = stripFront r
  stripFront str      = str

-- / Helper Funktion to print errors and keep old state (session).
putError ::  String -> Session -> IO Session
putError errorStr oldSession = do putStrLn errorStr
                                  return oldSession























