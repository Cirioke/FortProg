module SLDTester where

import SLDResolution
import Parser
import Type
import PrettyPrint


data Test = Test String [String]
 deriving (Show)
data Batch = Batch String [Test]
 deriving (Show)



strBatch :: String -> [(String,String)] -> Batch
strBatch file tests = Batch file (interpretes tests)
 where
  interpretes :: [(String,String)] -> [Test]
  interpretes tests = map (\(o, t) -> Test o (split t)) tests 
  split :: String -> [String]
  split str = foldr f [""] str
  f :: Char -> [String] -> [String] 
  f ';' prev = "":prev
  f c (h:r) = (c:h):r

testAll :: Batch -> IO ()
testAll (Batch file tests) = do 
    _prog <- parseFile ("PrologTestExamples/"++ file ++ ".pl") :: IO (Either String Prog)
    case _prog of 
      Right prog     -> 
        do
          putStrLn ("Loaded!")
          testList prog tests
      Left  errorStr -> 
        putStrLn ("Program could not be parsed!: " ++ errorStr)

testList :: Prog -> [Test] -> IO ()
testList prog [] = return ()
testList prog ((Test goalStr target): r) = do 
  case _goal of
    Right goal     -> do 
        putStrLn (show (map pretty (solveWith prog goal dfs)) ++ show ( target))
        testList prog r
    -- Error Handling
    Left  errorStr -> putStrLn ("ParseError: " ++ errorStr)
 where
  _goal = parse goalStr :: Either String Goal

anon = strBatch "anon" [ ("p(A,B).","{A -> a, B -> b}")
                       , ("p(_,_).","{}")
                       , ("q.","{}")]

arith = strBatch "arith" [ ("is(42,+(21,21)).","{}")
                         , ("is(+(21,21),42).","")
                         , ("is(+(21,21),+(21,21)).","")
                         , ("=(+(21,21),+(21,21)).","{}")
                         , ("=(+(21,21),42).","")
                         , ("=(42,42).","{}")
                         , ("is(_,mod(10,0)).","")
                         , ("factorial(0,F).","{F -> 1}")
                         , ("factorial(N,F).","{N -> 0, F -> 1}")
                         , ("factorial(30,F).","{F -> 265252859812191058636308480000000}")]
