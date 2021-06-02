module Main where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as CE
import IntgExpr

main :: IO()
main = runInputT defaultSettings loop 
   where  loop :: InputT IO()
          loop = do input <- getInputLine "lngExpr>" 
                    case input of 
                      Nothing -> return ()
                      Just "" -> return ()
                      Just st -> do liftIO $ interpretIO st   --outputStrLn "isGood"
                                    loop 


interpretIO :: String -> IO()
interpretIO input = CE.catch (putStrLn (interpret input)) hError


hError :: CE.ErrorCall -> IO()
hError (CE.ErrorCallWithLocation se _)  = putStrLn se

interpret :: String -> String
interpret st = let e = parseExpr st 
               in show $ eExpr e  

{-
-- work without Haskeline
import System.IO
main :: IO()
main = do hSetBuffering stdout NoBuffering
          loop 

loop :: IO()
loop = do
         putStr "lngExpr>" 
         input <- getLine
         if null input then return () 
            else do catch (putStrLn (interpret input)) hError  
                    loop 

hError :: ErrorCall -> IO()
hError (ErrorCallWithLocation se _)  = putStrLn se
-}





{-  prog SP file i1 ... ik
         P = 'P' - parser + do notation
		     'L' - library + aplicative
         S = 'W' - Work - parameter
             'S' - State 
             'A' - State + aplicative 
         file - file with program 
         i1 ... ik - input data		 
-}
{-
main :: IO ()
main = do sx <- getArgs
          if length sx > 1 then do 
             s <- head (head sx)
             p <- if (null . tail . head) sx then 'P' else (head sx)!!1
             fl <- readFile $ sx!!1
             il <- tail (tail sx) 
             case parseSPL fl of 
               Left  r -> print r
               Right p -> print $ iProgram p il
          else print "Parameters only " ++ (show (length sx))			   
 -}

