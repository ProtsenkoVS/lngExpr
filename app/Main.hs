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
                      Just st -> do liftIO $ interpretIO st   
                                    loop 

interpretIO :: String -> IO()
interpretIO input = CE.catch (putStrLn (interpret input)) hError

hError :: CE.ErrorCall -> IO()
hError (CE.ErrorCallWithLocation se _)  = putStrLn se
