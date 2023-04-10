module Main (main) where

import HW3.Parser
import HW3.Evaluator
import HW3.Pretty

import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where 
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do 
                            case parse input of
                                (Left parseErrorBundle) ->  outputStrLn (errorBundlePretty parseErrorBundle)
                                (Right hiExpr) -> do
                                    result <- eval hiExpr
                                    case result of
                                        (Left hiError) -> outputStrLn (show hiError)
                                        (Right hiValue) -> outputStrLn (show (prettyValue hiValue))
                            loop