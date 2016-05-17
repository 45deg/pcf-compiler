module Main where

import Data.Maybe ( fromJust )
import VM
import Compiler


main :: IO ()
main = do
        line <- getLine
        case compileProgram line of
          Just ast -> putStrLn ("Inst: " ++ (show ast)) >>
                      putStrLn ("Result: " ++ (show $ run $ buildState ast))
          Nothing -> putStrLn "Error"
