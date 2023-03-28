module Main where

import Control.Monad.State
import System.IO

import GameState
import GameIO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    s <- execStateT opening initialState
    evalStateT (forever repl) s
