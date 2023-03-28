module GameIO where

import Control.Monad.State
import System.Exit

import GameState
import Room
import Command
import Item

type GameIO t = StateT GameState IO t 

effectChange :: (GameState -> GameState) -> GameIO ()
effectChange = modify

prompt :: GameIO ()
prompt = lift $ putStr "-> "

printMessage :: GameIO ()
printMessage = do 
    s <- get 
    put $ s { message = Nothing }
    case message s of 
        Just m -> lift $ putStrLn m 
        Nothing -> pure ()

printDescription :: GameIO ()
printDescription = do 
    s <- get
    case (isBright s) of 
        True -> lift $ putStrLn $ desc (currentRoom s)
        False -> lift $ putStrLn "You are in a dark room."

printObjects :: GameIO ()
printObjects = do 
    s <- get 
    case (isBright s) of
        True -> do 
            let r = currentRoom s 
            let os = objects r 
            lift $ putStrLn "You see the following objects:"
            void $ mapM (\itemName -> lift $ putStrLn $ show itemName) os
        False -> 
            lift $ putStrLn "It is too dark to see any items in the room."

printExits :: GameIO ()
printExits = do 
    s <- get 
    let ex = exits $ currentRoom s
    lift $ putStrLn "There are exits in the following directions:"
    void $ mapM (\exit -> lift $ putStrLn $ show (fst exit)) ex

printInventory :: GameIO ()
printInventory = do
    s <- get 
    case (currentInventory s) of 
        [] -> lift $ putStrLn "You aren't carrying anything."
        _ -> do
            lift $ putStrLn "You are carrying the following objects:"
            void $ mapM (\item -> lift $ putStrLn $ show item) $ currentInventory s

actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList f items = do 
    void $ mapM helper $ items
        where
            helper item = do 
                effectChange (f item)
                printMessage

finishLevel :: GameIO ()
finishLevel = do 
    lift $ putStrLn "You successfully brought the jug into the yard.\nCongrats! You won the level!"

finishGame :: GameIO ()
finishGame = do 
    lift $ putStrLn "You successfully brought the jug into the yard again.\nCongrats! You win!"
        >> exitSuccess

checkGameOver :: GameIO ()
checkGameOver = do 
    s <- get
    case (haveWonGame s) of 
        True -> 
            case (level s) of 
                1 -> finishLevel >> level2Opening >> put level2State
                _ -> finishGame
        False -> pure ()

syntaxError :: GameIO () 
syntaxError = lift $ putStrLn "I don't understand that."

opening :: GameIO ()
opening = lift $ putStrLn "Welcome to Functional Adventure!"

level2Opening :: GameIO ()
level2Opening = lift $ putStrLn "You are suddenly transported to another set of rooms."

performCommand :: Command -> GameIO ()
performCommand cmd = 
    case cmd of 
        Look -> do 
            printDescription
            printObjects
            printExits 
        Move dir -> do
            effectChange (move dir)
            printMessage 
        Inventory -> printInventory 
        Take items -> actionOverList takeItem items 
        Drop items -> actionOverList dropItem items 
        Exit -> lift $ putStrLn "Goodbye!" >> exitSuccess

performConjunction :: Conjunction -> GameIO ()
performConjunction cnj = void $ mapM performCommand $ cnj

parseConjunction :: String -> GameIO ()
parseConjunction str = 
    case parseInput str of 
        Nothing -> syntaxError 
        Just cnj -> performConjunction cnj

repl :: GameIO ()
repl = do 
    prompt 
    line <- lift getLine 
    parseConjunction line
    -- checkTransitionLevel
    checkGameOver
