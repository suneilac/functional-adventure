module Example where

import qualified Data.List as List
import qualified Data.Map as M
import System.Random

import Item
import Direction
import Room
import Player
import GameState

choose :: [a] -> IO a
choose xs = do
    index <- randomRIO (0, length xs - 1)
    pure $ xs !! index

exampleList :: IO a -> IO Int -> IO [a]
exampleList ioVal ioLen = do
    len <- ioLen
    sequence $ List.replicate len ioVal

class Example a where
    example :: IO a

instance Example Item where
    example :: IO Item
    example = do
        weight <- randomRIO (0, 100)
        iname <- choose itemNames 
        pure $ Item iname weight

instance Example Direction where
    example :: IO Direction
    example = choose [N, S, E, W]

exitExample :: IO Exit
exitExample = do
    dir <- example :: IO Direction
    room <- choose roomNames
    
    pure (dir, room)

instance Example Room where
    example :: IO Room 
    example = do
        rname <- choose roomNames 
        exits <- exampleList exitExample $ randomRIO (2, 4)
        items <- exampleList (choose itemNames) $ randomRIO (2, 5)
        pure $ Room rname ("You are in the " ++ (show rname) ++ ". It is a randomly-generated room.") exits items False

instance Example Player where
    example :: IO Player
    example = do
        inventory <- exampleList (choose itemNames) $ randomRIO (0, 10)
        location <- choose roomNames
        maxWeight <- getMaxWeight inventory
        case inventory of
            [] -> pure $ Player [] 99 location
            (_ : _) -> pure $ Player (List.nub inventory) (fromIntegral maxWeight) location

getMaxWeight :: [ItemName] -> IO Int 
getMaxWeight items =
    let
        maxWt = foldl maxItem 0 items
        minWt = foldl minItem 300 items
    in
        randomRIO (maxWt - minWt, maxWt + minWt)

maxItem :: Int -> ItemName -> Int
maxItem _ Couch = fromIntegral $ weight couch
maxItem acc item = max acc $ fromIntegral (weight (univ M.! item))

minItem :: Int -> ItemName -> Int 
minItem _ Tarragon = fromIntegral $ weight tarragon
minItem acc item = min acc $ fromIntegral (weight (univ M.! item))

instance Example GameState where
    example :: IO GameState
    example = do 
        msg <- choose [Just "One possible message", Just "Yet another possible message", Nothing]
        rms <- exampleList (example :: IO Room) (randomRIO (2,3))
        items <- exampleList (example :: IO Item) (randomRIO (5, 10))
        player <- example :: IO Player
        pure $ GameState msg (mkMap $ List.nub rms) (mkUniverse $ List.nub items) player 0
