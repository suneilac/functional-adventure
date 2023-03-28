module GameState where

-- import Data.List
import qualified Data.Map as M

import Direction
import Item
import Room
import Player
import GHC.Exception as Exception

type GameMap = M.Map RoomName Room

data GameState = GameState
    {
        message :: Maybe String,
        gmap :: GameMap,
        universe :: Universe,
        player :: Player,
        level :: Integer
    }
    deriving (Show, Eq)

data KeyError = KeyError
    deriving Show

instance Exception KeyError

type Error a = Either String a 

mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList (fmap (\room@Room {rname} -> (rname, room)) rooms)

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState = GameState Nothing gameMap univ you 1

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap 
setRoomMap roomName rm gmap = M.map f gmap
    where
        f val = 
            if (rname val) == roomName
                then rm
            else val

setMessage :: String -> GameState -> GameState
setMessage "" gs = gs {message = Nothing}
setMessage msg gs = gs {message = Just msg}

currentInventory :: GameState -> [ItemName]
currentInventory gs = inventory (player gs)

currentRoom :: GameState -> Room
currentRoom gs = getRoomMap (location (player gs)) (gmap gs)

nearbyObjects :: GameState -> [ItemName]
nearbyObjects gs = objects (currentRoom gs)

takeItemHelper :: ItemName -> GameState -> GameState
takeItemHelper iname gs =
    setMessage ("You take the " ++ (show iname) ++ ".") gs'
        where
            gs' = 
                gs {
                    gmap = setRoomMap (rname (currentRoom gs)) (Room.removeItem iname (currentRoom gs)) (gmap gs),
                    universe = universe gs,
                    player = Player.addItem iname (player gs)
                }

takeItem :: ItemName -> GameState -> GameState
takeItem iname gs =
    case alreadyHaveTakeCheck iname gs of 
        Left msg -> setMessage msg gs
        Right gs' -> 
            case inRoomTakeCheck iname gs' of 
                Left msg -> setMessage msg gs'
                Right gs'' -> 
                    case weightCheck iname gs'' of 
                        Left msg -> setMessage msg gs''
                        Right gs''' -> takeItemHelper iname gs'''

dropItemHelper :: ItemName -> GameState -> GameState
dropItemHelper iname gs = 
    setMessage ("You drop the " ++ (show iname) ++ ".") gs'
    where
        gs' = 
            gs {
                gmap = setRoomMap (rname (currentRoom gs)) (Room.addItem iname (currentRoom gs)) (gmap gs),
                universe = universe gs,
                player = Player.removeItem iname (player gs)
            }

dropItem :: ItemName -> GameState -> GameState
dropItem iname gs = 
    case anywhereDropCheck iname gs of 
        Left msg -> setMessage msg gs 
        Right gs' -> 
            case inRoomDropCheck iname gs' of 
                Left msg -> setMessage msg gs'
                Right gs'' -> dropItemHelper iname gs''

inventoryWeight :: GameState -> Integer
inventoryWeight gs = 
    helper (currentInventory gs)
    where
        helper [] = 0
        helper (iname : t) = (weight $ (universe gs) M.! iname) + (helper t)

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs =
    let inv = inventory $ player gs
    in
        if elem iname inv
            then Left $ "You are already carrying the " ++ (show iname) ++ "."
            else Right gs

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs = 
    if elem iname (nearbyObjects gs)
        then Right gs 
        else Left $ "There is no " ++ (show iname) ++ " in this room."

weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs = 
    let 
        wt = inventoryWeight gs
        itemWeight = weight $ (universe gs) M.! iname
    in
        if wt + itemWeight > (maxWeight $ player gs) 
            then Left "That's too much weight for you to carry."
            else Right gs 

anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs = 
    if elem iname (nearbyObjects gs) || elem iname (currentInventory gs)
        then Right gs
        else Left $ "What do you mean drop the \"" ++ (show iname) ++ "\"?"

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gs = 
    if elem iname (nearbyObjects gs)
        then Left $ "You aren't carrying the " ++ (show iname) ++ "."
        else Right gs 

roomHasObjects :: GameState -> Bool 
roomHasObjects gs = hasObjects $ currentRoom gs

destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm =
    case (foldl helper False (exits rm)) of 
        True -> Just (findRoom dir (exits rm))
            where
                findRoom _ [] = rname rm
                findRoom dir' (h : t) = 
                    if fst h == dir' 
                        then snd h 
                    else 
                        findRoom dir t
        False -> Nothing
        where
            helper acc a = (dir == fst a) || acc

move :: Direction -> GameState -> GameState
move dir gs = 
    case destinationName dir (currentRoom gs) of 
        Just roomName ->
            setMessage ("You go " ++ (show dir) ++ ".") gs'
                where
                    gs' = gs {player = (newLocation roomName (player gs))}
        Nothing -> 
            setMessage ("There is no exit in that direction.") gs

haveWonGame :: GameState -> Bool
haveWonGame gs = (rname (currentRoom gs) == Yard) && elem Jug (currentInventory gs)

-- Level 2
gameMap2 :: GameMap 
gameMap2 = mkMap allRooms2

level2State :: GameState 
level2State = GameState Nothing gameMap2 univ you2 2

isBright :: GameState -> Bool 
isBright gs = (elem Lantern $ currentInventory gs) || not (dark $ currentRoom gs)