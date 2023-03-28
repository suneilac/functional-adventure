module Room where

import Item
import Direction
import qualified Data.List as List

data RoomName
  = Kitchen
  | Pantry
  | Yard
  | LivingRoom
  | Bedroom
  deriving (Eq, Ord)

instance Show RoomName where
    show :: RoomName -> String 
    show Kitchen = "kitchen"
    show Pantry = "pantry"
    show Yard = "yard"
    show LivingRoom = "living room"
    show Bedroom = "bedroom"

type Exit = (Direction, RoomName)

data Room = Room
    {
        rname :: RoomName,
        desc :: String,
        exits :: [Exit],
        objects :: [ItemName],
        dark :: Bool
    }
    deriving (Show, Eq)

kitchen :: Room
kitchen = Room Kitchen "You are in the kitchen" [(N, LivingRoom), (E, Pantry), (S, Yard)] [Pot, Stove] False

pantry :: Room
pantry = Room Pantry "You are in the pantry" [(W, Kitchen)] [Tarragon, Beans] False

yard :: Room
yard = Room Yard "You are outside the house" [(N, Kitchen)] [Grill] False

livingRoom :: Room
livingRoom = Room LivingRoom "You are in the living room" [(N, Bedroom), (S, Kitchen)] [Couch, Jug, Sandbag] False

bedroom :: Room
bedroom = Room Bedroom "You are in the bedroom" [(S, LivingRoom)] [Bed] False

roomNames :: [RoomName]
roomNames = map (\room -> rname room) allRooms

addItem :: ItemName -> Room -> Room
addItem item room@Room {objects} = room {objects = item : objects}

removeItem :: ItemName -> Room -> Room
removeItem item room@Room {objects} = room {objects = List.delete item objects}

allRooms :: [Room]
allRooms = [kitchen, livingRoom, pantry, bedroom, yard]

hasObjects :: Room -> Bool 
hasObjects rm = (objects rm) /= []

-- Level 2
darkKitchen :: Room 
darkKitchen = Room Kitchen "You are in the kitchen" [(W, Pantry), (E, LivingRoom)] [Pot, Stove] True

newPantry :: Room
newPantry = Room Pantry "You are in the pantry" [(E, Kitchen)] [Tarragon, Beans, Lantern] False

darkYard :: Room
darkYard = Room Yard "You are outside the house" [(W, LivingRoom)] [Grill] True

darkLivingRoom :: Room
darkLivingRoom = Room LivingRoom "You are in the living room" [(W, Kitchen), (E, Yard), (S, Bedroom)] [Couch, Sandbag] True

darkBedroom :: Room
darkBedroom = Room Bedroom "You are in the bedroom" [(N, LivingRoom)] [Bed, Jug] True

allRooms2 :: [Room]
allRooms2 = [darkKitchen, newPantry, darkYard, darkLivingRoom, darkBedroom]


