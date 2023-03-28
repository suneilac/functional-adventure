module Player where 

import Item
import Room

data Player = Player
    {
        inventory :: [ItemName],
        maxWeight :: Integer,
        location :: RoomName
    }
    deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
addItem item p = Player (item : inventory p) (maxWeight p) (location p)

removeItem :: ItemName -> Player -> Player
removeItem item p = Player (filter (\n -> item /= n) $ inventory p) (maxWeight p) (location p)

newLocation :: RoomName -> Player -> Player
newLocation room p = Player (inventory p) (maxWeight p) room

isCarryingAnything :: Player -> Bool
isCarryingAnything p = (inventory p) /= []

you :: Player
you = Player [] 100 Kitchen

you2 :: Player 
you2 = Player [] 100 Bedroom
