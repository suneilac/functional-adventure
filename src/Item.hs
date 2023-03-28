module Item where

import qualified Data.Map as M

data ItemName
  = Pot
  | Jug
  | Sandbag
  | Stove
  | Couch
  | Tarragon
  | Beans
  | Grill
  | Bed
  | Lantern
  deriving (Eq, Ord)

instance Show ItemName where
    show :: ItemName -> String 
    show Pot = "pot"
    show Jug = "jug"
    show Sandbag = "sandbag"
    show Stove = "stove"
    show Couch = "couch"
    show Tarragon = "tarragon"
    show Beans = "beans"
    show Grill = "grill"
    show Bed = "bed"
    show Lantern = "lantern"

type Universe = M.Map ItemName Item

data Item = Item
    {
        iname :: ItemName,
        weight :: Integer
    }
    deriving (Show, Eq)

mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (map (\item -> (iname item, item)) items) 

pot :: Item
pot = Item Pot 20

jug :: Item
jug = Item Jug 10

sandbag :: Item
sandbag = Item Sandbag 35

stove :: Item
stove = Item Stove 80

couch :: Item
couch = Item Couch 90

tarragon :: Item
tarragon = Item Tarragon 5

beans :: Item
beans = Item Beans 7

grill :: Item
grill = Item Grill 100

bed :: Item
bed = Item Bed 100

lantern :: Item 
lantern = Item Lantern 100

univ :: Universe
univ = mkUniverse [pot, jug, sandbag, stove, couch, tarragon, beans, grill, bed, lantern]

itemNames :: [ItemName]
itemNames = M.keys univ
