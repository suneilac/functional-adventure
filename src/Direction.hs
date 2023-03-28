module Direction where

data Direction = N | S | E | W
    deriving (Eq)
    
instance Show Direction where
    show :: Direction -> String 
    show N = "north"
    show S = "south"
    show E = "east"
    show W = "west"