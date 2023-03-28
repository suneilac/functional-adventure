module Command where

import Text.Parsec hiding (parse, runParser, (<|>), sepBy1, sepBy, choice)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName
itemNameP = 
    choice [
        Pot <$ string "pot",
        Jug <$ string "jug",
        Sandbag <$ string "sandbag",
        Stove <$ string "stove",
        Couch <$ string "couch",
        Tarragon <$ string "tarragon",
        Beans <$ string "beans",
        Grill <$ string "grill",
        Bed <$ string "bed",
        Lantern <$ string "lantern"
    ]
        
nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do 
    i <- itemNameP 
    pure $ [i]

nounPhrase :: Parser [ItemName] 
nounPhrase = sepBy1 itemNameP $ string ", " <|> string ","

inventoryP :: Parser Command 
inventoryP = Inventory <$ string "inventory"

takeP :: Parser Command 
takeP = do 
    items <- string "take " *> nounPhrase 
    pure $ Take items

exitP :: Parser Command
exitP = Exit <$ string "exit" <|> string "quit"

dropP :: Parser Command 
dropP = do 
    items <- string "drop " *> nounPhrase 
    pure $ Drop items

lookP :: Parser Command 
lookP = Look <$ string "look"

directionP :: Parser Direction 
directionP = 
    choice [
        N <$ string "north",
        S <$ string "south",
        E <$ string "east",
        W <$ string "west"
    ]

moveP :: Parser Command 
moveP = do
    dir <- directionP 
    pure $ Move dir

commandP :: Parser Command 
commandP = do 
    cmd <- inventoryP <|> takeP <|> exitP <|> dropP <|> lookP <|> moveP
    pure $ cmd

conjunctionP :: Parser Conjunction
conjunctionP = sepBy1 commandP (string " and ") <* eof

parseInput :: String -> Maybe Conjunction
parseInput s =
    case (parse conjunctionP s) of
        Left _ -> Nothing 
        Right out -> Just out