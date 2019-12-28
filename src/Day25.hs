{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day25 where

import           Control.Applicative         ((<|>))
import           Control.Parallel.Strategies (parMap, rseq)
import           Data.Char                   (chr, ord)
import           Data.Maybe                  (catMaybes)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Text.Megaparsec             (endBy, manyTill, option, try)
import           Text.Megaparsec.Char        (char, space, string)
import qualified Text.Megaparsec.Char.Lexer  as L


import           Advent.AoC
import           Advent.Search
import           Advent.TwoD
import           ComputerST

getInput :: IO Instructions
getInput = readInstructions "input/day25"

data Room = Room {
  roomName      :: String,
  roomNeighbors :: [Dir],
  roomItems     :: [String]
  } deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseRoom :: Parser Room
parseRoom = do
  _ <- lexeme $ manyTill L.charLiteral (string "== ")
  rname <- lexeme $ manyTill L.charLiteral (string " ==")
  _ <- lexeme $ manyTill L.charLiteral (string "Doors here lead:\n")
  doors <- lexeme $ door `endBy` "\n"
  items <- option [] (try $ lexeme parseItems)

  pure (Room rname doors items)

  where
    door = N <$ "- north"
           <|> E <$ "- east"
           <|> W <$ "- west"
           <|> S <$ "- south"

    parseItems = manyTill L.charLiteral (string "Items here:\n") *> (pstr `endBy` "\n")

    pstr = string "- " *> manyTill L.charLiteral (char '\n')

dirString :: Dir -> String
dirString N = "north"
dirString S = "south"
dirString E = "east"
dirString W = "west"

readRoom :: Paused -> (String, Room)
readRoom Paused{pausedOuts} =
  let s = chr <$> pausedOuts in (s, parseLit parseRoom (T.pack s))

cmdStr :: String -> [Int]
cmdStr s = (ord <$> s) <> [10]

data GameState = GameState {
  currentRoom  :: Room,
  currentState :: Paused,
  inv          :: Set String
  }

-- BFS the game state and return the first state where the given predicate is true.
search :: (GameState -> Bool) -> GameState -> GameState
search goal = head . filter goal . bfsOn (\GameState{..} -> roomName currentRoom <> show inv) neighbors

-- Find all the rooms and get all the things.
getAllItems :: Instructions -> GameState
getAllItems prog = search (\GameState{..} -> length inv == 8) initSt
  where
    initSt = let p = executePause [] prog
                 (_, r) = readRoom p
             in GameState r p mempty

-- Find all neighbor states from the given state.
--
-- The forward game states include not only entering the room, but
-- also picking up any items that may be in the room
neighbors :: GameState -> [GameState]
neighbors GameState{..} = do
  let procd = parMap rseq (\d -> let p' = resumePause (cmdStr $ dirString d) currentState
                                     (_, r) = readRoom p'
                            in (r, doRoom r p')) (roomNeighbors currentRoom)

  [GameState r p' (Set.union inv (Set.fromList i)) | (r,(i,p')) <- procd]

  where
    doNotWant = ["giant electromagnet", "infinite loop", "escape pod", "molten lava", "photons"]

    doRoom :: Room -> Paused -> ([String], Paused)
    doRoom Room{..} p = let rhas = filter (`notElem` doNotWant) roomItems in
                          (rhas, foldr (\x o -> resumePause (cmdStr ("take " <> x)) o) p rhas)

-- Find the way to a particular room.
goto :: String -> GameState -> GameState
goto rm = search (\GameState{..} -> roomName currentRoom == rm)

-- Go to the security check point to check weights.
weighMe :: Instructions -> GameState
weighMe = goto "Security Checkpoint" . getAllItems

-- Given a game state where from the security checkpoint location with
-- all items, figure out which items are necessary to proceed south
-- and return the final game screen.
brutus :: GameState -> String
brutus GameState{..} = head . catMaybes . parMap rseq tryOne $ todo
  where
    dropAll = foldr (\x o -> resumePause (cmdStr ("drop " <> x)) o) currentState inv

    todo = Set.toList $ Set.powerSet inv

    tryOne x = either (const Nothing) (\FinalState{outputs} -> Just $ mconcat [show x, "\n", map chr outputs]) $
               resume (cmdStr "south") $ foldr (\i o -> resumePause (cmdStr ("take " <> i)) o) dropAll x

-- Play the game interactively.
run :: Paused -> IO ()
run p@Paused{pausedOuts} = do
  putStrLn (chr <$> pausedOuts)
  n <- fmap ord <$> getLine
  case resume (n <> [10]) p of
    Left (NoInput p')         -> run p'
    Right FinalState{outputs} -> putStrLn (map chr outputs)
    Left x -> error $ "Some other termination status: " <> show x

part1' :: Instructions -> String
part1' = brutus . weighMe

part1 :: IO ()
part1 = putStr . part1' =<< getInput
