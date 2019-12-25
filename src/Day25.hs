{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day25 where

import           Control.Applicative        ((<|>))
import           Data.Char                  (chr, ord)
import           Data.List                  (tails)

import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Text.Megaparsec            (endBy, manyTill, option, try)
import           Text.Megaparsec.Char       (char, eol, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L


import           AoC
import           ComputerST
import           Search
import           TwoD

getInput :: IO Instructions
getInput = readInstructions "input/day25"

data Room = Room {
  roomName      :: String,
  roomNeighbors :: [Dir],
  roomItems     :: [String] } deriving Show

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseRoom :: Parser Room
parseRoom = do
  _ <- space1
  _ <- manyTill L.charLiteral (string "== ")
  rname <- manyTill L.charLiteral (string " ==")
  _ <- space
  _ <- manyTill L.charLiteral (string "Doors here lead:\n")
  doors <- door `endBy` "\n"
  _ <- space
  items <- option [] (try parseItems)
  _ <- space
  _ <- manyTill L.charLiteral eol

  pure (Room rname doors items)

  where
    door = N <$ "- north"
           <|> E <$ "- east"
           <|> W <$ "- west"
           <|> S <$ "- south"

    parseItems = do
      _ <- manyTill L.charLiteral (string "Items here:\n")
      pstr `endBy` "\n"

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

search :: (GameState -> Bool) -> GameState -> GameState
search goal = head . filter goal . bfsOn (\GameState{..} -> (roomName currentRoom) <> show inv) neighbors

getAllItems :: Instructions -> GameState
getAllItems prog = search (\GameState{..} -> length inv == 8) initSt
  where
    initSt = let p = executePause [] prog
                 (_, r) = readRoom p
             in GameState r p mempty

neighbors :: GameState -> [GameState]
neighbors GameState{..} = do
  let procd = map (\d -> let p' = resumePause (cmdStr $ dirString d) currentState
                             (_, r) = readRoom p'
                         in (r, doRoom r p')) (roomNeighbors currentRoom)

  [GameState r p' (Set.union inv (Set.fromList i)) | (r,(i,p')) <- procd]

  where
    doNotWant = ["giant electromagnet", "infinite loop", "escape pod", "molten lava", "photons"]

    doRoom :: Room -> Paused -> ([String], Paused)
    doRoom Room{..} p = let rhas = filter (`notElem` doNotWant) roomItems in
                          (rhas, foldr (\x o -> resumePause (cmdStr ("take " <> x)) o) p rhas)

goto :: String -> GameState -> GameState
goto rm = search (\GameState{..} -> (roomName currentRoom) == rm)

weighMe :: Instructions -> GameState
weighMe = goto "Security Checkpoint" . getAllItems

brutus :: GameState -> IO ()
brutus GameState{..} = go todo
  where
    dropAll = foldr (\x o -> resumePause (cmdStr ("drop " <> x)) o) currentState inv

    todo = foldMap (flip combinations (Set.toList inv)) [1..8]

    go [] = error "noooo"
    go (x:xs) = case resume (cmdStr "south") $ foldr (\i o -> resumePause (cmdStr ("take " <> i)) o) dropAll x of
                  Left _                    -> go xs
                  Right FinalState{outputs} -> putStrLn (map chr outputs)

-- Play the game interactively.
run :: Paused -> IO ()
run p@Paused{pausedOuts} = do
  putStrLn $ (chr <$> pausedOuts)
  n <- fmap ord <$> getLine
  case resume (n <> [10]) p of
    Left (NoInput p')         -> run p'
    Right FinalState{outputs} -> putStrLn (map chr outputs)
    Left x -> error $ "Some other termination status: " <> show x

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

part1 :: IO ()
part1 = do
  prog <- getInput
  brutus . weighMe $ prog
