module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList aw) = do
  index <- randomRIO (0, length aw - 1)
  return $ aw !! index

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ intersperse ',' guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (replicate (length w) Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) guess = guess `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess = guess `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle s discovered guessed) guess =
  if alreadyGuessed p guess
    then p
    else Puzzle s newDiscovered (guess : guessed) 
      where
        zipper guessed wordChar guessChar =
          if wordChar == guessed
            then Just wordChar
            else guessChar
        newDiscovered = zipWith (zipper guess) s discovered

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
    (_,True)->do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
  
    (True,False)->do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)

    (False,False)->do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
  if length guessed >=17 then
    do  putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do  putStrLn "You win!"
        exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>=runGame
    _   -> putStrLn "Your guess must be a single character"

main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
