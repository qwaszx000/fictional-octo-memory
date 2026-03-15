{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)

import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

type WordleWord = BS.ByteString

getAllowedGuesses :: IO [WordleWord]
getAllowedGuesses = BS.readFile allowedGuessesFN <&> BS.split (fromIntegral $ fromEnum '\n')
  where
    allowedGuessesFN :: FilePath
    allowedGuessesFN = "./allowed_guesses.csv"

getAllowedAnswers :: IO [WordleWord]
getAllowedAnswers = BS.readFile allowedAnswersFN <&> BS.split (fromIntegral $ fromEnum '\n')
  where
    allowedAnswersFN :: FilePath
    allowedAnswersFN = "./allowed_answers.csv"

-- Allowed guesses, allowed answers
type GuessCtx = ([WordleWord], [WordleWord])
data GuessResult = GRWrong | GROtherPlace | GRCorrect
    deriving (Show, Eq)

-- Greens get priority
cmpWords :: WordleWord -> WordleWord -> [GuessResult]
cmpWords guess answer = reverse $ fst $ foldl' cmpChar ([], countedCors) zippedWords
  where
    zippedWords :: [(Word8, Word8)]
    zippedWords = BS.zip guess answer

    countedCors :: Map.Map Word8 Int
    countedCors = foldl' countCorrects Map.empty zippedWords

    countCorrects :: Map.Map Word8 Int -> (Word8, Word8) -> Map.Map Word8 Int
    countCorrects m (gc, ac)
        | gc == ac = Map.insertWith (+) gc 1 m
        | otherwise = m

    cmpChar :: ([GuessResult], Map.Map Word8 Int) -> (Word8, Word8) -> ([GuessResult], Map.Map Word8 Int)
    cmpChar (rs, m) (gc, ac) =
        let
            -- Corrects are used, plus new GROtherPlace
            cUsed = Map.findWithDefault 0 gc m
            cAns :: Int = fromIntegral $ BS.count gc answer
            m' = Map.insert gc (cUsed + 1) m
         in
            if
                | gc == ac -> (GRCorrect : rs, m)
                | cAns > cUsed -> (GROtherPlace : rs, m')
                | otherwise -> (GRWrong : rs, m)

filterCtx :: ([WordleWord] -> [WordleWord]) -> GuessCtx -> GuessCtx
filterCtx f (gs, as) = (f gs, f as)

filterByResult :: WordleWord -> [GuessResult] -> (WordleWord -> Bool)
filterByResult guess res w = passesChars && passesCounts
  where
    zippedGuess :: [(GuessResult, Word8)]
    zippedGuess = {-# SCC "zippedGuess" #-} zip res (BS.unpack guess)

    eqZip :: [Bool]
    eqZip = {-# SCC "eqZip" #-} BS.zipWith (==) guess w

    countChars :: Map.Map Word8 Int -> (GuessResult, Word8) -> Map.Map Word8 Int
    countChars m (GRWrong, gc) = {-# SCC "countChars1" #-} Map.insertWith (+) gc 0 m
    countChars m (_, gc) = {-# SCC "countChars2" #-} Map.insertWith (+) gc 1 m

    countedChars :: Map.Map Word8 Int
    countedChars = {-# SCC "countedChars" #-} foldl' countChars Map.empty zippedGuess

    passesCounts :: Bool
    passesCounts = {-# SCC "passesCounts" #-} Map.foldlWithKey' filterWithMap True countedChars
      where
        filterWithMap :: Bool -> Word8 -> Int -> Bool
        filterWithMap False _ _ = False
        filterWithMap True c n
            | n == 0 = c `BS.notElem` w
            | otherwise = BS.count c w >= fromIntegral n

    passesChars :: Bool
    passesChars = {-# SCC "passesChars" #-} foldl' filterWithChars True zippedEq
      where
        zippedEq :: [(GuessResult, Bool)]
        zippedEq = {-# SCC "zippedEq" #-} zip res eqZip

        filterWithChars :: Bool -> (GuessResult, Bool) -> Bool
        filterWithChars False _ = False
        filterWithChars True (GRCorrect, isEq) = isEq
        filterWithChars True (_, isEq) = not isEq

-- https://wiki.haskell.org/99_questions/Solutions/26
-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- == 3^5
possibleResults :: [[GuessResult]]
possibleResults = combinations 5 [GRCorrect, GROtherPlace, GRWrong]
  where
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _ = [[]] -- double list because it is base case for our do part
    combinations _ [] = [] -- and this is for short-circuit behavior
    combinations n as = do
        el <- as
        rest <- combinations (n - 1) as
        pure $ el : rest

tryGuessAnswer :: Int -> WordleWord -> GuessCtx -> Maybe Int
tryGuessAnswer _ ans (_, []) = error $ "Allowed answers are empty for expected answer " <> show ans
tryGuessAnswer 0 _ _ = Nothing
-- tryGuessAnswer 1 _ (_, _ : _ : _) = Nothing
tryGuessAnswer leftGuesses expectedAnswer (_, [lastGuess])
    | lastGuess == expectedAnswer = Just $ leftGuesses - 1
    | otherwise =
        error $
            "Last guess is "
                <> show lastGuess
                <> " but expected "
                <> show expectedAnswer
tryGuessAnswer leftGuesses expectedAnswer ctx = tryGuessAnswer (leftGuesses - 1) expectedAnswer newCtx
  where
    selectedGuess :: WordleWord
    selectedGuess = selectBestNextWord ctx

    newCtx :: GuessCtx
    newCtx = nextCtx expectedAnswer selectedGuess ctx

nextCtx :: WordleWord -> WordleWord -> GuessCtx -> GuessCtx
nextCtx answer guess = filterCtx $ filter $ filterByResult guess guessResult
  where
    guessResult :: [GuessResult]
    guessResult = cmpWords guess answer

selectBestNextWord :: GuessCtx -> WordleWord
selectBestNextWord (gs, _) = snd $ foldl' entIter (0, "") gs
  where
    entIter :: (Double, WordleWord) -> WordleWord -> (Double, WordleWord)
    entIter prev@(maxEnt, _) curW =
        let
            wEnt = wordEntropy curW gs
         in
            if wEnt > maxEnt
                then (wEnt, curW)
                else prev

wordEntropy :: WordleWord -> [WordleWord] -> Double
wordEntropy w gs = sum $ do
    -- should be avg
    res <- possibleResults
    let newGuessList = filter (filterByResult w res) gs
    let ngCount :: Double = fromIntegral $ length newGuessList
    guard $ ngCount > 0
    let gCount :: Double = fromIntegral $ length gs
    let probability = ngCount / gCount
    let entropy = -logBase 2 probability
    pure $ probability * entropy

avg :: (Fractional r) => [r] -> r
avg vals = sum vals / fromIntegral (length vals)

initProg :: IO (WordleWord, GuessCtx)
initProg = do
    ags <- getAllowedGuesses
    aas <- getAllowedAnswers

    let ctx :: GuessCtx = (take 100 aas, take 100 aas) -- (ags, aas)
    let firstBestW = selectBestNextWord ctx
    -- let firstBestW = "queue" -- for full aas
    pure (firstBestW, ctx)

main :: IO ()
main = do
    (firstBestW, ctx) <- initProg
    print firstBestW

    let maxAttemps = 6
    let res = (\a -> tryGuessWithFirstWord maxAttemps a firstBestW ctx <&> (maxAttemps -)) <$> snd ctx
    let stat = Map.fromListWith (+) $ fmap (,1) res
    -- print res
    print $ Map.toDescList stat
  where
    tryGuessWithFirstWord :: Int -> WordleWord -> WordleWord -> GuessCtx -> Maybe Int
    tryGuessWithFirstWord n answer firstW ctx = tryGuessAnswer (n - 1) answer $ nextCtx answer firstW ctx

-- ghci> iterateWord $ BS.pack "actor"
iterateWord :: WordleWord -> IO ()
iterateWord w = do
    (firstBestW, ctx) <- initProg
    print firstBestW

    putStrLn $ "Expecting answer: " <> show w
    putStrLn $ "Initial ctx: " <> show ((length *** length) ctx)
    let ctx' = nextCtx w firstBestW ctx
    nextIter w ctx'
  where
    nextIter :: WordleWord -> GuessCtx -> IO ()
    nextIter answer ctx = do
        putStrLn $ "Step ctx: " <> show ((length *** length) ctx)
        let nextW = selectBestNextWord ctx
        putStrLn $ "Using word: " <> show nextW
        putStrLn $ "Word result: " <> show (cmpWords nextW answer)
        let ctx' = nextCtx w nextW ctx
        case ctx' of
            (_, []) -> error "No answer in the end"
            (_, [lastAns]) -> print lastAns
            _ -> nextIter answer ctx'
