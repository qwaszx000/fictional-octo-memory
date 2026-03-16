{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word8)

import Control.Arrow ((&&&), (***))
import Control.Monad (guard, replicateM)
import Data.Foldable (foldl', maximumBy)
import Data.Functor ((<&>))
import Data.Ord (comparing)

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
            cAns = BS.count gc answer
            m' = Map.insert gc (cUsed + 1) m
         in
            if
                | gc == ac -> (GRCorrect : rs, m)
                | cAns > cUsed -> (GROtherPlace : rs, m')
                | otherwise -> (GRWrong : rs, m)

filterCtx :: ([WordleWord] -> [WordleWord]) -> GuessCtx -> GuessCtx
filterCtx f (gs, as) = (f gs, f as) -- "hard" mode, but it prunes serach space
-- filterCtx f (gs, as) = (gs, f as) -- intended mode, but it's too slow

filterByResult :: WordleWord -> [GuessResult] -> (WordleWord -> Bool)
filterByResult guess res w = passesChars && passesCounts
  where
    countedChars :: Map.Map Word8 Int
    countedChars =
        {-# SCC "countedChars" #-}
        Map.fromListWith (+) $
            zipWith
                (\c gr -> (c, if gr == GRWrong then 0 else 1))
                (BS.unpack guess)
                res

    passesCounts :: Bool
    passesCounts =
        {-# SCC "passesCounts" #-}
        and $
            Map.mapWithKey
                (\c n -> if n == 0 then c `BS.notElem` w else BS.count c w >= n)
                countedChars

    passesChars :: Bool
    passesChars =
        {-# SCC "passesChars" #-}
        and $
            zipWith
                (\gr eq -> (gr == GRCorrect) == eq)
                res
                (BS.zipWith (==) guess w)

-- https://wiki.haskell.org/99_questions/Solutions/26
-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- == 3^5
possibleResults :: [[GuessResult]]
possibleResults = replicateM 5 [GRCorrect, GROtherPlace, GRWrong]

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

maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f as = snd $ maximumBy (comparing fst) $ fmap (f &&& id) as

selectBestNextWord :: GuessCtx -> WordleWord
selectBestNextWord (gs, as) = maximumOn (flip wordEntropy as) gs

wordEntropy :: WordleWord -> [WordleWord] -> Double
wordEntropy w as = sum $ do
    -- should be avg instead of sum
    -- but sum results in a lot faster execution
    -- and results seem to be ok(even though using sum is wrong)
    -- not sure why such a speed diff
    res <- possibleResults
    let newAnswersList = filter (filterByResult w res) as
    let ngCount :: Double = fromIntegral $ length newAnswersList
    guard $ ngCount > 0
    let aCount :: Double = fromIntegral $ length as
    let probability = ngCount / aCount
    let entropy = -logBase 2 probability
    pure $ probability * entropy

avg :: (Fractional r) => [r] -> r
avg vals = sum vals / fromIntegral (length vals)

initProg :: IO (WordleWord, GuessCtx)
initProg = do
    -- ags <- getAllowedGuesses
    aas <- getAllowedAnswers

    let ctx :: GuessCtx = (take 200 aas, take 200 aas) -- (ags, aas)
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

-- Thanks to Daniel Wagner for help
-- https://stackoverflow.com/a/79908180
