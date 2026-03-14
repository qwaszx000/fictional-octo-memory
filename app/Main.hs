{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map

import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

type WordleWord = BS.ByteString

getAllowedGuesses :: IO [WordleWord]
getAllowedGuesses = BS.readFile allowedGuessesFN <&> BS.lines
  where
    allowedGuessesFN :: FilePath
    allowedGuessesFN = "./allowed_guesses.csv"

getAllowedAnswers :: IO [WordleWord]
getAllowedAnswers = BS.readFile allowedAnswersFN <&> BS.lines
  where
    allowedAnswersFN :: FilePath
    allowedAnswersFN = "./allowed_answers.csv"

-- Allowed guesses, allowed answers
type GuessCtx = ([WordleWord], [WordleWord])
data GuessResult = GRWrong | GROtherPlace | GRCorrect
    deriving (Show, Eq)

-- Greens get priority
-- aaaaa -> aabaa -> ggwgg, not ggygw
-- -- tmpS = "aa", gc = 'a', ac = 'b'
-- -- restG = "aa", restA = "aa"
-- aa -> ba -> wg
cmpWords :: WordleWord -> WordleWord -> [GuessResult]
cmpWords guess answer = reverse $ fst $ foldl' cmpChar ([], 0) $ BS.zip guess answer
  where
    cmpChar :: ([GuessResult], Int) -> (Char, Char) -> ([GuessResult], Int)
    cmpChar (res, curI) (gc, ac) =
        (,curI + 1) $
            (: res) $
                let
                    (guessDone, guessRest') = BS.splitAt (fromIntegral curI) guess
                    guessRest = BS.drop 1 guessRest'
                    (answerDone, answerRest') = BS.splitAt (fromIntegral curI) answer
                    answerRest = BS.drop 1 answerRest'
                 in
                    if
                        | gc == ac -> GRCorrect
                        -- rest of guess has enough of this char, but maybe it is GRCorrect somewhere
                        -- So current is wrong
                        | BS.count gc guessRest >= BS.count gc answer -> GRWrong
                        | BS.count gc answerDone < BS.count gc guessRest -> GRWrong
                        | otherwise -> GROtherPlace

-- aaaaa
-- aabaa
-- ggwgg
--
-- ababa
-- aabaa
-- gyywg or gwyyg
-- count c guessRest >= count c answer -> GRWrong
--
-- ab
-- bb
-- wg
--
-- abb
-- bba
-- ygy
-- count c guessDone < count c answer -> GROtherPlace

filterCtx :: ([WordleWord] -> [WordleWord]) -> GuessCtx -> GuessCtx
filterCtx f (gs, as) = (f gs, f as)

filterByResult :: WordleWord -> [GuessResult] -> (WordleWord -> Bool)
filterByResult guess res w = filterCountedChars w && filterPerChars w
  where
    guessS :: String
    guessS = BS.unpack guess

    filterPerChars :: WordleWord -> Bool
    filterPerChars = snd . BS.foldl' applyPerCharF (0, True)
      where
        applyPerCharF :: (Int, Bool) -> Char -> (Int, Bool)
        applyPerCharF acc@(_, False) _ = acc
        applyPerCharF (n, True) c = (n + 1,) $ (perCharFilter !! n) c

    perCharFilter :: [Char -> Bool]
    perCharFilter = zipWith buildCharFilter guessS res

    buildCharFilter :: Char -> GuessResult -> (Char -> Bool)
    buildCharFilter c GRCorrect = (== c)
    buildCharFilter c _ = (/= c)

    countCharsRequired :: Map.Map Char Int
    countCharsRequired = Map.fromListWith (+) filteredChars
      where
        filteredChars :: [(Char, Int)]
        filteredChars = mapMaybe transformChars $ zip guessS res

        transformChars :: (Char, GuessResult) -> Maybe (Char, Int)
        transformChars (c, GROtherPlace) = Just (c, 1)
        transformChars _ = Nothing

    filterCountedChars :: WordleWord -> Bool
    filterCountedChars w = Map.foldlWithKey' filterFold True countCharsRequired
      where
        filterFold :: Bool -> Char -> Int -> Bool
        filterFold False _ _ = False
        filterFold True c minCs = BS.count c w >= fromIntegral minCs

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
selectBestNextWord (gs, _) = head $ sortOn (Down . wordEntropy) gs
  where
    wordEntropy :: WordleWord -> Double
    wordEntropy w = sum $ do
        res <- possibleResults
        let newGuessList = filter (filterByResult w res) gs
        let gCount :: Double = fromIntegral $ length gs
        let ngCount :: Double = fromIntegral $ length newGuessList
        let probability = ngCount / gCount
        let entropy = logBase 2 probability
        pure $ probability * entropy

main :: IO ()
main = do
    ags <- getAllowedGuesses
    aas <- getAllowedAnswers

    let ctx :: GuessCtx = (take 50 aas, take 50 aas) -- (ags, aas)
    let firstBestW = selectBestNextWord ctx
    print firstBestW

    let maxAttemps = 6
    let res = (\a -> tryGuessWithFirstWord maxAttemps a firstBestW ctx <&> (maxAttemps -)) <$> snd ctx
    let stat = Map.fromListWith (+) $ fmap (,1) res
    print res
    print $ Map.toDescList stat
  where
    tryGuessWithFirstWord :: Int -> WordleWord -> WordleWord -> GuessCtx -> Maybe Int
    tryGuessWithFirstWord n answer firstW ctx = tryGuessAnswer (n - 1) answer $ nextCtx answer firstW ctx
