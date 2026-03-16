{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)

import Control.Arrow ((&&&), (***))
import Control.Monad (replicateM, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldlM, maximumBy, sequenceA_)
import Data.Functor ((<&>))
import Data.Ord (comparing)

type WordleWord = BS.ByteString
type WIntMap = Map.Map Word8 Int

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
    deriving (Show, Eq, Ord)

-- Greens get priority
cmpWords :: WordleWord -> WordleWord -> [GuessResult]
cmpWords guess answer = reverse $ runST $ do
    vec <- mismatchesV
    foldlM (fiter vec) [] $ BS.zip guess answer
  where
    fiter :: MV.MVector s Int -> [GuessResult] -> (Word8, Word8) -> ST s [GuessResult]
    fiter vec grs (gc, ac) = do
        if gc == ac
            then pure $ GRCorrect : grs
            else do
                let ind = char2Index gc
                val <- MV.unsafeRead vec ind
                case val of
                    0 -> pure $ GRWrong : grs
                    n -> MV.unsafeWrite vec ind (n - 1) >> pure (GROtherPlace : grs)

    mismatchesV :: ST s (MV.MVector s Int)
    mismatchesV = do
        vec <- MV.replicate 26 0
        sequenceA_ $ BS.zipWith (\cg ca -> if cg == ca then pure () else MV.unsafeModify vec (+ 1) $ char2Index ca) guess answer
        pure vec

filterCtx :: ([WordleWord] -> [WordleWord]) -> GuessCtx -> GuessCtx
filterCtx f (gs, as) = (f gs, f as) -- "hard" mode, but it prunes serach space
-- filterCtx f (gs, as) = (gs, f as) -- intended mode, but it's too slow

char2Index :: Word8 -> Int
char2Index c = fromIntegral c - fromEnum 'a'

index2Char :: Int -> Word8
index2Char i = fromIntegral $ fromEnum 'a' + i

filterByResult :: WordleWord -> [GuessResult] -> (WordleWord -> Bool)
filterByResult guess res w = passesCounts && passesChars
  where
    countedChars :: ST s (MV.MVector s Int)
    countedChars = do
        vec <- MV.replicate 26 (-1 :: Int)
        zipWithM_
            ( \c gr ->
                if gr == GRWrong
                    then MV.unsafeModify vec (\val -> if val < 0 then 0 else val) $ char2Index c
                    else MV.unsafeModify vec (\val -> if val < 0 then 1 else val + 1) $ char2Index c
            )
            (BS.unpack guess)
            res
        pure vec

    passesCounts :: Bool
    passesCounts = runST $ do
        vec <- countedChars
        MV.ifoldl'
            ( \acc i v ->
                acc
                    && if
                        | v < 0 -> True
                        | v == 0 -> index2Char i `BS.notElem` w
                        | otherwise -> BS.count (index2Char i) w >= v
            )
            True
            vec

    passesChars :: Bool
    passesChars =
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
maximumOn f as = snd $ maximumBy (comparing fst) $ map (f &&& id) as

selectBestNextWord :: GuessCtx -> WordleWord
selectBestNextWord (gs, as) = maximumOn (flip wordEntropy as) gs

wordEntropy :: WordleWord -> [WordleWord] -> Double
wordEntropy w as = sum $ do
    -- sum - faster, but results are worse
    -- avg is slower, but results are better
    (_, ngCount) <-
        Map.toList $
            Map.fromListWith (+) $
                [(cmpWords w a, 1) | a <- as]
    let probability = ngCount / aCount
    pure $ probability * (-log probability)
  where
    aCount :: Double
    aCount = fromIntegral $ length as

avg :: (Fractional r, Foldable f) => f r -> r
avg vals = sum vals / fromIntegral (length vals)

initProg :: IO (WordleWord, GuessCtx)
initProg = do
    -- ags <- getAllowedGuesses
    aas <- getAllowedAnswers

    let ctx :: GuessCtx = (aas, aas) -- (ags, aas)
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
