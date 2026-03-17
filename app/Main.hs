{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Control.Arrow ((&&&), (***))
import Control.Monad (replicateM, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldl', foldlM, maximumBy, sequenceA_)
import Data.Functor ((<&>))
import Data.Ord (comparing)

import Data.Bits

type WordleWord = Word32

-- Or use unsafeRotateR to preserve char order
-- But we'll need to do unsafeRotateR 7 in the and
-- And it'll require using Word32 instead of Int to be sure
unsafeEncodeBS :: BS.ByteString -> WordleWord
unsafeEncodeBS = BS.foldl' go 0
  where
    go :: WordleWord -> Word8 -> WordleWord
    go acc w = (flip unsafeShiftL 5 acc) .|. char2Index w

-- Aligned by 5bit block size
-- Transforms 0b00000 patterns to 0b00001
mark5bitZeros :: WordleWord -> WordleWord
mark5bitZeros v = b1 .&. b2 .&. b3 .&. b4 .&. b5
  where
    v0, b1, b2, b3, b4, b5 :: WordleWord
    v0 = complement v
    b1 = v0 .&. 0b0000100001000010000100001
    b2 = flip unsafeShiftR 1 $ v0 .&. 0b0001000010000100001000010
    b3 = flip unsafeShiftR 2 $ v0 .&. 0b0010000100001000010000100
    b4 = flip unsafeShiftR 3 $ v0 .&. 0b0100001000010000100001000
    b5 = flip unsafeShiftR 4 $ v0 .&. 0b1000010000100001000010000

countZero5bitBlocksA :: WordleWord -> Int
countZero5bitBlocksA = popCount . mark5bitZeros

countChars :: Word8 -> WordleWord -> Int
countChars c w = countEqs w $ replicate5bit 5 $ char2Index c

replicate5bit :: Int -> Word8 -> WordleWord
replicate5bit 0 _ = 0
replicate5bit 1 w = fromIntegral w
replicate5bit n w
    | n > 6 = error "Can't place more than 6 5bits in Int"
    | otherwise = go n 0
  where
    go :: Int -> WordleWord -> WordleWord
    go 0 acc = acc
    go nl acc = go (nl - 1) ((flip unsafeShiftL 5 acc) .|. fromIntegral w)

countEqs :: WordleWord -> WordleWord -> Int
countEqs w1 w2 = countZero5bitBlocksA $ w1 `xor` w2

listEqs :: WordleWord -> WordleWord -> [Bool]
listEqs w1 w2 = word5bFoldl (\acc w -> (w == 0) : acc) [] (w1 `xor` w2)

word5bFoldl' :: forall b. (b -> Word8 -> b) -> b -> WordleWord -> b
word5bFoldl' f !acc = go 5 acc
  where
    go :: Int -> b -> WordleWord -> b
    go 0 !gacc _ = gacc
    go n !gacc it = go (n - 1) (gacc `f` fromIntegral (it .&. 0b11111)) $ flip unsafeShiftR 5 it

word5bFoldl :: forall b. (b -> Word8 -> b) -> b -> WordleWord -> b
word5bFoldl f = go 5
  where
    go :: Int -> b -> WordleWord -> b
    go 0 !gacc _ = gacc
    go n !gacc it = go (n - 1) (gacc `f` fromIntegral (it .&. 0b11111)) $ flip unsafeShiftR 5 it

decodeInt :: WordleWord -> [Word8]
decodeInt = word5bFoldl (\acc c -> index2Char c : acc) []

onNewLine :: Word8 -> Bool
onNewLine = (== (fromIntegral $ fromEnum '\n'))

getAllowedGuesses :: IO [WordleWord]
getAllowedGuesses = BS.readFile allowedGuessesFN <&> BS.splitWith onNewLine <&> fmap unsafeEncodeBS
  where
    allowedGuessesFN :: FilePath
    allowedGuessesFN = "./allowed_guesses.csv"

getAllowedAnswers :: IO [WordleWord]
getAllowedAnswers = BS.readFile allowedAnswersFN <&> fmap unsafeEncodeBS . BS.splitWith onNewLine
  where
    allowedAnswersFN :: FilePath
    allowedAnswersFN = "./allowed_answers.csv"

-- Allowed guesses, allowed answers
type GuessCtx = ([WordleWord], [WordleWord])
data GuessResult = GRWrong | GROtherPlace | GRCorrect
    deriving (Show, Eq)

-- We assume that our list of GuessResults can be placed in Int
-- So int must be able to contain at least 2*n bits
unsafeGRs2Int :: [GuessResult] -> Int
unsafeGRs2Int = foldl' (\acc gr -> (flip unsafeShiftL 2 acc) .|. encodeGR gr) 0
  where
    encodeGR :: GuessResult -> Int
    encodeGR GRWrong = 0b11
    encodeGR GROtherPlace = 0b10
    encodeGR GRCorrect = 0b01

-- Greens get priority
cmpWords :: WordleWord -> WordleWord -> [GuessResult]
cmpWords guess answer = reverse $ runST $ do
    vec <- mismatchesV
    foldlM (fiter vec) [] $ zip (decodeInt guess) (decodeInt answer)
  where
    fiter :: MV.MVector s Int -> [GuessResult] -> (Word8, Word8) -> ST s [GuessResult]
    fiter vec grs (gc, ac) = do
        if gc == ac
            then pure $ GRCorrect : grs
            else do
                let ind :: Int = char2Index gc
                val <- MV.unsafeRead vec ind
                case val of
                    0 -> pure $ GRWrong : grs
                    n -> MV.unsafeWrite vec ind (n - 1) >> pure (GROtherPlace : grs)

    mismatchesV :: ST s (MV.MVector s Int)
    mismatchesV = do
        vec <- MV.replicate 26 0
        zipWithM_ (\cg ca -> if cg == ca then pure () else MV.unsafeModify vec (+ 1) $ char2Index ca) (decodeInt guess) (decodeInt answer)
        pure vec

filterCtx :: ([WordleWord] -> [WordleWord]) -> GuessCtx -> GuessCtx
filterCtx f (gs, as) = (f gs, f as) -- "hard" mode, but it prunes serach space
-- filterCtx f (gs, as) = (gs, f as) -- intended mode, but it's too slow

char2Index :: (Integral i) => Word8 -> i
char2Index c = fromIntegral c - fromIntegral (fromEnum 'a')

index2Char :: (Integral i) => i -> Word8
index2Char i = fromIntegral $ fromEnum 'a' + fromIntegral i

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
            (decodeInt guess)
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
                        | v == 0 -> (index2Char i) `countChars` w == 0
                        | otherwise -> (index2Char i) `countChars` w >= v
            )
            True
            vec

    passesChars :: Bool
    passesChars =
        and $
            zipWith
                (\gr eq -> (gr == GRCorrect) == eq)
                res
                (listEqs guess w)

-- https://wiki.haskell.org/99_questions/Solutions/26
-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
-- == 3^5
possibleResults :: [[GuessResult]]
possibleResults = replicateM 5 [GRCorrect, GROtherPlace, GRWrong]

tryGuessAnswer :: Int -> WordleWord -> GuessCtx -> Maybe Int
tryGuessAnswer _ ans (_, []) = error $ "Allowed answers are empty for expected answer " <> show (BS.pack $ decodeInt ans)
tryGuessAnswer 0 _ _ = Nothing
-- tryGuessAnswer 1 _ (_, _ : _ : _) = Nothing
tryGuessAnswer leftGuesses expectedAnswer (_, [lastGuess])
    | lastGuess == expectedAnswer = Just $ leftGuesses - 1
    | otherwise =
        error $
            "Last guess is "
                <> show (BS.pack $ decodeInt lastGuess)
                <> " but expected "
                <> show (BS.pack $ decodeInt expectedAnswer)
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
    (_, ngCount) <-
        IMap.toList $
            IMap.fromListWith (+) $
                [(unsafeGRs2Int $ cmpWords w a, 1) | a <- as]
    let probability = ngCount / aCount
    pure $ probability * (-log probability)
  where
    aCount :: Double
    aCount = fromIntegral $ length as

initProg :: IO (WordleWord, GuessCtx)
initProg = do
    -- ags <- getAllowedGuesses
    aas <- getAllowedAnswers

    let ctx :: GuessCtx = (aas, aas) -- (ags, aas)
    let firstBestW = selectBestNextWord ctx
    pure (firstBestW, ctx)

main :: IO ()
main = do
    (firstBestW, ctx) <- initProg
    print $ BS.pack $ decodeInt firstBestW

    let maxAttemps :: Int = 6
    let res = (\a -> tryGuessWithFirstWord maxAttemps a firstBestW ctx <&> (maxAttemps -)) <$> snd ctx
    let stat :: Map.Map (Maybe Int) Int = Map.fromListWith (+) $ fmap (,1) res
    print $ Map.toDescList stat
  where
    tryGuessWithFirstWord :: Int -> WordleWord -> WordleWord -> GuessCtx -> Maybe Int
    tryGuessWithFirstWord n answer firstW ctx = tryGuessAnswer (n - 1) answer $ nextCtx answer firstW ctx

-- ghci> iterateWord $ BS.pack "actor"
iterateWord :: WordleWord -> IO ()
iterateWord w = do
    (firstBestW, ctx) <- initProg
    print $ BS.pack $ decodeInt firstBestW

    putStrLn $ "Expecting answer: " <> show (BS.pack $ decodeInt w)
    putStrLn $ "Initial ctx: " <> show ((length *** length) ctx)
    let ctx' = nextCtx w firstBestW ctx
    nextIter w ctx'
  where
    nextIter :: WordleWord -> GuessCtx -> IO ()
    nextIter answer ctx = do
        putStrLn $ "Step ctx: " <> show ((length *** length) ctx)
        let nextW = selectBestNextWord ctx
        putStrLn $ "Using word: " <> show (BS.pack $ decodeInt nextW)
        putStrLn $ "Word result: " <> show (cmpWords nextW answer)
        let ctx' = nextCtx w nextW ctx
        case ctx' of
            (_, []) -> error "No answer in the end"
            (_, [lastAns]) -> print $ BS.pack $ decodeInt lastAns
            _ -> nextIter answer ctx'

-- Thanks to Daniel Wagner for help
-- https://stackoverflow.com/a/79908180
