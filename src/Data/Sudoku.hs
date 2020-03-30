{-# LANGUAGE RecordWildCards #-}

module Data.Sudoku
  ( Sudoku
  , SudokuCell(..)
  , emptySudoku
  , newSudoku
  , (!)
  , set
  , isValid
  , solve
  , randomSudoku
  , pp
  )
where

import           Control.Monad
import           Data.Array              hiding ( (!) )
import qualified Data.Array                    as A
import           Data.Function                  ( on )
import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( intToDigit )
import           System.Random                  ( mkStdGen )
import           System.Random.Shuffle          ( shuffle' )
import           Data.List                      ( sortBy
                                                , groupBy
                                                , intercalate
                                                , foldl'
                                                , (\\)
                                                )

data SudokuCell = Fixed Int | Flex Int | None
  deriving (Eq, Show)

newtype Sudoku = Sudoku { getSudoku :: Array (Int, Int) SudokuCell }
  deriving (Eq, Show)

emptySudoku :: Sudoku
emptySudoku = Sudoku $ listArray ((1, 1), (9, 9)) $ repeat None

newSudoku :: [Int] -> Sudoku
newSudoku =
  Sudoku
    . array ((1, 1), (9, 9))
    . zip [ (j, i) | i <- [1 .. 9], j <- [1 .. 9] ]
    . map (\n -> if n `elem` [1 .. 9] then Fixed n else None)

set :: Sudoku -> (Int, Int) -> SudokuCell -> Sudoku
set Sudoku {..} ind newV = Sudoku $ getSudoku // [(ind, newV)]

(!) :: Sudoku -> (Int, Int) -> SudokuCell
sud ! pos = getSudoku sud A.! pos

getAxis :: ((Int, Int) -> Int) -> Sudoku -> [[SudokuCell]]
getAxis f =
  map (map snd)
    . groupBy ((==) `on` (f . fst))
    . sortBy (compare `on` (f . fst))
    . assocs
    . getSudoku

rows :: Sudoku -> [[SudokuCell]]
rows = getAxis snd

cols :: Sudoku -> [[SudokuCell]]
cols = getAxis fst

squares :: Sudoku -> [[SudokuCell]]
squares sudoku = [ getForStartPoint i j | i <- [1, 4, 7], j <- [1, 4, 7] ]
 where
  getForStartPoint a b =
    [ sudoku ! (x, y) | x <- [a .. a + 2], y <- [b .. b + 2] ]

isValid :: Sudoku -> Bool
isValid sudoku = all (isValidSection . collectNums)
  $ concat [rows sudoku, cols sudoku, squares sudoku]
 where
  collectNums :: [SudokuCell] -> [Int]
  collectNums = foldl'
    (\acc mn -> case mn of
      Flex  n -> n : acc
      Fixed n -> n : acc
      _       -> acc
    )
    []
  isValidSection :: [Int] -> Bool
  isValidSection = null . (\\ aValues)
  aValues :: [Int]
  aValues = [1 .. 9]

solve :: MonadPlus m => Int -> Sudoku -> m Sudoku
solve seed = solveFrom 0
 where
  solveFrom 81 sud = return sud
  solveFrom n  sud = case cell of
    Fixed _ -> solveFrom (n + 1) sud
    _       -> msum $ map (solveFrom (n + 1)) nextIts
   where
    cell    = sud ! pos
    pos     = bimap (+ 1) (+ 1) $ divMod n 9
    nextIts = filter
      isValid
      (map (\k -> set sud pos $ Flex k) (shuffle' [1 .. 9] 9 (mkStdGen seed)))


randomSudoku :: Int -> Int -> Maybe Sudoku
randomSudoku seed diff = flexToFixed <$> remove diff rns initialField
 where
  rns = shuffle' [ (x, y) | x <- [1 .. 9], y <- [1 .. 9] ] 81 $ mkStdGen seed
  Just initialField = solve seed emptySudoku
  uniqueSolution [_] = True
  uniqueSolution _   = False
  remove 0 _        sud = Just sud
  remove _ []       _   = Nothing
  remove n (x : xs) sud = case sud ! x of
    None -> remove n xs sud
    Flex _ ->
      let newS = set sud x None
      in  if uniqueSolution $ solve seed newS
            then remove (n - 1) xs newS
            else remove n xs sud
  flexToFixed =
    Sudoku
      . array ((1, 1), (9, 9))
      . map
          (\(i, v) ->
            ( i
            , case v of
              Flex n -> Fixed n
              x      -> x
            )
          )
      . assocs
      . getSudoku


pp :: Sudoku -> String
pp = intercalate "\n" . map (map toSym) . rows
 where
  toSym (Fixed n) = intToDigit n
  toSym (Flex  n) = intToDigit n
  toSym _         = '.'
