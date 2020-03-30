import           Test.Hspec
import           Data.Sudoku
import           Data.Array

main :: IO ()
main = hspec $ do
  describe "Sudoku.emptySudoku" $ do
    it "creates an empty sudoku field filled with nothings" $ do
      elems emptySudoku `shouldBe` replicate 81 None

  describe "Sudoku.newSudoku" $ do
    it "creates a sudoku field based on user input 1" $ do
      newSudoku (concat testSudoku1)
        `shouldSatisfy` isCorrect testSudoku1

    it "creates a sudoku field based on user input 2" $ do
      newSudoku (concat testSudoku2)
        `shouldSatisfy` isCorrect testSudoku2

  describe "Sudoku.isValid" $ do
    it "can validate a fully filled correct sudoku" $ do
      isValid (newSudoku (concat testSudoku2)) `shouldBe` True

    it "can reject a fully filled incorrect sudoku" $ do
      isValid (newSudoku (concat testSudoku1)) `shouldBe` False


isCorrect :: [[Int]] -> Sudoku -> Bool
isCorrect ls sud = and
  [ sud ! (x, y) == Fixed (ls !! (y - 1) !! (x - 1)) | x <- [1 .. 9], y <- [1 .. 9] ]

testSudoku1 :: [[Int]]
testSudoku1 =
  [ [1, 2, 3, 4, 5, 6, 7, 8, 9]
  , [2, 3, 4, 5, 6, 7, 8, 9, 1]
  , [3, 4, 5, 6, 7, 8, 9, 1, 2]
  , [4, 5, 6, 7, 8, 9, 1, 2, 3]
  , [5, 6, 7, 8, 9, 1, 2, 3, 4]
  , [6, 7, 8, 9, 1, 2, 3, 4, 5]
  , [7, 8, 9, 1, 2, 3, 4, 5, 6]
  , [8, 9, 1, 2, 3, 4, 5, 6, 7]
  , [9, 1, 2, 3, 4, 5, 6, 7, 8]
  ]

testSudoku2 :: [[Int]]
testSudoku2 =
  [ [1, 2, 3, 4, 5, 6, 7, 8, 9]
  , [4, 5, 6, 7, 8, 9, 1, 2, 3]
  , [7, 8, 9, 1, 2, 3, 4, 5, 6]
  , [2, 3, 4, 5, 6, 7, 8, 9, 1]
  , [5, 6, 7, 8, 9, 1, 2, 3, 4]
  , [8, 9, 1, 2, 3, 4, 5, 6, 7]
  , [3, 4, 5, 6, 7, 8, 9, 1, 2]
  , [6, 7, 8, 9, 1, 2, 3, 4, 5]
  , [9, 1, 2, 3, 4, 5, 6, 7, 8]
  ]

sudoku1 =
  [ [0, 0, 0, 8, 0, 1, 0, 0, 3]
  , [0, 0, 5, 0, 0, 0, 0, 8, 4]
  , [0, 7, 6, 0, 0, 9, 0, 0, 0]
  , [0, 0, 0, 1, 0, 0, 0, 7, 0]
  , [4, 0, 0, 0, 3, 0, 0, 0, 5]
  , [0, 8, 0, 0, 0, 2, 0, 0, 0]
  , [0, 0, 0, 9, 0, 0, 8, 2, 0]
  , [9, 2, 0, 0, 0, 0, 1, 0, 0]
  , [6, 0, 0, 2, 0, 4, 0, 0, 0]
  ]