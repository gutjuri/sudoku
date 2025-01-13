import Data.Aeson
import Data.Sudoku
import qualified  Data.ByteString.Lazy as B

main :: IO ()
main = do
  case encode <$> randomSudoku 12 34 of
    Just s -> B.putStrLn s
    _ -> return ()
  print =<< deserialize <$> B.readFile "test.txt"