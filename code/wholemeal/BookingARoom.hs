import           Control.Arrow ((>>>))
import qualified Data.Set      as S

main :: IO ()
main = interact $ words >>> parse >>> solve

data Hotel = Hotel
  { numRooms :: Int
  , booked   :: [Int]
  }

parse :: [String] -> Hotel
parse (r:_:rs) = Hotel (read r) (map read rs)

solve :: Hotel -> String
solve (Hotel r rs) =
  maybe "too late" show . S.lookupMin
  $ S.fromList [1..r] `S.difference` S.fromList rs
