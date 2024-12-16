import           Data.List       (isPrefixOf, uncons)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readMx

a :: Map V2 Char -> String
a m = show . sum . fmap (length . filter (isPrefixOf "XMAS" . pathToWord m) . infinitePathsFrom) . Map.keys $ m

b :: Map V2 Char -> String
b m = show . length . filter ((\(xs, ys) -> (xs == "SAM" || xs == "MAS") && (ys == "SAM" || ys == "MAS")) . crossToWords m) . Map.keys $ m

type V2 = (Int, Int)

crossToWords :: Map V2 Char -> V2 -> (String, String)
crossToWords m v0 = (toWord [(-1, -1), (0, 0), ( 1, 1)], toWord [( 1, -1), (0, 0), (-1, 1)])
            where toWord = pathToWord m . fmap (v0 |+|)

infinitePathsFrom :: V2 -> [[V2]]
infinitePathsFrom v0 = fmap (\v -> iterate (|+| v) v0) [ (x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

pathToWord :: Map V2 Char -> [V2] -> String
pathToWord m = maybe [] (\(v, vs) -> maybe [] (: pathToWord m vs) . Map.lookup v $ m) . uncons

readMx :: String -> Map V2 Char
readMx = Map.fromList . concat . zipWith (\y cs -> zipWith (\x c -> ((x, y), c)) [0..] cs) [0..] . lines

(|+|) (x, y) (x', y') = (x + x', y + y')
