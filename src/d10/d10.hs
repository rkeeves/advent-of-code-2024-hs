import           Data.Char       (digitToInt)
import           Data.Function   (on)
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readGrid

a = show . sum . fmap (Set.size . cata (\(v, n) sets -> Set.unions ((if n == 9 then Set.singleton v else Set.empty):sets))) . forest 0

b = show . sum . fmap (cata (\(v, n) sums -> (if n == 9 then 1 else 0) + sum sums)) . forest 0

data Rose a = Rose a [Rose a] deriving (Show)
type V2 = (Int, Int)

forest :: Int -> Map Int [V2] -> [Rose (V2, Int)]
forest n g = maybe [] (let fs = forest (n + 1) g in fmap (\v -> Rose (v, n) (filter (\(Rose (v', _) _) -> 1 == manhattan v v') fs))) $ g !? n

cata f (Rose v xs) = f v (fmap (cata f) xs)

manhattan (x, y) (x', y') = abs (x' - x) + abs (y' - y)

readGrid :: String -> Map Int [V2]
readGrid = Map.fromListWith (++) . concat . zipWith (\y -> zipWith (\x c -> (digitToInt c, [(x, y)])) [0..]) [0..] . lines
