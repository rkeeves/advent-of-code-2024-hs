import           Data.List       (nub, uncons)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main = interact $ unlines . (\x -> ["A", uncurry a x, "B", uncurry b x]) . readGrid

type V2   = (Int, Int)

a :: Int -> [[V2]] -> String
a n = show . length . nub . flatMapEdges (\(a, bs) -> mapMaybe (\b -> let xs = nextsWhile (inBound 0 n) a b in fmap fst . uncons . drop 1 $ xs) bs)

b :: Int -> [[V2]] -> String
b n = show . length . nub . flatMapEdges (\(a, bs) -> concatMap (\b -> let xs = nextsWhile (inBound 0 n) a b in if length bs < 2 then drop 1 xs else xs) bs)

flatMapEdges :: ((V2, [V2]) -> [a]) -> [[V2]] -> [a]
flatMapEdges f = concatMap (concatMap f . edges)

edges :: [V2] -> [(V2, [V2])]
edges xs = fmap (\x -> (x, filter (/= x) xs)) xs

nextsWhile :: (V2 -> Bool) -> V2 -> V2 -> [V2]
nextsWhile p a b = let d = (b |-| a) in takeWhile p (if d == (0, 0) then [a] else iterate (|+| d) b)

inBound :: Int -> Int -> V2 -> Bool
inBound lo hi (x, y) = lo <= x && x < hi && lo <= y && y < hi

(|+|) (x, y) (x', y') = (x + x', y + y')

(|-|) (x, y) (x', y') = (x - x', y - y')

readGrid :: String -> (Int, [[V2]])
readGrid s = (length $ lines s, Map.elems . Map.fromListWith (++) . filter ((/= '.') . fst) . zipMx (\c v -> (c, [v])) . lines $ s)

zipMx :: (Char -> V2 -> a) -> [[Char]] -> [a]
zipMx f = concat . zipWith (\y cs -> zipWith (\x c -> f c (x, y)) [0..] cs) [0..]
