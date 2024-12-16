import           Data.List       (nub, unfoldr)
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readStartAndGrid

data Block = Free | Wall deriving Eq
type V2   = (Int, Int)
type V2D  = (V2, V2)

a :: (V2, Map V2 Block) -> String
a (v0, m) = show . length . nub . fmap fst $ pathFrom (v0, (0, -1)) m

b :: (V2, Map V2 Block) -> String
b (v0, m) = show . length . nub . mapMaybe (\v -> if or . zipFloyd (==) $ pathFrom (v0, (0, -1)) (Map.insert v Wall m) then Just v else Nothing) $ walls
    where walls = nub [ v | v <- drop 1 . fmap (uncurry (|+|)) $ pathFrom (v0, (0, -1)) m, m !? v == Just Free ]

zipFloyd :: (a -> a -> b) -> [a] -> [b]
zipFloyd f xs = unfoldr (\(xs, ys) -> if null xs || null ys then Nothing else Just (f (head xs) (head ys), (drop 1 xs, drop 2 ys))) (drop 1 xs, drop 2 xs)

pathFrom :: V2D -> Map V2 Block -> [V2D]
pathFrom vd0 m = unfoldr (>>= (\vd@(v, d) -> let v' = v |+| d; d' = clock d in Just (vd, (\c -> if c == Wall then (v, d') else (v', d)) <$> (m !? v')))) $ Just vd0

readStartAndGrid :: String -> (V2, Map V2 Block)
readStartAndGrid s = (head . Map.keys . Map.filter (== '^') $ m, Map.map (\c -> if c == '#' then Wall else Free) m)
    where m = Map.fromList . concat . zipWith (\y cs -> zipWith (\x c -> ((x, y), c)) [0..] cs) [0..] . lines $ s

(|+|) (x, y) (x', y') = (x + x', y + y')

clock (x, y) = (-y, x)
