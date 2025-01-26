import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap read . words . head . lines

a :: [Int] -> String
a = show . sum . IntMap.elems . (!! 25) . iterate nextMap . IntMap.fromListWith (+) . fmap (,1)

b = show . sum . IntMap.elems . (!! 75) . iterate nextMap . IntMap.fromListWith (+) . fmap (,1)

nextMap :: IntMap Int -> IntMap Int
nextMap = IntMap.foldlWithKey' (\m x n -> IntMap.unionWith (+) m $ IntMap.fromListWith (+) . fmap (,n) . nextNums $ x) IntMap.empty

nextNums :: Int -> [Int]
nextNums x
    | x == 0    = [1]
    | even n    = [read $ take (n `div` 2) xs, read $ drop (n `div` 2) xs]
    | otherwise = [2024 * x]
    where
        xs = show x
        n  = length xs
