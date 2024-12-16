import           Data.Bifunctor (bimap)
import           Data.Char      (isDigit)
import           Data.List      (sortBy, tails)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readInput

a (order, xss) = show . sum . fmap mid . filter (isAscending order) $ xss

b (order, xss) = show . sum . fmap (mid . sortBy order) . filter (not . isAscending order) $ xss

mid xs = xs !! (length xs `div` 2)

isAscending :: (Int -> Int -> Ordering) -> [Int] -> Bool
isAscending f = all (all ((`elem` [LT, EQ]) . uncurry f) . (\(x : xs) -> fmap (x,) xs)) . init . tails

listToOrder abs a b
    | (a, b) `elem` abs = LT
    | (b, a) `elem` abs = GT
    | otherwise = EQ

readInput :: String -> (Int -> Int -> Ordering, [[Int]])
readInput = bimap (listToOrder . fmap (\[x, y] -> (x, y))) (drop 1) . break null . fmap (fmap read . words . fmap (\c -> if isDigit c then c else ' ')) . lines
