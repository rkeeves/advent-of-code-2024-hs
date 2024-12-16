import           Data.List (inits, tails)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter isGood

b = show . length . filter (any isGood . skipOnes)

isGood :: [Int] -> Bool
isGood xs = all (1 `to` 3) (differences xs) || all ((-3) `to` (-1)) (differences xs)
    where to lo hi d = lo <= d && d <= hi

skipOnes xs = zipWith (++) (inits xs) (tail (tails xs))

differences xs = zipWith (-) xs (drop 1 xs)
