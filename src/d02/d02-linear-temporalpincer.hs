import           Data.Maybe (listToMaybe)
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter (\xs -> good (inRng 1 3) xs || good (inRng (-3) (-1)) xs)
    where
        good p xs = and . zipWith p xs $ tail xs

b = show . length . filter (\xs -> temporalpincer (inRng 1 3) xs || temporalpincer (inRng (-3) (-1)) xs)
    where
        temporalpincer :: (a -> a -> Bool) -> [a] -> Bool
        temporalpincer p xs = case findIndices (\x y -> not $ p x y) 1 xs of
            [] -> True
            rb -> let n = length xs in any (\k -> k < 1 || (n - 2) < k || p (xs !! (k - 1)) (xs !! (k + 1))) [blue..red]
                where
                    n    = length xs
                    blue = last rb - 1
                    red  = head rb
        findIndices :: (a -> a -> Bool) -> Int -> [a] -> [Int]
        findIndices p i (a:b:xs) = if p a b then i : findIndices p (i + 1) (b:xs) else findIndices p (i + 1) (b:xs)
        findIndices p _ _ = []

inRng l h x y = let d = y - x in l <= d && d <= h

diffs xs = zipWith (flip (-)) xs (tail xs)
