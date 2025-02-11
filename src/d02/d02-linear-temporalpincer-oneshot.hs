import           Data.Maybe (catMaybes, listToMaybe)
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter (any (\(lo, hi) -> all (inRng 1 3) [lo, hi] || all (inRng (-3) (-1)) [lo, hi]) . temporalpincer 0)

b = show . length . filter (any (\(lo, hi) -> all (inRng 1 3) [lo, hi] || all (inRng (-3) (-1)) [lo, hi]) . temporalpincer 1)

inRng l h x = l <= x && x <= h

temporalpincer k xs = catMaybes $ zipWith merge (prefixes xs) (drop k $ suffixes xs)
    where
        prefixes :: [Int] -> [Maybe (Int, Maybe (Int, Int))]
        prefixes = scanl (\m b -> maybe (Just (b, Nothing)) (\(a, m') -> let d = b - a in maybe (Just (b, Just (d, d))) (\(l, h) -> Just (b, Just (min l d, max h d))) m') m) Nothing
        suffixes :: [Int] -> [Maybe (Int, Maybe (Int, Int))]
        suffixes = scanr (\a m -> maybe (Just (a, Nothing)) (\(b, m') -> let d = b - a in maybe (Just (a, Just (d, d))) (\(l, h) -> Just (a, Just (min l d, max h d))) m') m) Nothing
        merge :: Maybe (Int, Maybe (Int, Int)) -> Maybe (Int, Maybe (Int, Int)) -> Maybe (Int, Int)
        merge (Just (a, Just (l, h))) (Just (b, Just (l', h'))) = let d = b - a in Just (minimum [d, l, l'], maximum [d, h, h'])
        merge (Just (a, Just (l, h))) (Just (b, _))             = let d = b - a in Just (min d l, max d h)
        merge (Just (a, Just (l, h))) _                         = Just (l, h)
        merge (Just (a, _))           (Just (b, Just (l', h'))) = let d = b - a in Just (min d l', max d h')
        merge (Just (a, _))           (Just (b, _))             = let d = b - a in Just (d, d)
        merge (Just (a, _))           _                         = Nothing
        merge _                       (Just (b, Just (l', h'))) = Just (l', h')
        merge _                       (Just (b, _))             = Nothing
        merge _                       _                         = Nothing
        diffs xs = zip xs (drop 1 xs)
