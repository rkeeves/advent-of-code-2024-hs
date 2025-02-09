main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter (\xs -> temporalpincer 0 (inRng 1 3) xs || temporalpincer 0 (inRng (-3) (-1)) xs)

b = show . length . filter (\xs -> temporalpincer 1 (inRng 1 3) xs || temporalpincer 1 (inRng (-3) (-1)) xs)

inRng l h x y = let d = y - x in l <= d && d <= h

temporalpincer k p xs = or $ zipWith merge prefixes (drop k suffixes)
    where
        merge (good, Just a) (good', Just b) = good && good' && p a b
        merge (good, _     ) (good', _     ) = good && good'
        prefixes = scanl (\(good, ma) b -> (maybe good (\a -> good && p a b) ma, Just b)) (True, Nothing) xs
        suffixes = scanr (\a (good, mb) -> (maybe good (\b -> p a b && good) mb, Just a)) (True, Nothing) xs
