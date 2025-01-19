main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter (\ds -> all (inRng 1 3) ds || all (inRng (-3) (-1)) ds) . fmap diffs

b = show . length . filter (\ds -> allButOneLinearTime (inRng 1 3) ds || allButOneLinearTime (inRng (-3) (-1)) ds) . fmap diffs
    where
        allButOneLinearTime :: (Int -> Bool) -> [Int] -> Bool
        allButOneLinearTime ok []     = True
        allButOneLinearTime ok (x:xs) = go Nothing x xs
            where
                go :: Maybe Int -> Int -> [Int] -> Bool
                go _  y []     = True
                go xs y (z:zs) = if ok y then go (Just y) z zs else case xs of
                    Nothing -> (     ok         z  ||     ok    (z + y) ) && all ok zs
                    Just x  -> ( all ok [y + x, z] || all ok [x, z + y] ) && all ok zs

inRng l h x = l <= x && x <= h

diffs xs = zipWith (-) xs (drop 1 xs)
