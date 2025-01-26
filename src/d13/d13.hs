import           Data.Char  (isDigit)
import           Data.Maybe (mapMaybe)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readBlocks

a = show . sum . mapMaybe (fmap (\(a, b) -> 3 * a + b) . lineqsys)

b = show . sum . mapMaybe ((fmap (\(a, b) -> 3 * a + b) . lineqsys) . (\(a, b, (cx, cy)) -> (a, b, (cx + k, cy + k))))
    where k = 10_000_000_000_000

lineqsys ((ax, ay), (bx, by), (cx, cy)) = if u `mod` v == 0 && u' `mod` v' == 0 then Just (a, b) else Nothing
    where
        u = (bx * cy) - (by * cx)
        v = (bx * ay) - (by * ax)
        a = u `div` v
        u' = cx - (ax * a)
        v' = bx
        b = u' `div` v'

type V2 = (Integer, Integer)

readBlocks :: String -> [(V2, V2, V2)]
readBlocks = fmap (block . take 3) . chunks 4 . lines
    where
        chunks n xs = if null xs then [] else take n xs : chunks n (drop n xs)
        block (a:b:c:_) = (pair a, pair b, pair c)
        pair = read . ('(':) . foldr (:) [')'] . filter (\c -> isDigit c || ',' == c)
