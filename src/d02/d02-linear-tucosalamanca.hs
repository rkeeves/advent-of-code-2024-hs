import           Data.List  (foldl')
import           Data.Maybe (listToMaybe)
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a = show . length . filter (\xs -> good (inRng 1 3) xs || good (inRng (-3) (-1)) xs)
    where
        good p xs = and . zipWith p xs $ tail xs

b = show . length . filter (\xs -> g (foldl' (f (inRng 1 3)) Empty xs) || g (foldl' (f (inRng (-3) (-1))) Empty xs))
    where
        f :: (Int -> Int -> Bool) -> Tuco -> Int -> Tuco
        f p Empty a = One a
        f p (One a)     b = if p a b then a `And` b else a `Or` b
        f p (a `Or ` b) c = if p a c || p b c then Tail c else Reject
        f p Reject _ = Reject
        f p (Tail a) b = if p a b then Tail b else Reject
        f p (a `And` b) c
          | p b c = b `And` c
          | p a c = b `Or` c
          | otherwise = Tail b
        g Reject = False
        g _      = True

data Tuco = Empty | One Int | Or Int Int | Reject | Tail Int | And Int Int deriving (Show)

inRng l h x y = let d = y - x in l <= d && d <= h

diffs xs = zipWith (flip (-)) xs (tail xs)
