import           Data.Char       (isDigit)
import           Data.List       (group)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as Set

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . pics xybounds . readRobots

a :: [Pic] -> String
a = show . product . Map.fromListWith (+) . fmap (,1) . mapMaybe (maybeQuadrant xybounds . fst) . (!! 100)
    where
        maybeQuadrant :: V2 -> V2 -> Maybe (Int, Int)
        maybeQuadrant (xmax, ymax) (x, y) = if x == x' || y == y' then Nothing else Just (x `quot` (x' + 1), y `quot` (y' + 1))
            where
                x' = xmax `div` 2
                y' = ymax `div` 2

b = unlines . fmap (\(i, pic) -> unlines [show i, pretty xybounds pic]) . take 1 . filter (\(_, p) -> juanJoyaBorja xybounds p) . zip [0..]
    where
        juanJoyaBorja (xmax, ymax) p = any (\y -> (any ((> 10) . length) . filter head . group . fmap (\x -> Set.member (x, y) s)) [0..xmax-1]) [0..ymax-1]
            where
                s = Set.fromList . fmap fst $ p

xybounds = (101, 103)

type Pic = [PV]
type PV = (V2, V2)
type V2  = (Int, Int)

pretty :: V2 -> Pic -> String
pretty (xmax, ymax) xs = let x' = xmax `div` 2; y' = ymax `div` 2 in unlines [ [ if x == x' || y == y' then '*' else let k = length $ filter (== (x, y)) . fmap fst $ xs in if k == 0 then '_' else head (show k) | x <- [0..xmax-1] ] | y <- [0..ymax-1] ]

pics :: V2 -> Pic -> [Pic]
pics bounds = iterate (fmap (\(p, v) -> (,v) $ zipv mod (zipv (+) p v) bounds))

zipv :: (Int -> Int -> Int) -> V2 -> V2 -> V2
zipv f (x, y) (x', y') = (f x x', f y y')

readRobots :: String -> Pic
readRobots = fmap parseLine . lines
    where
        parseLine = (\(x:y:dx:dy:_) -> ((x, y), (dx, dy))) . fmap read . words . fmap (\c -> if isDigit c || c == '-' then c else ' ')
