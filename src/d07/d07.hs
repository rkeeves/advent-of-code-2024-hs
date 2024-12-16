import           Data.Char          (isDigit)
import           Data.List          (foldl', stripPrefix)
import           Data.Maybe         (mapMaybe)
import           GHC.Settings.Utils (maybeRead)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readEqs

a = show . sum . fmap head . filter (\(x:xs) -> elem 0 $ solves [minus, divide] x (reverse xs))

b = show . sum . fmap head . filter (\(x:xs) -> elem 0 $ solves [minus, divide, deconc] x (reverse xs))

minus x y = if x < y then Nothing else Just (x - y)

divide x y = if x < y || x `mod` y /= 0 then Nothing else Just (x `div` y)

deconc :: Int -> Int -> Maybe Int
deconc x y = let x' = reverse $ show x; y' = reverse $ show y; in stripPrefix y' x' >>= maybeRead . reverse

solves :: [Int -> Int -> Maybe Int] -> Int -> [Int] -> [Int]
solves ops x0 = foldl' (\xs y -> concatMap (\x -> mapMaybe (\f -> f x y) ops) xs) [x0]

readEqs :: String -> [[Int]]
readEqs = fmap (fmap read . words .  filter (\c -> isDigit c || c == ' ')) . lines
