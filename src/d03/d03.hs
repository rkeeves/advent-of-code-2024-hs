import           Data.Char                    (isDigit)
import           Data.List                    (foldl', tails)
import           Text.ParserCombinators.ReadP (ReadP, char, count, readP_to_S,
                                               satisfy, string, (<++))

main = interact $ unlines . (\x -> ["A", a x, "B", b x])

a = show . sum . greedy mult

b = show . snd . foldl' add (1, 0) . greedy ((Right <$> mult) <++ (Left 1 <$ string "do()")  <++ (Left 0 <$ string "don't()"))
    where
        add (_, n) (Left k') = (k', n)
        add (k, n) (Right x) = (k, n + k * x)

mult :: ReadP Int
mult =  (\_ x _ y _ -> x * y) <$> string "mul(" <*> num <*> char ',' <*> num <*> char ')'
    where
        num    = digs 3 <++ digs 2 <++ digs 1
        digs n = read <$> count n (satisfy isDigit)

greedy p [] = []
greedy p s  = case readP_to_S p s of
    (k, s') : _ -> k : greedy p s'
    _           -> greedy p (drop 1 s)
