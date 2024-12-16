import           Data.Bifunctor     (Bifunctor (bimap))
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . bimap frequency frequency . unzip . fmap ((\(x:y:_) -> (read x, read y)) . words) . lines

a :: (IntMap Int, IntMap Int) -> String
a = show . sum . uncurry (zipWith (\l r -> abs (l - r))) . bimap unfrequency unfrequency

b = show . sum . IntMap.elems . uncurry (IntMap.intersectionWithKey (\x n n' -> x * n * n'))

frequency = IntMap.fromListWith (+) . map (,1)

unfrequency = concatMap (\(k, n) -> replicate n k) . IntMap.assocs
