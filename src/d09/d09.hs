import           Data.Char       (digitToInt)
import           Data.List       (foldl', unfoldr)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readBlocks

a = show . checksum . compressBy (\n -> maybe (decrMaxId n) decrId) . concatMap (\(n, id) -> replicate n (1, id))
    where
        decrId    id m = decr id m =<< Map.lookup id m
        decrMaxId _  m = (\(id, n) -> decr id m n) =<< Map.lookupMax m
        decr      id m n = Just ((1, id), if n <= 1 then Map.delete id m else Map.insert id (n - 1) m)
b = show . checksum . compressBy (\n -> maybe (dropBestId n) dropId)
    where
        dropId     id m = drop id m =<< Map.lookup id m
        dropBestId n  m = (\(id, n') -> drop id m n') =<< (Map.lookupMax . Map.filter (<= n) $ m)
        drop       id m n = Just ((n, id), Map.delete id m)

type Id    = Int
type Block = (Int, Maybe Id)

checksum :: [Block] -> Int
checksum = snd . foldl' (\(i, s) (n, m) -> (i + n, s + maybe 0 (\id -> (((n - 1) * n `div` 2) + (n * i)) * id) m)) (0, 0)

compressBy :: (Int -> Maybe Int -> Map Id Int -> Maybe ((Int, Id), Map Id Int)) -> [Block] -> [Block]
compressBy f bs0 = unfoldr (\(m, bs) -> case bs of
    []          -> Nothing
    ((n, i):bs) -> case f n i m of
        Nothing             -> Just ((n,  Nothing), (m, bs))
        Just ((n', id), m') -> Just ((n', Just id), (m', if n - n' > 0 then (n - n', Nothing):bs else bs)))
        (freqs bs0, bs0)

freqs :: [Block] -> Map Id Int
freqs = Map.fromListWith (+) . mapMaybe (\(n, m) -> m >>= (Just . (,n)))

readBlocks :: String -> [Block]
readBlocks = zipWith (\i -> (, if even i then Just (i `div` 2) else Nothing)) [0 ..] . fmap digitToInt . head . lines
