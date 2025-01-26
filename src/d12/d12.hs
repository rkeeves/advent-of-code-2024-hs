import           Data.List       (foldl', unfoldr)
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . concatMap (fmap (\s -> (s, normals s)) . components) . Map.elems . readGrid

a :: [(Set V2, Set (V2, V2))] -> String
a = show . sum . fmap (\(tiles, norms) -> Set.size tiles * Set.size norms)

b :: [(Set V2, Set (V2, V2))] -> String
b = show . sum . fmap (\(tiles, norms) -> Set.size tiles * sides norms)
    where sides ns = length . filter (\(v, d@(x, y)) -> (v |+| (y, -x), d) `Set.notMember` ns) . Set.elems $ ns

type V2 = (Int, Int)
type Grid = Map V2 Char

normals :: Set V2 -> Set (V2, V2)
normals s = Set.fromList [ (v, d) | v <- Set.elems s, d <- cardinals,  (v |+| d) `Set.notMember` s ]

components :: Set V2 -> [Set V2]
components = unfoldr (fmap (\(v, s) -> dfs Set.empty [v] s) . Set.minView)
    where
        dfs :: Set V2 -> [V2] -> Set V2 -> (Set V2, Set V2)
        dfs seen []     rest = (seen, rest)
        dfs seen (v:vs) rest = dfs (Set.insert v seen) (vs' ++ vs) (foldl' (flip Set.delete) rest vs')
                where
                    vs' = filter (`Set.member` rest) . fmap (v |+|) $ cardinals

cardinals = [(0, 1), (0, -1), (1, 0), (-1, 0)]

(|+|) :: V2 -> V2 -> V2
(|+|) (x, y) (x', y') = (x + x', y + y')

readGrid :: String -> Map Char (Set V2)
readGrid = fmap Set.fromList . Map.fromListWith (++) . concat . zipWith (\y -> zipWith (\x c -> (c, [(x, y)])) [0..]) [0..] . lines
