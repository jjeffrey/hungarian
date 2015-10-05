module Hungarian (hungarianMin, hungarianMax) where
import Data.Matrix
import Data.List
import qualified Data.Vector as Vector

test = do
        let testMatrix = fromLists [[90,75,75,80],[35,85,55,65],[125,95,90,105],[45,110,95,115]]
        let brideGroomMatrix = fromLists [[7,4,7,3,10],[5,9,3,8,7],[3,5,6,2,9],[6,5,0,4,8],[0,0,0,0,0]]
        let matrix1a = fromLists [[17,4,10],[15,5,8],[18,7,11]]
        let matrix1b = fromLists [[3,-2,0,1],[5,3,-3,4],[2,7,5,3],[5,-2,0,1]]
        let matrix5a = fromLists [[150,65,210,135,0],[175,75,230,155,0],[135,85,200,140,0],[140,70,190,130,0],[170,50,200,160,0]]
        let matrix6 = fromLists [[20,15,10,10,17,23,25,5,15]
                                ,[10,10,12,15,9,7,8,7,8]
                                ,[12,9,9,10,10,5,7,13,9]
                                ,[13,14,10,15,15,5,7,13,9]
                                ,[12,13,10,15,14,5,9,20,10]
                                ,[15,14,14,16,14,5,10,20,10]
                                ,[7,9,12,12,7,6,7,15,12]
                                ,[5,6,8,8,5,4,5,10,7]
                                ,[5,6,8,8,5,4,5,10,7]]
        putStrLn "Bulldozer Test"
        putStrLn "Bulldozer Matrix"
        print testMatrix
        putStrLn "Optimal assignment of Bulldozer Matrix"
        print $ hungarianMin testMatrix
        putStrLn "Bride-Groom Test"
        putStrLn "BrideGroom Matrix"
        print brideGroomMatrix
        putStrLn "Optimal assignment of Bride-Groom Matrix"
        print $ hungarianMax brideGroomMatrix
        putStrLn "Problem #1"
        putStrLn "Matrix a"
        print matrix1a
        putStrLn "Optimal assignment of Matrix a"
        print $ hungarianMin matrix1a
        putStrLn "Matrix b"
        print matrix1b
        putStrLn "Optimal assignment of Matrix b"
        print $ hungarianMin matrix1b
        putStrLn "Matrix 5a (Bidding Coins)"
        putStrLn "Matrix 5a"
        print matrix5a
        putStrLn "Optimal assignment of Matrix 5a"
        print $ hungarianMax matrix5a
        putStrLn "Matrix 6 (Players)"
        putStrLn "Matrix 6"
        print matrix6
        putStrLn "Optimal assignment of Matrix 6"
        print $ hungarianMax matrix6


data MatrixLine = RowLine Int | ColLine Int deriving (Eq, Show)

isRowLine :: MatrixLine -> Bool
isRowLine (RowLine _) = True
isRowLine _ = False

isColLine :: MatrixLine -> Bool
isColLine (ColLine _) = True
isColLine _ = False

lineValue :: MatrixLine -> Int
lineValue (RowLine num) = num
lineValue (ColLine num) = num

rowLinesOf :: [MatrixLine] -> [MatrixLine]
rowLinesOf = filter isRowLine

colLinesOf :: [MatrixLine] -> [MatrixLine]
colLinesOf = filter isColLine



--EXPORTED FUNCTIONS--

hungarianMin :: Matrix Int -> Matrix Int
hungarianMin matr = let smatr = subtractMins matr
                        srch = findInMatrix 0 smatr
                        pp = producePairs srch
                        newPairs = removeRedundantPairs srch pp
                        in optimize 0 smatr srch newPairs

hungarianMax :: Matrix Int -> Matrix Int
hungarianMax matr = hungarianMin $ mapOverSearch (\num -> num * (-1)) (searchMatrix (const True) matr) matr



--MATRIX OPTIMIZATION--

optimize :: Int -> Matrix Int -> [(Int,Int)] -> [(MatrixLine,Int)] -> Matrix Int
optimize val matr srch pp =
        if isOptimal matr srch pp
                then matr
                else let matr2 = moreOptimizedMatrix matr (map fst pp)
                         newSearch = findInMatrix val matr2
                         minPairs = removeRedundantPairs newSearch $ producePairs newSearch
                         in optimize val matr2 newSearch minPairs

moreOptimizedMatrix :: Matrix Int -> [MatrixLine] -> Matrix Int
moreOptimizedMatrix matr mls = let rowSearch = searchLines (rowLinesOf mls) (const True) matr
                                   colSearch = searchLines (colLinesOf mls) (const True) matr
                                   search = union rowSearch colSearch
                                   invs = invertSearch search matr
                                   min = minimumIn matr invs
                                   matr1 = mapOverSearch (\x -> x - min) invs matr
                                   in mapOverSearch (+ min) (rowSearch `intersect` colSearch) matr1

--True if isConsistent and number of matrix lines is equal to n
isOptimal :: Matrix a -> [(Int,Int)] -> [(MatrixLine,Int)]  -> Bool
isOptimal matr srch mls = isConsistent (map fst mls) srch && length mls == nrows matr && length mls == ncols matr

--True if the matrix lines cover all of the target values
isConsistent :: [MatrixLine] -> [(Int,Int)] -> Bool
isConsistent mls = all (`locationInLines` mls)



--SUBTRACT MINS--

--Initial Step: subtracts minimum value of each row and column of matrix from that row or column.
subtractMins :: Matrix Int -> Matrix Int
subtractMins = fromLists . Data.List.transpose . map subtractMinFromList . Data.List.transpose . map subtractMinFromList . toLists

--subtracts minimum value from list
subtractMinFromList :: [Int] -> [Int]
subtractMinFromList array = map (\num -> num - minimum array) array



--MATRIXLINE PAIRS--

--produces all lines that cross through search
producePairs :: [(Int,Int)] -> [(MatrixLine,Int)]
producePairs locs = let rowGrouped = groupLocs True $ sortLocs True locs
                        colGrouped = groupLocs False $ sortLocs False locs
                        in map (\gr -> (RowLine (fst $ head gr), length gr)) rowGrouped ++ map (\gr -> (ColLine (snd $ head gr), length gr)) colGrouped

removeRedundantPairs :: [(Int,Int)] -> [(MatrixLine, Int)] -> [(MatrixLine, Int)]
removeRedundantPairs srch pairs = let spairs = sortPairs pairs
                                      ml = fst $ last spairs
                                      npairs = iterateRemove (isRowLine ml) (findInLineLocs ml srch) spairs
                                      in if length npairs <= 2 then npairs else removeRedundantPairs srch (init npairs) ++ [last npairs]

iterateRemove ::  Bool -> [Int] -> [(MatrixLine, Int)]  -> [(MatrixLine, Int)]
iterateRemove isRow locs spairs = dropZeroPairs $ if isRow
                                                        then foldr (\x acc -> map (reduceMatchingColPair x) acc) spairs locs
                                                        else foldr (\x acc -> map (reduceMatchingRowPair x) acc) spairs locs

dropZeroPairs :: [(MatrixLine, Int)] -> [(MatrixLine, Int)]
dropZeroPairs = filter (\(ml,y) -> y /= 0)


reduceMatchingRowPair :: Int -> (MatrixLine, Int) -> (MatrixLine, Int)
reduceMatchingRowPair x (RowLine y, z) = if x == y then (RowLine y, z - 1) else (RowLine y, z)
reduceMatchingRowPair _ (ColLine y, z) = (ColLine y, z)

reduceMatchingColPair :: Int -> (MatrixLine, Int) -> (MatrixLine, Int)
reduceMatchingColPair x (ColLine y, z) = if x == y then (ColLine y, z - 1) else (ColLine y, z)
reduceMatchingColPair _ (RowLine y, z) = (RowLine y, z)


addToRowPair :: Int -> (MatrixLine, Int) -> (MatrixLine, Int)
addToRowPair x (RowLine y, z) = (RowLine y, x + z)
addToRowPair _ (ColLine y, z) = (ColLine y, z)

addToColPair :: Int -> (MatrixLine, Int) -> (MatrixLine, Int)
addToColPair x (ColLine y, z) = (ColLine y, x + z)
addToColPair _ (RowLine y, z) = (RowLine y, z)


sortPairs :: [(MatrixLine,Int)] -> [(MatrixLine,Int)]
sortPairs = sortBy comparePairs

comparePairs :: (MatrixLine, Int) -> (MatrixLine, Int) -> Ordering
comparePairs pairX pairY | x < y = LT
                         | x > y = GT
                         | x == y = EQ
                         where x = snd pairX
                               y = snd pairY


--LOCATIONS--

groupLocs :: Bool -> [(Int,Int)] -> [[(Int,Int)]]
groupLocs b = groupBy (eqLocs b)

sortLocs :: Bool -> [(Int,Int)] -> [(Int,Int)]
sortLocs b = sortBy (compareLocs b)

eqLocs :: Bool -> (Int,Int) -> (Int, Int) -> Bool
eqLocs byRow pairX pairY = let x = if byRow then fst pairX else snd pairX
                               y = if byRow then fst pairY else snd pairY
                               in x == y

compareLocs :: Bool -> (Int,Int) -> (Int, Int) -> Ordering
compareLocs byRow pairX pairY | x < y = LT
                              | x > y = GT
                              | x == y = EQ
                              where x = if byRow then fst pairX else snd pairX
                                    y = if byRow then fst pairY else snd pairY

--Locates where values on line appears in matrix
findInLineLocs :: MatrixLine -> [(Int,Int)] -> [Int]
findInLineLocs (RowLine num) = foldr (\(x,y) acc -> if x == num then y:acc else acc) []
findInLineLocs (ColLine num) = foldr (\(x,y) acc -> if y == num then x:acc else acc) []

--Checks if location is on any of the matrix lines
locationInLines :: (Int,Int) -> [MatrixLine] -> Bool
locationInLines loc = any (\ml -> if isRowLine ml then lineValue ml == fst loc
                                                  else lineValue ml == snd loc)



--SEARCHES--

--Locates where condition-satisfying values appear on line in matrix
searchLine :: (Eq a) => MatrixLine -> (a -> Bool) -> Matrix a -> [(Int,Int)]
searchLine (RowLine num) f = zip (repeat num) . findIndices f . Vector.toList . getRow (num + 1)
searchLine (ColLine num) f = (\js -> zip js (repeat num)) . findIndices f . Vector.toList . getCol (num + 1)

findInLine ml val = searchLine ml (==val)

--Locates where condition-satisfying values appear on a list of lines in matrix
searchLines :: (Eq a) => [MatrixLine] -> (a -> Bool) -> Matrix a -> [(Int,Int)] 
searchLines mls f matr = removeDuplicates $ concatMap (\ml -> searchLine ml f matr) mls

findInLines mls val = searchLines mls (==val)

--Locates where condition-satisfying values appears anywhere in matrix
searchMatrix :: (Eq a) => (a -> Bool) -> Matrix a -> [(Int,Int)]
searchMatrix f = fst . foldl (\(search, rowNum) row-> (zip (repeat rowNum) (findIndices f row) ++ search, rowNum + 1)) ([],0) . toLists

findInMatrix num = searchMatrix (==num)


--inverts the search
invertSearch :: [(Int,Int)] -> Matrix a -> [(Int,Int)]
invertSearch arr matr = pairs (nrows matr - 1) (ncols matr - 1) \\ arr


--maps a function over a search
mapOverSearch :: (a -> a) -> [(Int, Int)] -> Matrix a -> Matrix a
mapOverSearch f arr matri = foldr (applyInMatrix f) matri arr

--foldr over a search [NOT USED]
foldrOverSearch :: (a -> b -> b) -> b -> [(Int,Int)] -> Matrix a -> b
foldrOverSearch f acc arr matri = foldr (\(i,j) acc -> f (matri!(i+1,j+1)) acc) acc arr


--Finds minimum value within a search
minimumIn :: (Ord a) => Matrix a -> [(Int,Int)] -> a
minimumIn matr mls = minimum $ getFromSearch matr mls

getFromSearch :: Matrix a -> [(Int,Int)] -> [a]
getFromSearch matr = map (\(i,j)-> matr!(i+1,j+1))



--FUNCTION APPLICATION--

--applies function to a location in matrix
applyInMatrix :: (a -> a) -> (Int,Int) -> Matrix a -> Matrix a
applyInMatrix f ni = fromLists . applyAtNestedIndex f ni . toLists

--applies function to an index in a nested list
applyAtNestedIndex :: (a -> a) -> (Int,Int) -> [[a]] -> [[a]]
applyAtNestedIndex f (i,j) = applyAtIndex (applyAtIndex f j) i

--applies function to index in a list
applyAtIndex :: (a -> a) -> Int -> [a] -> [a]
applyAtIndex f i = (\(x,y:ys) -> x ++ f y : ys) . splitAt i



--MISCELLANEOUS--

--From http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

--Creates all combinations of pairs from max a and b
pairs :: Int -> Int -> [(Int,Int)]
pairs aMax bMax = [(a,b) | a <- [0 .. aMax], b <- [0 .. bMax]]