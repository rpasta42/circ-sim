import qualified Data.Matrix as M

--cabal install matrix
--https://hackage.haskell.org/package/matrix-0.3.5.0/docs/Data-Matrix.html


{-|

m = |a b c|
    |d e f|

m = [[a, b, c], [d, e, f]]

nrows = 2
ncols = 3

-}

--data Matrix a = ListMatrix [[a]]



lst = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

m = M.fromLists lst

{-|

accessing:
m M.! (1, 2) = 2
m M.! (2, 3) = 6

M.getElem 2 3 m

-}


--pair of minor matrices and coefficients
{-|getMinorPairs :: (Num a) => Matrix a -> [(Matrix a, a)]
getMinorPairs m = getMinorPairs' m 0

getMinorPairs' :: (Num a) => [[a]] -> Int -> [([[a]], a)]
getMinorPairs' (coeffs:rest) n =
   (coeffs !! n,
-}

excludeColumn :: (Num a) => [[a]] -> Int -> [[a]]
excludeColumn m i = map (excludeIndex i) m

excludeIndex :: [a] -> Int -> [a]
excludeIndex excludeIndex = fst . foldr
   (\item (index, lst) -> if index == excludeIndex then (index+1, lst) else (index+1, item : lst))
   (0, [])

--determinant :: (Num a) => [[a]] -> a
--determinant xs = determinant' (M.fromLists xs)

--determinant' :: (Num a) => Matrix a -> a
--determinant' m
--   | nrows m == 0 && ncols m == 0 = error "empty matrix"
--   |



--bad
--determinant [] = error "empty matrix"
--determinant [[x]] =

