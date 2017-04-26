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
getMinorPairs :: (Num a) => M.Matrix a -> [(a, M.Matrix a)]
getMinorPairs m = map (\(index, m) -> getMinorPairs' m index) (zip [0..] (repeat (M.toLists m)))


getMinorPairs' :: (Num a) => [[a]] -> Int -> (a, M.Matrix a)
getMinorPairs' (coeffs:rest) n = (coeffs !! n, M.fromLists $ excludeColumn' n rest)


excludeColumn :: Int -> M.Matrix a -> M.Matrix a
--excludeColumn i m = M.fromLists $ excludeColumn' i $ M.toLists m
excludeColumn i = M.fromLists . (excludeColumn' i) . M.toLists


excludeColumn' :: Int -> [[a]] -> [[a]]
excludeColumn' i m = map (excludeIndex i) m

excludeIndex :: Int -> [a] -> [a]
excludeIndex excludeIndex xs = snd $ foldr
   (\item (index, lst) -> if index == excludeIndex then (index-1, lst) else (index-1, item : lst))
   (length xs - 1, [])
   xs

--determinant :: (Num a) => [[a]] -> a
--determinant xs = determinant' (M.fromLists xs)

--determinant' :: (Num a) => Matrix a -> a
--determinant' m
--   | nrows m == 0 && ncols m == 0 = error "empty matrix"
--   |



--bad
--determinant [] = error "empty matrix"
--determinant [[x]] =

