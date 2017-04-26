import qualified Data.Matrix as M
import Data.List (concat)

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



lst = [[1, 5, 3], [4, 5, 6], [7, 8, 9]]

{-|
lst = [
   ['a', 'b', 'c', 'd'],
   ['e', 'f', 'g', 'h'],
   ['i', 'j', 'k', 'l'],
   ['m', 'n', 'o', 'p']]
-}

m = M.fromLists lst

{-|

accessing:
m M.! (1, 2) = 2
m M.! (2, 3) = 6

M.getElem 2 3 m

-}


type MinorPair a = (a, M.Matrix a)

--pair of minor matrices and coefficients
getMinorPair :: M.Matrix a -> [MinorPair a]
getMinorPair m = map (\index -> getMinorPair' lstM index) [0..M.ncols m-1]
   where lstM = M.toLists m

getMinorPair' :: [[a]] -> Int -> (a, M.Matrix a)
getMinorPair' (coeffs:rest) n = (coeffs !! n, M.fromLists $ excludeColumn' n rest)


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

determinant :: (Num a) => M.Matrix a -> a
determinant m
   | M.nrows m == 0 && M.ncols m == 0 = error "empty matrix"
   | M.nrows m == 2 && M.ncols m == 2 = (M.getElem 1 1 m) * (M.getElem 2 2 m) - (M.getElem 1 2 m) * (M.getElem 2 1 m)
   | otherwise = determinant' m

determinant' :: (Num a) => M.Matrix a -> a
determinant' m =
   let minorPairs = getMinorPair m
       alternatingSigns = concat $ repeat ['m', 'p']
   in
      foldr
         (\ (sign, (coeff, matrix)) acc -> ((if sign == 'p' then (+) else (-)) acc (coeff * determinant matrix)))
         0
         (zip alternatingSigns minorPairs)




--bad
--determinant [] = error "empty matrix"
--determinant [[x]] =

displayMinorPairs matrix = do
   let xs = getMinorPair matrix
   mapM_ putStrLn $ map (\(i, subm) -> ((show i) ++ " \n| \n" ++ (M.prettyMatrix subm) ++ "|\n")) xs

main = displayMinorPairs m


