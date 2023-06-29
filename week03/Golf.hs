module Golf where

import Data.List

-- skips :: [a] -> [[a]]
-- skips l = take (length l) (repeat l)

-- st1 l = zip [1..] (take (length l) (repeat l))
-- 
-- st2 l = map (\el -> ((fst el), (zip [1..] (snd el)))) (zip [1..] (take (length l) (repeat l)))
-- 
-- st3 l = map (\el -> filter (\x -> ((fst x) `mod` (fst el)) == 0) (zip [1..] (snd el))) (zip [1..] (take (length l) (repeat l)))
-- 
-- st4 l = map (\el -> map (\e -> snd e) (filter (\x -> ((fst x) `mod` (fst el)) == 0) (zip [1..] (snd el)))) (zip [1..] (take (length l) (repeat l)))
-- 
-- st5 l = map (\el -> map snd (filter (\x -> ((fst x) `mod` (fst el)) == 0) (zip [1..] (snd el)))) (zip [1..] (take (length l) (repeat l)))
-- 
-- st6 l = takeWhile (/= []) $ map (\el -> map snd $ filter (\x -> ((fst x) `mod` (fst el)) == 0) $ zip [1..] $ snd el) $ zip [1..] $ repeat l
-- 
-- st7 l = takeWhile (/= []) $ map (\(n, lst) -> map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [1..] $ lst) $ zip [1..] $ repeat l
-- 
-- skips l = st7 l

skips l = takeWhile (/= []) $ map (\(n, lst) -> map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [1..] $ lst) $ zip [1..] $ repeat l

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | a <= b && b > c = b : localMaxima (c:xs)
    | otherwise       = localMaxima (b:c:xs)
localMaxima _         = []


-- localMaxima2 :: [Integer] -> [Integer]
-- localMaxima2 xs = f xs
--     where


testList = [1,2,4,3,3,7,9,6,3]
testList2 = [1,4,5,4,6,6,3,4,2,4,9]

histoList l = map tail $ groupBy (==) $ sort (l ++ [0..9])
maxLen l = maximum $ map length l
toStars hl l = (replicate ((maxLen hl) - (length l)) ' ') ++ (replicate (length l) '*')
histoMatrix l = transpose $ map (toStars hl) hl
                where hl = histoList l
histoLines l = (histoMatrix l) ++ ["==========", "0123456789"]

histogram :: [Integer] -> String
histogram = unlines . histoLines

