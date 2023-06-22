
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n p1 p2 p3
    | n < 0 || p1 == p2 || p2 == p3 || p3 == p1 = []
    | n == 1                                    = [(p1, p2)]
    | otherwise = (hanoi (n - 1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n - 1) p3 p2 p1) 


main :: IO ()
main =  do
    putStrLn "Hello, Ex05"
    putStrLn "Moving 2 disks"
    print $ hanoi 2 "a" "b" "c"
    putStrLn "Moving 3 disks"
    print $ hanoi 3 "a" "b" "c"
    
