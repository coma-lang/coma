import Data.List

type Row = [String]
type Table = [Row]


--sort . (
--let A := (open A.csv) in
--forEach row in A
--if (get row 0 == get row 1) then write (get row 2, get row 0)
-- )

--var A := open A.csv
--var B := new Table
--forEach row in A
--if (get row 0 == get row 1) then addTo B (get row 2, get row 0)
--sort B
--writeToFile B
 
--Problem 2
p2 :: Table -> Table -> Table
p2 [] t = sort t   --Sorted lexicographically
p2 (r:rs) t | row == [] = p2 rs t
            | otherwise = p2 rs (t ++ row)
        where row = [p2' r]

p2' :: Row -> Row 
p2' (r1:r2:r3:[]) | r1 == r2 = [r3, r1]
                  | otherwise = []

--Problem 3
p3 :: Table -> Table -> Table -> Table
p3 [] _ t = sort t  --Sorted lexicographically
p3 (ps:pss) qss t | table == [] = p3 pss qss t
                  | otherwise = p3 pss qss (t ++ table) 
            where table = filter (\xs -> xs /= []) (p3' ps qss)

p3' :: Row -> Table -> Table
p3' _ [] = []
p3' p@(p1:ps) (qs:qss) | p1 == q1 = ([p1] ++ lMerge ps (tail qs)) : p3' p qss
                       | otherwise = p3' p qss
        where q1 = head qs

lMerge :: Row -> Row -> Row
lMerge [] [] = []
lMerge (p:ps) (q:qs) | p == "" = q : lMerge ps qs
                     | otherwise = p : lMerge ps qs



test1 = p3 [["1","5","4",""],["2","","2",""],["3","7","1","2"],["4","8","",""]] [["1","6","4","7"],["2","8","5","3"],["2","","","1"],["4","","2","3"]] []