-- Problem 1


let A := open "A.csv" in
    let B := open "B.csv" in
        join A B



-- Problem 2


let A := read "A.csv" in
    forEach row : A
        if (get row 2 == get row 1) 
            then Just (get row 3, get row 1) 
            else Nothing



-- Problem 3


let P := read "P.csv" in 
let Q := read "Q.csv" in  
    forEach rp : p
        forEach rq : q 
            if (get rp 1 == get rq 1) 
                then Just (get rp 1, safeGet rq rp 2, safeGet rq rp 3, safeGet rq rp 4)
-- safeGet returns the value from p if the q value is empty



-- Problem 4


let A := read "A.csv" in
    forEach row : A
        if (get row r2 == "") then Nothing else Just (get row 1, get row 2)



-- Problem 5


let A := read "A.csv" in
    forEach row : A
        Just (get row 1, "0", get row 2)
