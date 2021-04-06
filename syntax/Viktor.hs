-- Core.read    : String -> Table
-- Core.get     : List Int -> Row -> Row
-- Core.merge   : Row -> Row -> Row
-- Core.value   : Int -> Row -> String
-- Core.forEach : List a -> (a -> b) -> List b
-- Core.join    : Table -> Table -> Table
-- Core.empty   : String -> Bool
-- Core.not     : Bool -> Bool
-- Core.select  : List Int -> Table -> Table
-- Core.given   : (Row -> Bool) -> Table -> Table



-- PROBLEM 1


let A := read "A.csv" in
let B := read "B.csv" in
join A B



-- PROBLEM 2


let A := read "A.csv" in
select [2 0] (given (row -> get [0] row = get [1] row) A)



-- PROBLEM 3


let P := read "P.csv" in
let Q := read "Q.csv" in


let filtered := given (row -> get [0] row = get [4] row) (join P Q) in


-- forEachRow : Row -> Maybe Row
let forEachRow := 
  -- row : p1 p2 p3 p4  q1 q2 q3 q4
  -- ids : 0  1  2  3   4  5  6  7
  row ->
    let ps := get [1 2 3] row in
    let qs := get [5 6 7] row in
    get [0] row ++ merge ps qs
in


forEach forEachRow filtered



-- PROBLEM 4


let A := read "A.csv" in
given (row -> not (empty (value 1 row))) A