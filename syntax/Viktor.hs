-- Problem 3

-- Core.read    : String -> Table
-- Core.get     : List Int -> Row -> Row
-- Core.merge   : Row -> Row -> Row
-- Core.value   : Int -> Row -> String
-- Core.forEach : List a -> (a -> b) -> List b
-- Core.join    : Table -> Table -> Table


let P := read "P.csv" in
let Q := read "Q.csv" in


-- forEachRow : Row -> Maybe Row
let forEachRow := 
  -- row : p1 p2 p3 p4  q1 q2 q3 q4
  -- ids : 0  1  2  3   4  5  6  7
  row ->
    let ps := get [1, 2, 3] row in
    let qs := get [5, 6, 7] row in
    let rest := merge ps qs in
    if (value 0 row == value 4 row)
      (get [0] row ++ rest) -- then
      None                  -- else
in


forEach forEachRow (join P Q)