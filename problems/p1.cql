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
