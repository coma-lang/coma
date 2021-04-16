-- PROBLEM 2


let A := read "test/data/numbers.csv" in
select [2 0] (given (\row -> get [0] row = get [1] row) A)
