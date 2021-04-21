-- PROBLEM 5


let A := read "test/data/p5-1-A.csv" in


let forEachRow := 
  \row ->
    let a0 := get [0] row in
    a0 ++ ["0"] ++ a0
in


forEach forEachRow A
