-- Use this file to experiment with different expressions on which you are 
-- currently working.


let g := given (\r -> value 0 r != "") in 
g [ ["" ""] ["a" "a"] ["b" "b"] ]
