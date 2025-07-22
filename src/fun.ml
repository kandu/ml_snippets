include Stdlib.Fun

let compose (f1:'b->'c) (f2:'a->'b) n= f1 (f2 n)
let ( * ): ('b->'c) -> ('a->'b) -> 'a -> 'c = compose

