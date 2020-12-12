module Utils

let xor first second = 
    match first, second with
    | true, true | false, false -> false
    | true, false| false, true  -> true