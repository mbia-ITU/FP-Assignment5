//Exercise 5.1
let sum m n = 
    let rec aux acc n = 
        match n with 
        | 0 -> acc + m 
        | _ -> aux (acc + (m + n)) (n-1) 
    aux 0 n

//Exercise 5.2
let length lst =
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | x::xs -> aux xs (acc+1)
    aux lst 0


//Exercise 5.3
let foldBack f lst acc =
    let rec aux lst c =
        match lst with
        | [] -> c acc
        | x::xs -> aux xs (fun r -> c (f x r))
    aux lst id


//Exercise 5.4
let factC x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

//Exercise 5.5
