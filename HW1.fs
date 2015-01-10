open System
open System.IO
 
//квадратное уравнение

let solve a b c =
    let dis = (b * b) - (4.0 * a * c)
    if dis < 0.0 then
        failwith "no roots"
    else
        (-b - sqrt dis) / (2.0 * a), (-b + sqrt dis) / (2.0 * a) 
        
solve 1.0 2.0 3.0

//добавление в конец
let rec append l x =
    match l with
    | [] -> [x]
    | h :: t -> h :: (append t x)

append [1;2;3] 9

//развернуть список
let rec reverse l =
    match l with
    | [] -> []
    | h :: t -> reverse t @ [h]

reverse [1 .. 1000000]

//развернуть список(хвостовая рекурсия)

let rev l =
    let rec rev' l acc = 
        match l with
        | [] -> acc
        | h::t -> rev' t (h::acc)
    rev' l []

rev [1 .. 1000000]


//сумма элементов
let rec sum l =
    match l with
    | [] -> 0
    | h :: t -> sum t + h
 
sum [1 .. 10000000]

//сумма элементов(хвостовая)
let sum' l =
    let rec sum'' l acc =
        match l with
        |[] -> acc
        |h::t -> sum'' t (acc+h)
    sum'' l 0I

sum' [1I .. 10000000I]

//количество элементов(длина)
let rec lenght l =
    match l with
    | [] -> 0
    | h :: t -> lenght t + 1
 
lenght [1; 2; 3]



//отфильтровать список переданной функцией
let rec filter f l =
    match l with
    | [] -> []
    | h :: t ->
        if f h then
            h :: (filter f t)
        else
            filter f t

filter (fun x -> x > 0) [2; 4; -1; -3]

//фильтр с использованием переданной функции
let rec map f l =
    match l with
    | [] -> []
    | h :: t -> f h :: map f t
 
map (fun x -> bigint.Pow(2323123123123I, x)  )  [2; 3; 4]














