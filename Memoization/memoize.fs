open System

let str = "manyu manit manka"
let substr = "man"

let search (str: string) (substr: string) : bigint =
    let cache = new System.Collections.Generic.Dictionary<(char list)*(char list), bigint>()
    let rec search' (str: char list) (substr: char list) : bigint =
        if not(cache.ContainsKey(str, substr))
        then 
            cache.[(str, substr)] <- match str, substr with
                                     | [], _ -> 0I
                                     | _, [] -> 1I
                                     | s::stl, sub::subtl -> if s = sub
                                                             then (search' stl substr) + (search' stl subtl)
                                                             else search' stl substr
        cache.[(str, substr)]

    search' (str.ToCharArray() |> Array.toList) (substr.ToCharArray() |> Array.toList)

let ans = search str substr

printf "the number of occurrences = %A\n" (ans)

Console.ReadKey()
