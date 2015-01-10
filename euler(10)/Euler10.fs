open System.Numerics 
open System 

let prime num =
    if num > 3I && (num % 2I = 0I || num % 3I = 0I) then
        false
    else
        let edge = BigInteger(sqrt(float num))
        let rec calc num' i =
            if num' > edge then
                true
            else
                if num % num' = 0I then 
                    false
                else 
                    calc(num' + i) i
        calc 5I 2I

let res = [2I..2000000I] |> List.filter(fun x -> prime x) |> List.sum
printf "%A" (res)
System.Console.ReadKey()


 