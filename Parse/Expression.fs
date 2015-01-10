open System

type Expr = 
    | Const of int
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let rec calc expr =
    let simple expr =
        match expr with
        |Add(Const 0, a)|Add(a, Const 0) ->a
        |Add(Const a, Const b)|Add(Const b, Const a) -> Const(a+b)
        |Sub(b, Const 0) -> b
        |Sub(Const a, Const b) -> Const(a-b)
        |Mul(a, Const 0)|Mul(Const 0, a) -> Const 0
        |Mul(Const a, Const b) -> Const(a*b)
        |Div(Const 0, a) -> Const 0
        |Div(a, Const 0) -> failwith"div by 0"
        |Div(Const a, Const b) -> Const(a/b)
        |Div(a, Const 1) -> a
        |_ -> expr

    match expr with
    |Add(a, b) -> simple(Add(calc a, calc b))
    |Sub(a, b) -> simple(Sub(calc a, calc b))
    |Mul(a, b) -> simple(Mul(calc a, calc b))
    |Div(a, b) -> simple(Div(calc a, calc b))
    |Const a -> expr
    |Var a -> expr


let y = ( Add ((Add(Const 1, Const 2)), (Add (Const 2, Const 0))))
let x = (Add( (Add (Const 1, Const 2)), (Add (Var "a", Const 0))))

let y' = calc y
let x' = calc x







    