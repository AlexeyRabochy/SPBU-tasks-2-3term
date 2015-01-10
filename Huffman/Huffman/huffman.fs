module Huffman

type CodeTree = 
  | Fork of CodeTree * CodeTree * char list * int
  | Leaf of char * int

//преобразуем строку в список символов
let stringzchars str = Seq.toList str

//считает количество символов
let rec count (chars: char list) (a: char) : int =
    match chars with
    | hd::tl -> if hd = a then 1 + (count tl a)
                else count tl a
    | [] -> 0

//удалит в списке заданный символ(который уже встретился)
let rec modify (chars: char list) (a: char) : char list =
    match chars with
    |hd::tl -> if hd = a then modify tl a
               else hd::(modify tl a)
    | [] -> []

//возвращает список символ и частоту его повторений
let rec times (chars: char list) : (char*int) list =
    match chars with
    | hd::tl -> (hd, (count chars hd))::(times (modify chars hd))
    | [] -> []

let weight (tree: CodeTree) : int =
    match tree with
    | Leaf(ch, n) -> n
    | Fork(_,_,_,n) -> n

let chars (tree: CodeTree) : char list =
    match tree with
    | Leaf(ch, n) -> ch::[]
    | Fork(_,_,ch,_) -> ch

//проверяет список 1 элемент - true, 
let singleton (treeList: CodeTree list) : bool =
    match treeList with
    | hd::[] -> true
    | _ -> false

let makeCodeTree (left: CodeTree) (right: CodeTree) : CodeTree =
    Fork(left, right, (chars left)@(chars right), (weight left)+(weight right))

let rec insert (tree: CodeTree) (treeList: CodeTree list) =
    match treeList with
    |hd::tl -> if (weight tree <= weight hd)
                then tree::treeList
                else hd::(insert tree tl)
    |[] -> tree::[]

let makeLeaf (charWithFreq: (char*int)) : CodeTree =
    Leaf(charWithFreq)

//получает список вершин отсорт. по возраст.
let makeOrderedLeafList (charListWithFreq: (char*int) list) : CodeTree list =
    let treeList = List.map makeLeaf charListWithFreq //делает лист из каждого элемента
    let rec sortList (treeList: CodeTree list) (newTreeList: CodeTree list) = //соритруем символы по частоте
        match treeList with
        |hd::tl -> sortList tl (insert hd newTreeList)
        |[] -> newTreeList
    sortList (treeList) ([])

// берет 2 первых элемента и формирует узел
let combine (treeList: CodeTree list) : CodeTree list =
    let combine' (treeList: CodeTree list) : CodeTree list =
        match treeList with
        | hd1::hd2::tl -> (makeCodeTree hd1 hd2)::tl
        | _ -> failwith "Impossible, verify singleton"

    match (combine' treeList) with
    | hd::tl -> insert hd tl
    | _ -> failwith "Impossible, verify singleton"

    // по сути цикл while
let rec until f k treeList = 
    if (f treeList) then treeList
    else until f k (k treeList)

// code tree   
let createCodeTree (chars: string) : CodeTree = 
    match (until singleton combine (makeOrderedLeafList <| (times <| (stringzchars <| chars)))) with 
    | hd::tl -> hd
    | [] -> failwith "Empty Tree"

// decode

type Bit = int


//let tree = Fork ( Fork (Leaf ('a', 3), Leaf('b', 2), ['a'; 'b'], 5), Fork (Leaf ('c', 2), Leaf('d', 1), ['c'; 'd'], 3) , ['a'; 'b'; 'c'; 'd'], 8)

let decode (tree: CodeTree) (bits: Bit list) : char list =
    let rec decode' (tree: CodeTree) (helpTree: CodeTree) (bits: Bit list) : char list = 
        match bits with 
        | hd::tl -> match helpTree with
                    | Leaf(a,_) -> a::decode' tree tree bits
                    | Fork(trl, trr, _, _) -> if hd = 0 then decode' tree trl tl
                                                        else decode' tree trr tl
        | [] -> match helpTree with
                | Leaf(a, _) -> a::[]
                | _ -> []

    decode' tree tree bits

                    
    

// encode
let rec encode (tree: CodeTree)  (text: char list) : Bit list = 
    let rec encodeChar (tree: CodeTree) (c: char) = 
        match tree with
        | Fork(Leaf(a,_), Leaf(b,_), list, _) -> if a = c then [0]
                                                          else [1]
        | Fork(Leaf(a, _), tr, _, _) -> if a = c then [0]
                                                 else 1::encodeChar tr c 
        | Fork(tr, Leaf(b, _), _, _) -> if b = c then [1]
                                                 else 0::encodeChar tr c
        | Fork(Fork(trl, trr, leftList, n), Fork(trl2, trr2, rightList, n2), _, _) -> if List.exists (fun x -> x = c) leftList then 0::encodeChar (Fork(trl, trr, leftList, n)) c
                                                                                                                               else 1::encodeChar (Fork(trl2, trr2, rightList, n2)) c
        | Leaf(_, _) -> failwith "Impossible"

    match text with
    | [] -> []
    | hd::tl -> (encodeChar tree hd )@(encode tree tl)

//decode tree (encode tree ['a';'a';'b';'c';'a';'b';'c';'d'])



let str = "abacdcba"
//let strlist = stringzchars str
//times strlist
//List.map makeLeaf (times strlist)
//makeOrderedLeafList (times strlist)
let ans = createCodeTree str
let ans2 = decode ans (encode ans (("abacdcba") |> stringzchars))
//3a 2b 2c 1d
//let ans2 = encode ans (("abacdcba") |> stringzchars)
