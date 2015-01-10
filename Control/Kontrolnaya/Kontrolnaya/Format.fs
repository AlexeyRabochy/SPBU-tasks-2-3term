module Format
open System.Numerics
open System
open System.Collections.Generic

type LineElement =
| Word of string * int
| Space of int

type Line =
| Line of LineElement list

type FormatStyle =
| Left
| Right
| Center
| Width
// пребразует строчку в слово
let convertToWord (word: string) : LineElement =
    Word(word, word.Length)
// составляет Line пока есть возможность добавлять слова
let rec makeLine (source: LineElement list) (width: int) : (Line * LineElement list) =
    match source with
    | Word(w, length)::tl -> if (length = width) 
                             then (Line([Word(w, length)]), tl)
                             elif (length < width) 
                             then let (Line(a), b) = makeLine tl (width - length - 1)
                                  (Line(Word(w, length)::Space(1)::a), b)
                             else (Line([]), source)

    | [] -> (Line([]), [])
    | _ -> failwith"incorrect source in makeLine"
// список слов преобразует в список Line. 
// Комплектует Line пока может, затем начинает комплековать следующий Line с того слова, где остановился
let rec convertToLine (source: LineElement list) (width: int) : Line list =
    match source with
    | hd :: tl -> let (a, b) = makeLine source width
                  a::(convertToLine b width)
    | [] -> []
// добавляет столько пробелов в конец, что Line занимает в длину места, сколько задано в максимальной ширине
let rec addSpaceInEnd (source: LineElement list) (width: int) : LineElement list =
    match source with
    | Word(w, length)::[] -> if width > length then Word(w, length)::Space(width-length)::[]
                             else Word(w, length)::[]
    | Space(length)::[] -> Space(width)::[]
    | Word(w, length)::tl -> Word(w, length)::(addSpaceInEnd (tl) (width-length))
    | Space(length)::tl -> Space(length)::(addSpaceInEnd tl (width-length))
    | [] -> []
// добавляет указанное количество пробелов в начало
let addSpaceInBegin (source: LineElement list) (numb: int) : LineElement list = 
    Space(numb)::source
// удаляет пробелы в начале
let deleteSpaceInBegin (source: LineElement list) : LineElement list =
    match source with
    | Space(_)::tl -> tl
    | _ -> source
// удаляет пробелы в конце
let deleteSpaceInEnd (source: LineElement list) : LineElement list =
    source |> List.rev |> deleteSpaceInBegin |> List.rev 
// подсчитывает длину Line в символах(длина всех слов и пробелов)
let rec countLength (source: LineElement list) : int =
    match source with
    | Space(len)::tl -> len + (countLength tl)
    | Word(string, len)::tl -> len + (countLength tl)
    | [] -> 0
// подсчитывает количество пробелов в Line
let rec countSpaces (source: LineElement list) : int =
    match source with
    | Space(_)::tl -> 1 + countSpaces tl
    | Word(_, _)::tl -> countSpaces tl
    | [] -> 0
// применяет левое форматирование к одному Line
let primeLeftFormat (source: LineElement list) (width: int) : LineElement list =
    match source with
    | Space(length)::tl -> addSpaceInEnd tl width
    | _ -> addSpaceInEnd source width

// применяет левое форматирование к списку Line
let rec leftFormat (source: Line list) (width: int) : Line list =
    match source with
    | Line(hd)::tl -> Line(primeLeftFormat hd width)::(leftFormat tl width)
    | [] -> []
// применяет правое форматирование к одному Line
let primeRightFormat (source: LineElement list) (width: int) : LineElement list =
    (primeLeftFormat (List.rev source) width) |> List.rev
// применяет правое форматирование к списку Line
let rec rightFormat (source: Line list) (width: int) : Line list =
    match source with
    | Line(hd)::tl -> Line(primeRightFormat hd width)::(rightFormat tl width)
    | [] -> []
// применяет форматирование по центру к одному Line
let rec primeCenterFormat (source: LineElement list) (width: int) : LineElement list =
    let source = source |> deleteSpaceInBegin |> deleteSpaceInEnd 
    let size = source |> countLength
    let numbInBegin = (width-size)/2
    addSpaceInEnd (addSpaceInBegin source numbInBegin) width
// применяет форматирование по центру к списку Line
let rec centerFormat (source: Line list) (width: int) : Line list =
    match source with
    | Line(hd)::tl -> Line(primeCenterFormat hd width)::(centerFormat tl width)
    | [] -> []
// вставляет в каждый пробел в Line указанное количество
let rec insertInAllSpaces (source: LineElement list) (numb: int) =
    match source with
    | Word(s, l)::tl -> Word(s, l)::(insertInAllSpaces tl numb)
    | Space(len)::tl -> Space(len + numb)::(insertInAllSpaces tl numb)
    | [] -> []
// Вставляет в указанное количество пробелов в начале по 1 пробелу
let rec insertInSomeSpaces (source: LineElement list) (numb: int) =
    match source with
    | Word(s, l)::tl -> Word(s, l)::(insertInSomeSpaces tl numb)
    | Space(len)::tl -> if (numb > 0)
                        then Space(len + 1)::(insertInSomeSpaces tl (numb - 1))
                        else source
    | [] -> []
// применяет форматирование по ширине
let rec primeWidthFormat (source: LineElement list) (width: int) : LineElement list =
    let source = source |> deleteSpaceInBegin |> deleteSpaceInEnd
    // n - количество мест куда доставлять пробелы
    let n = source |> countSpaces
    // p - сколько пробелов нужно вставить
    let p = width - (source |> countLength)
    // если не нулевые n и p, то мы вставляем в каждый пробел по p / n и в первые p % n по 1 пробелу
    if (n = 0)&&(p > 0) then primeCenterFormat source width
    elif (n = 0)&&(p = 0) then source
    else insertInSomeSpaces (insertInAllSpaces source (p / n)) (p % n)
// форматирование по ширине
let rec widthFormat (source: Line list) (width: int) : Line list =
    match source with
    | Line(hd)::tl -> Line(primeWidthFormat hd width)::(widthFormat tl width)
    | [] -> []
// форматирование
let format (source: Line list) (width: int) (form: FormatStyle) : Line list =
    match form with
    | Left -> leftFormat source width 
    | Right -> rightFormat source width
    | Center -> centerFormat source width
    | Width -> widthFormat source width
// выводит нужное кол-во пробелов
let rec printSpace (len: int) : unit =
    if len > 0 then printf "%s" " "
                    printSpace (len - 1)
    else ()
// функция вывода линий
let rec printLine (output: Line) : unit =
    match output with
    | Line(Word(w,len)::tl) -> printf "%s" w
                               printLine (Line(tl))
    | Line(Space(len)::tl) -> printSpace len
                              printLine (Line(tl))
    | Line([]) -> printf "\n"
// функция вывода списка линий    
let rec printLineList (output: Line list) : unit =
    match output with
    | hd::tl -> printLine hd
                printLineList tl
    | [] -> ()

let input = ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]

let test = List.map convertToWord input

let ans = convertToLine test 11
let ans2 = format ans 11 Width


printLineList ans2
Console.ReadKey()