open System
open System.IO

// Задание 1

let packStr = 
    let rec pair' acc lastSym count =
        function
        | x::xs when x = lastSym -> pair' acc lastSym (count + 1) xs
        | x::xs -> pair' ((lastSym, count)::acc) x 1 xs
        | [] -> (lastSym, count)::acc
    function 
    | x::xs -> List.rev (pair' [] x 1 xs)
    | [] -> []

let array = "aabbabs"

let explode (s:string) = [for i in s -> i] 

array |> explode |> packStr





//  Задание 2

let rec unpackStr = 
    let rec getArray acc =
        function 
        | (a, b) when b = 0 -> acc
        | (a, b) -> getArray (a::acc) (a, (b-1))
    function 
    | [] -> []
    | x::xs -> (getArray [] x)@(unpackStr xs)

let test = [('a', 2); ('b', 2); ('a', 1); ('b', 1); ('s', 1)]

let implode (list : char list) = 
    List.fold(fun acc c -> acc + (string c)) "" list

test |> unpackStr |> implode






// Задание 3

let readFile file =
    let lines = File.ReadLines file
    let seq1  = lines |> Seq.collect(fun x -> x.Split(';', '!', '?', '-', ' ', ':', '.') )
    seq1 |> Seq.filter(fun x -> x.Length > 0)

readFile "/Users/VadimPl/Desktop/шарп/lab2.fs"






// Задание 4

let createDict str = 
    let seqWord = readFile str
    let tmp = seqWord |> Seq.groupBy(fun x -> x)
    let dict = tmp |> Seq.map(fun (x, y) -> x, (Seq.length y)) |> Seq.toList
    dict

createDict "/Users/VadimPl/Desktop/шарп/lab2.fs"

type Word = 
    | Cnt of int
    | Word_ of string

let getIndex dict word =
    let rec numWord' acc i =
        match acc with
        | (w, x)::xs when w = word -> i
        | x::xs -> numWord' xs (i + 1) 
        | [] -> -1
    numWord' dict 0

let tmp = getIndex (createDict "/Users/VadimPl/Desktop/шарп/lab2.fs") "OpenBrace"
