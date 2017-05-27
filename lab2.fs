open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "plahtinskiy.ve@phystech.edu"

type JSON = 
    | Object of (string * JSON) list
    | Array of JSON list
    | Number of int
    | String of string
    | Boolean of bool
    | Null
  
type Token =
    | OpenBrace 
    | CloseBrace
    | OpenBracket 
    | CloseBracket
    | Colon
    | Comma
    | Number of int
    | String of string
    | Boolean of bool
    | Null
  
let explode (s:string) = [for i in s -> i]


let tokenize source =
    let rec parseNumber acc = function
        | c :: t when List.exists (fun x -> x = c) [','; '}'; ']' ] -> (acc, c :: t)
        | c :: t -> parseNumber (acc + c.ToString()) t
        | [] -> parseNumber (acc) []    
    let rec parseString acc = function
        | '\\' :: 'n' :: t -> parseString (acc + "\n") t
        | '"' :: t -> (acc, t) //"
        | c :: t -> parseString (acc + c.ToString()) t
        | _ -> failwith "parseString failed"
    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
        | '{' :: t -> tokenize' (OpenBrace :: acc) t
        | '}' :: t -> tokenize' (CloseBrace :: acc) t
        | '[' :: t -> tokenize' (OpenBracket :: acc) t
        | ']' :: t -> tokenize' (CloseBracket :: acc) t
        | ':' :: t -> tokenize' (Colon :: acc) t
        | ',' :: t -> tokenize' (Comma :: acc) t
        | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false:: acc) t
        | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
        | '"' :: t -> //"
            let (s, t') = parseString "" t
            tokenize' (String s :: acc) t'
        | c :: t when Char.IsDigit(c) ->
            let (n, t') = parseNumber (c.ToString()) t
            let num = int n
            tokenize' (Number num :: acc) t'
        | [] -> List.rev acc
        | _ -> failwith "Parse error"
    tokenize' [] source
       
let parseTokens source =
    let rec parse' list =
        let rec parseObject list = function
            | Token.String name::Colon::t ->
                let obj, t' = parse' t
                parseObject ((name, obj) :: list) t'
            | Comma::t -> parseObject list t
            | CloseBrace::t -> JSON.Object list, t
            | n -> failwith "Object panic!"
        let rec parseArray list = function 
            | Comma::t -> parseArray list t
            | CloseBracket::t -> JSON.Array (List.rev list), t
            | t ->
                let obj, t' = parse' t
                parseArray (obj::list) t'
        match list with
            | Token.String s::t -> JSON.String s, t
            | Token.Null :: t -> JSON.Null, t
            | Token.Boolean b::t -> JSON.Boolean b, t
            | Token.Number n::t -> JSON.Number n, t
            | OpenBrace::t -> parseObject [] t
            | OpenBracket::t -> parseArray [] t
            | _ -> failwith "parse panic"
    match parse' source with
        | res, [] -> res
        | _, _ -> failwith "Tokenize error"

let parse str = 
    str |> explode |> tokenize |> parseTokens

// 20 Вариант: Добавить произвольный примитивный элемент в дерево,
// его позиция задаётся списком ключей.
let rec lab3 json keys elem = 
    let rec iter list key acc= 
        match list with
            | (a, x)::t when a = key -> (acc, (a, x), t)
            | x::t -> iter t key (x::acc)
            | [] -> failwith "error"
    match (json, keys) with
        | (JSON.Object(obj), x::[]) -> JSON.Object((x, elem)::obj)
        | (JSON.Object(obj), x::t) -> 
            let (a, (name, b), c) = iter obj x []
            JSON.Object(a@(name, (lab3 b t elem))::c)
        | _ -> failwith "error" 

let rec stringify = function
    | JSON.Object(list) ->
        "{" + (List.map (fun pair -> "\"" + fst pair + "\" : " + stringify (snd pair)) list |> String.concat ", ") + "}"
    | JSON.Array(array) -> 
        "[" + ((List.map stringify array) |> String.concat ", ") + "]"
    | JSON.Number(number) -> string number
    | JSON.String(str) -> str
    | JSON.Boolean(true) -> "true"
    | JSON.Boolean(false) -> "false"
    | JSON.Null -> "null"
        
let rec generate() = 
    let rnd = new Random()
    let rec generator() = 
        match rnd.Next(6) with
            | 0 -> JSON.Null
            | 1 -> JSON.Number(rnd.Next())
            | 2 -> JSON.String(rnd.Next().ToString())
            | 3 -> JSON.Boolean(if (rnd.Next(2) = 0) then true else false)
            | 4 -> 
                let n = rnd.Next(5)
                let rec generate' acc = function
                    | 0 -> acc
                    | n -> generate' (generator()::acc) (n - 1)
                JSON.Array(generate' [] n)
            | _ ->
                let n = rnd.Next(5)
                let rec generate' acc = function
                    | 0 -> acc
                    | n -> generate' ((rnd.Next().ToString(), generator())::acc) (n - 1)
                JSON.Object(generate' [] n)
    generator()
    
let example = "{
  \"rrr\":\"Rty\",
  \"Param\":
  {
    \"r1\":1,
    \"r2\":4
  },
  \"Count1\":10,
  \"Count2\":34,
  \"Number\":
  [
    0,10,20,32,45
  ]
  \"Obj\":
  {
    \"key1\":1,
    \"key2\":2
  },
  \"array\":
  [
    {
      \"k1\":\"a\",
      \"k2\":1,
      \"k3\":43
    },
    {
      \"k4\":\"r\",
      \"k5\":61
    },
    {
      \"k6\":\"rr\",
      \"k7\":68
    }
  ],
}"
    
let test1() = parse example

let test2() = generate() |> stringify

let test3() = 
    let obj = parse example
    lab3 obj ("Param"::"testkey"::[]) (JSON.String("test")) |> stringify
    
let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main()