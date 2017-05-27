// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "plahtinskiy.ve@phystech.edu"

let site = "https://lorwiki.ru/wiki/%D0%A7%D0%B0%D1%81%D1%82%D1%8C_1._%D0%9E%D0%B1%D1%89%D0%B8%D0%B5_%D0%B2%D0%BE%D0%BF%D1%80%D0%BE%D1%81%D1%8B_%D0%BE_Lisp"

let getAllLinks (link:string) = async {
    let! html = HtmlDocument.AsyncLoad(link)
    let tmp = html.CssSelect("div#mw-content-text")
    return tmp
          |> Seq.collect(fun x -> x.Descendants ["a"])
          |> Seq.map(fun x -> x.AttributeValue("href"))
          |> Seq.filter(fun x -> not (x.Chars(0).Equals('#')))
          |> Seq.toList
    }

type htmlpages =
    | Page of bool
    | None

let getRusPage (link:string) =
    let alph = "ЁЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
    try
        let page =(HtmlDocument.Load(link).Descendants ["p"]
            |> Seq.map(fun elem -> elem.InnerText())
            |> Seq.map(fun elem -> String.map(System.Char.ToUpper) elem)
            |> Seq.filter(fun elem -> String.exists(fun c -> String.exists(fun x -> x = c) alph) elem))
        if (Seq.length page) > 0
            then htmlpages.Page(true)
        else htmlpages.Page(false)
    with
       :? System.Net.WebException -> htmlpages.None

let func1 x = match x with
    | htmlpages.Page(a) -> true
    | htmlpages.None -> false

let func2 x = match x with
    | htmlpages.Page(a) -> a
    | htmlpages.None -> false 

let lab3 () =
    let urls = [site] |> List.map getAllLinks |> Async.Parallel |> Async.RunSynchronously |> Array.toList |> List.concat
    let validUrls = urls |> List.map(fun x -> getRusPage x) |> List.filter(func1)
    let fullLength = List.length validUrls
    let rusSites = validUrls |> List.filter(func2) |> List.length
    let engSites = fullLength - rusSites
    (rusSites, engSites)

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main()