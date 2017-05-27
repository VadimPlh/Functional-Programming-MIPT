open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "plahtinskiy.ve@phystech.edu"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor x : float = System.Math.Exp(2. * x) // функция, которую раскладываем
let n, a, b = 20., 0.1, 0.6 // интервал

let rec factorial = function
    | 0 -> 1
    | n -> n * factorial(n - 1)
    
let rec power x = function 
    | 0 -> 1.0
    | n -> x * power x (n - 1)

let tailor x : Result = 
    let rec tailor1 x n ans =
        let element x n = (power (2. * x) n) / float(factorial n)
        let tmp = element x n
        if (tmp < delta) then (ans, n)
                         else tailor1 x (n + 1) (ans + tmp)
    tailor1 x 0 0.0

let tailorA x : Result = 
    let rec tailor1 x n ans prev =
        let next = prev * (2. * x) / float(n - 1)
        if (next < delta) then (ans, n)
                          else tailor1 x (n + 1) (ans + next) next
    tailor1 x 2 1.0 1.0

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )
    
// *** Вторая часть 

let fSolve1, a1, b1 = (fun x -> 0.1 * x * x - x * System.Math.Log(x)), 1., 2. // функция, решение которой ищем
let fSolve2, a2, b2 = (fun x -> 3. * x - 4. * System.Math.Log(x) - 5.), 2., 4.
let fSolve3, a3, b3 = (fun x -> System.Math.Acos(x) - System.Math.Sqrt(1. - 0.3 * x * x * x)), 0., 1.

let derivative f x = (f(x + delta) - f(x)) / delta

let iter f a b : Result = 
    let start = (a + b) / 2.0
    let step x = (x - (f x) / (derivative f start))
    let rec iter1 n point =
        let newx = step point
        if (abs (point - newx) < delta) then (point, n)
                                        else iter1 (n + 1) newx
    iter1 0 start

let newton f a b : Result = 
    let start = (a + b) / 2.0
    let rec newton1 n pred = 
        let newx = pred - (f pred) / (derivative f pred)
        if (abs (newx - pred) < delta) then (pred, n)
                                       else newton1 (n + 1) newx
    newton1 0 start
    
let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result = 
        let med = (a + b) / 2.
        let funcMed = f med
        if (abs funcMed < delta) then (med, i + 1)
                                 else
                                     let next = dichotomyA (i + 1) f 
                                     if f a * funcMed < 0. then next a med
                                                           else next med b
    dichotomyA 0 // чтобы воспользоваться каррированием

let printSolve fSolve a b  =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve a b) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

let printSolves() =
    printSolve fSolve1 a1 b1
    printSolve fSolve2 a2 b2
    printSolve fSolve3 a3 b3

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseStrin