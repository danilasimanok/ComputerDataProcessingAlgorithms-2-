module Tests

open NUnit.Framework
open Tasks.Tasks
open System.IO
open System
open FsUnit

[<Test>]
let testMinCommonCases () = 
    min [1 .. 5] |> should equal <| Some 1
    min [5; 1; 3; 2; 4] |> should equal <| Some 1

[<Test>]
let testMinExtremeCase () =
    min [] |> should equal None

[<Test>]
let testPrintSquareCommonCase () =
    use sw = new StringWriter()
    Console.SetOut(sw)
    printSquare 4
    sw.ToString() |> should equal "****\r\n*  *\r\n*  *\r\n****\r\n"

[<TestCase(0)>]
[<TestCase(-1)>]
let testPrintSquareExtreneCase (n) =
    use sw = new StringWriter()
    Console.SetOut(sw)
    printSquare n
    sw.ToString() |> should equal ""

(*[<Test>]
let testStack () =
    
    let stack = new Stack()
    let list1 = [1 .. 10]
    let list2 = [11 .. 20]

    let rec push list =
        match list with
        | [] -> ()
        | hd :: tl ->
            stack.Push hd
            push tl
    
    let t1 = System.Threading.Thread (fun () -> push list)
    *)