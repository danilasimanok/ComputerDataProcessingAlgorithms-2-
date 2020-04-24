module Tests

open NUnit.Framework
open Tasks.Tasks
open System.IO
open System
open FsUnit
open System.Threading

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
    sw.ToString() |> should equal "****\n*  *\n*  *\n****\n"

[<TestCase(0)>]
[<TestCase(-1)>]
let testPrintSquareExtreneCase (n) =
    use sw = new StringWriter()
    Console.SetOut(sw)
    printSquare n
    sw.ToString() |> should equal ""

[<Test>]
let testBlockingQueue () =
    let bq = new BlockingQueue<int>()
    let destination () =
        bq.Dequeue() |> ignore
    let t1 = new Thread(destination)
    t1.Start()
    Thread.Sleep 1000
    t1.IsAlive |> should equal true
    bq.Enqueue 10
    Thread.Sleep 1000
    t1.IsAlive |> should equal false