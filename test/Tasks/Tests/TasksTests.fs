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
    sw.ToString() |> should equal "****\n*  *\n*  *\n****\n"

[<TestCase(0)>]
[<TestCase(-1)>]
let testPrintSquareExtreneCase (n) =
    use sw = new StringWriter()
    Console.SetOut(sw)
    printSquare n
    sw.ToString() |> should equal ""

[<Test>]
let testPriorityQueueCommonCase () =
    let pq = new PriorityQueue<string> ()
    pq.Enqueue 2 "the"
    pq.Enqueue 4 "remained"
    pq.Enqueue 6 "The"
    pq.Enqueue 5 "future"
    pq.Enqueue 1 "past."
    pq.Enqueue 3 "in"
    let message = (Seq.ofList << List.init 6) (fun x -> pq.Dequeue ())
    String.concat " " message |> should equal "The future remained in the past."

[<Test>]
let testPriorityQueueExtremeCase () =
    let pq = new PriorityQueue<string> ()
    ignore << pq.Dequeue |> should (throwWithMessage "Queue is empty!") typeof<exn>