module LibraryTests

open NUnit.Framework
open FsUnit

open Required.Tasks

[<Test>]
let testFactorialCommonCases () =
    factorial 0 |> should equal <| Some 1
    factorial 1 |> should equal <| Some 1
    factorial 8 |> should equal <| Some 40320

[<Test>]
let ``factorial is not defined for negative integers``() =
    factorial -1 |> should equal None
    factorial -16 |> should equal None

[<Test>]
let queryFirstAndSecondNumbers () =
    (fibonacci 0, fibonacci 1) |> should equal (Some 0, Some 1)

[<TestCase(2)>]
[<TestCase(10)>]
[<TestCase(23)>]
let testSecondPartOfFibonachiDefinition (n) =
    let fibn, fibn1, fibn2 =
        match fibonacci n, fibonacci (n - 1), fibonacci (n - 2) with
        | Some a, Some b, Some c -> a, b, c
        | _, _, _ -> -1, -1, -1
    fibn |> should equal <| fibn1 + fibn2

[<Test>]
let ``fibonachi numbers could have only positive index`` () =
    fibonacci -1 |> should equal None
    fibonacci -9 |> should equal None

[<Test>]
let testReverseCommonCase () =
    reverse [1 .. 100] |> should equal <| List.rev [1 .. 100]

[<Test>]
let testReverseExtremeCase () =
    reverse [] |> should equal <| List.rev []

[<Test>]
let testPowersCommonCases () =
    powersOf2 0 0 |> should equal <| Some [1]
    powersOf2 4 0 |> should equal <| Some [16]
    powersOf2 1 5 |> should equal <| Some [2; 4; 8; 16; 32; 64]

[<TestCase(-1, 0)>]
[<TestCase(0, -1)>]
[<TestCase(-1, -1)>]
let testPowersWithInvalidArgs (n, m) =
    powersOf2 n m |> should equal None

[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(5)>]
let testPositionCommonCases(el) =
    position [1 .. 5] el |> should equal <| Some (el - 1)

[<TestCase(100)>]
[<TestCase(20)>]
let testPositionExtremeCases(el) =
    position [1 .. 5] el |> should equal None