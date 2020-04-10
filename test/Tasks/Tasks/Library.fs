namespace Tasks

module Tasks =
    
    let min list =
        match list with
        | [] -> None
        | hd :: tl -> Some <| List.fold (fun i min -> if i < min then i else min) hd tl

    let printSquare n =

        let rec printRecursive printBorder printInterior i =
            if i = 1
            then printBorder ()
            elif i = n
            then
                printBorder ()
                printRecursive printBorder printInterior (i - 1)
            else
                printInterior ()
                printRecursive printBorder printInterior (i - 1)

        let printBorderInner () = printf "*"
        let printInteriorInner () = printf " "
        let printBorderOuter () =
            printRecursive printBorderInner printBorderInner n
            printfn ""
        let printInteriorOuter () =
            printRecursive printBorderInner printInteriorInner n
            printfn ""

        if n <= 0
        then ()
        else printRecursive printBorderOuter printInteriorOuter n

    type Stack () =
        
        let mutable list = []

        let pop () =
            match list with
            | [] -> None
            | hd :: tl ->
                list <- tl
                Some hd
        
        member this.Push x =
            lock this (fun () -> list <- x :: list)

        member this.TryPop () =
            lock this pop