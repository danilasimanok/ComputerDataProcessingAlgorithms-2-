namespace Tasks

open System.Threading

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

    type Queue<'a> =
        Queue of 'a list * 'a list

    type BlockingQueue<'a> () =
        
        let mutable queue : Queue<'a> = Queue ([], [])

        let enqueue element =
            match queue with
            | Queue (l1, l2) -> queue <- Queue (element :: l1, l2)

        let dequeue () =
            let inner () =
                match queue with
                | Queue ([], []) -> None
                | Queue (l1, []) ->
                    let hd :: tl = l1
                    queue <- Queue ([], List.rev tl)
                    Some hd
                | Queue (l1, l2) ->
                    let hd :: tl = l2
                    queue <- Queue (l1, tl)
                    Some hd
            lock queue inner

        member _.Enqueue element =
            lock queue (fun() -> enqueue element)

        member _.Dequeue () =
            let rec inner () =
                match dequeue () with
                | None ->
                    Thread.Sleep 0
                    inner ()
                | Some element -> element
            inner ()