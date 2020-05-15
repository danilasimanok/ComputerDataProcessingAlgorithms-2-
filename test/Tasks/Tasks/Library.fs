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
            printf "\n"
        let printInteriorOuter () =
            printRecursive printBorderInner printInteriorInner n
            printf "\n"

        if n <= 0
        then ()
        else printRecursive printBorderOuter printInteriorOuter n

    type PriorityQueue<'a> () =
        
        let mutable queue : (int * 'a) list = []

        member _.Enqueue priority element =
            let rec inner queue acc =
                match queue with
                | [] -> List.rev ((priority, element) :: acc)
                | (hdPriority, hdElement) :: tl ->
                    if priority > hdPriority
                    then List.rev ((priority, element) :: acc) @ queue
                    else inner tl ((hdPriority, hdElement) :: acc)
            queue <- inner queue []

        member _.Dequeue () =
            match queue with
            | [] -> failwith "Queue is empty!"
            | (_, hdElement) :: tl ->
                queue <- tl
                hdElement