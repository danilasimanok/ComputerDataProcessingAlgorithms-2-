[<EntryPoint>]
    
    let main _ =
        let factorial n =
            let rec inner n i acc =
                if n <= i
                then acc * i
                else inner n (i + 1) (i * acc)
            if n >= 0
            then Some(inner n 1 1)
            else None

        
        printfn "0! = %O\n(-1)! = %O\n10! = %O\n-----" (factorial 0) (factorial -1) (factorial 10)

        let fib n =
            let rec inner n0 n1 i n =
                if n = i
                then n1
                else inner n1 (n0 + n1) (i + 1) n
            if n = 0
            then Some(0)
            else 
                if n < 0
                then None
                else Some(inner 0 1 1 n)

        printfn "fib_-2 : %O\nfib_0 = %O\nfib_24 = %O\nfib_23 = %O\n-----" (fib -2) (fib 0) (fib 24) (fib 23)

        let reverse list =
            let rec inner list acc =
                match list with
                    | [] -> acc
                    | hd :: tl -> inner tl (hd :: acc)
            inner list []

        let list = [1 .. 100]
        printfn "%A\nreversed is \n%A\n-----" list (reverse list)

        let powers_of_2 n m =
            let rec inner n sum acc i current =
                if i = sum
                then current :: acc
                else inner n sum (if i >=n then (current :: acc) else acc) (i + 1) (2 * current)
            if n < 0 || m < 0
            then None
            else Some(reverse (inner n (n + m) [] 0 1))

        printfn "powers of 2 n = m = 5 %O\npowers of 2 n = 0 m = 5 %O\npowers of 2 n = -1 m = 5 %O\n-----" (powers_of_2 5 5) (powers_of_2 0 5) (powers_of_2 -1 5)

        let position list element =
            let rec inner list position element =
                match list with
                    | [] -> None
                    | hd :: tl ->
                        if hd = element
                        then Some(position)
                        else inner tl (position + 1) element
            inner list 0 element

        printfn "position of 3 in var list is %O\nposition of 90 in var list is %O\nposition of 900 in var list is %O" (position list 3) (position list 90) (position list 900)

        0 // return an integer exit code