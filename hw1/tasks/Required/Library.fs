namespace Required

module Tasks =

    let factorial n =
        let rec inner n i acc =
            if n <= i
            then acc * i
            else inner n (i + 1) (i * acc)
        if n >= 0
        then Some(inner n 1 1)
        else None

    let fibonacci n =
        let rec inner n0 n1 i n =
            if n = i
            then n1
            else inner n1 (n0 + n1) (i + 1) n
        if n = 0
            then Some(0)
        elif n < 0
            then None
            else Some(inner 0 1 1 n)

    let reverse list =
        let rec inner list acc =
            match list with
                | [] -> acc
                | hd :: tl -> inner tl (hd :: acc)
        inner list []

    let powersOf2 n m =
        let rec inner n sum acc i current =
            if i = sum
            then current :: acc
            else inner n sum (if i >= n then (current :: acc) else acc) (i + 1) (2 * current)
        if n < 0 || m < 0
        then None
        else Some(reverse (inner n (n + m) [] 0 1))

    let position list element =
        let rec inner list position element =
            match list with
                | [] -> None
                | hd :: tl ->
                    if hd = element
                    then Some(position)
                    else inner tl (position + 1) element
        inner list 0 element