namespace Tasks

module Numbers =
    
    let fold_even_count =
        List.fold (fun acc i -> if i % 2 = 0 then acc + 1 else acc) 0

    let filter_even_count list =
        List.length (List.filter (fun i -> i % 2 = 0) list)

    let map_even_count list =
        List.fold (fun acc f -> if f then acc + 1 else acc) 0 (List.map (fun i -> i % 2 = 0) list)

    let primes =
        let newState primes =
            let rec next i =
                let rec isPrime primes =
                    match primes with
                    | [] -> true
                    | hd :: tl ->
                        if i % hd = 0
                        then false
                        else isPrime tl
                if isPrime primes
                then i
                else next (i + 1)
            match primes with
            | [] -> Some (2, [2])
            | hd :: _ ->
                let prime = next hd
                Some (prime, prime :: primes)
        Seq.unfold newState []

module Forest =
    
    type Tree<'a> =
        | Node of Tree<'a> * 'a * Tree<'a>
        | Empty

