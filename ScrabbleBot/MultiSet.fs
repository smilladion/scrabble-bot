// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    open System

    type MultiSet<'a when 'a : comparison> =  
        | MS of Map<'a, uint32>
        override q.ToString() =
            match q with
            | MS s -> let str = "{" + (Map.fold (fun acc key value -> acc + sprintf "(%A, #%A), " key (int value) ) "" s)
                      str.Remove(str.Length - 2) + "}"

    let empty : MultiSet<'a> = (MS Map.empty)

    let isEmpty (MS s) = Map.isEmpty s

    let size (MS s) = Map.fold (fun acc key value -> acc + value) 0u s

    let contains a (MS s) = Map.containsKey a s

    let numItems a (MS s) = 
        match s.TryFind(a) with
        | None -> 0u
        | Some v -> v

    let add a n (MS s) =
        if (n = 0u) then MS (s.Remove a)
        else MS (s.Add(a, numItems a (MS s) + n))

    let addSingle a s =
        add a 1u s
        
    let remove a n (MS s) = 
        MS (if n >= numItems a (MS s) then s.Remove(a) else s.Add(a, (numItems a (MS s) - n)))
        
    let removeSingle a (MS s) = 
        remove a 1u (MS s)

    let fold f acc (MS s) = Map.fold f acc s

    let foldBack f (MS s) acc = Map.foldBack f s acc

    let map f (MS s) = 
        fold (fun acc key value ->
        let v = value
        let k = f key
        add k v acc) empty (MS s)
        
    let ofList lst = List.foldBack addSingle lst empty

    let toList s = foldBack (fun key value acc -> List.replicate ((int) value) key @ acc) s [];;

    let union (MS s1) (MS s2) = 
       MS ((fold (fun acc key value -> 
            if (value > numItems key (MS acc))
            then (acc.Add(key, value)) 
            else (acc.Add(key, (numItems key (MS acc))))) s2 (MS s1)))

    let sum s1 s2 = 
       fold (fun acc key value -> add key value acc) s2 s1

    let subtract (MS s1) (MS s2) = 
        MS (fold (fun acc key value -> 
            if (value - numItems key (MS acc) <= 0u)
            then acc.Remove key 
            else acc.Add(key, value - numItems key (MS acc))) s2 (MS s1))

    let intersection (MS s1) (MS s2) =
        fold (fun (MS acc) key value ->
            if numItems key (MS s1) <> 0u
            then MS (acc.Add(key, Math.Max(value, numItems key (MS acc))))
            else (MS acc)) empty (MS s2)
