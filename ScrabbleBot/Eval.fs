// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let add (a: SM<int>) (b: SM<int>) = a >>= fun x -> b >>= fun y -> ret (x + y)
    let sub (a: SM<int>) (b: SM<int>) = a >>= fun x -> b >>= fun y -> ret (x - y)
    let mul (a: SM<int>) (b: SM<int>) = a >>= fun x -> b >>= fun y -> ret (x * y)
    let div (a: SM<int>) (b: SM<int>) = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x / y) else fail DivisionByZero
    let modu (a: SM<int>) (b: SM<int>) = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x % y) else fail DivisionByZero


    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *)

    let (.=.) a b = AEq (a, b)
    let (.<.) a b = ALt (a, b)
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

    let rec arithEval a : SM<int> =
        match a with
        | N n         -> ret n
        | V v         -> lookup v
        | WL          -> wordLength >>= fun x -> ret(x)
        | PV a        -> arithEval a >>= pointValue
        | Add (a, b)  -> add (arithEval a) (arithEval b)
        | Sub (a, b)  -> sub (arithEval a) (arithEval b)
        | Mul (a, b)  -> mul (arithEval a) (arithEval b)
        | Div (a, b)  -> div (arithEval a) (arithEval b)
        | Mod (a, b)  -> modu (arithEval a) (arithEval b)
        | CharToInt c -> charEval c >>= fun x -> ret(x |> int)
    and charEval c : SM<char> =
        match c with
        | C c         -> ret c
        | CV a        -> arithEval a >>= characterValue
        | ToUpper c   -> charEval c >>= fun x -> ret(System.Char.ToUpper x)
        | ToLower c   -> charEval c >>= fun x -> ret(System.Char.ToLower x)
        | IntToChar a -> arithEval a >>= fun x -> ret(x |> char)
    and boolEval b : SM<bool> =
        match b with
        | TT           -> ret true
        | FF           -> ret false
        | AEq (a, a1)  -> arithEval a >>= fun x -> arithEval a1 >>= fun y -> ret(x = y)
        | ALt (a, a1)  -> arithEval a >>= fun x -> arithEval a1 >>= fun y -> ret(x < y)

        | Not b        -> boolEval b >>= fun x -> ret(not x)
        | Conj (b, b1) -> boolEval b >>= fun x -> boolEval b1 >>= fun y -> ret(x && y)

        | IsLetter c   -> charEval c >>= fun x -> ret(System.Char.IsLetter x)
        | IsDigit c    -> charEval c >>= fun x -> ret(System.Char.IsDigit x)

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare s              -> declare s
        | Ass (s, a)             -> arithEval a >>= update s
        | Skip                   -> ret()
        | Seq (stm1, stm2)       -> stmntEval stm1 >>>= stmntEval stm2 >>>= ret()
        | ITE (bExp, stm1, stm2) -> boolEval bExp >>= fun x ->
                                        if x then
                                            push >>>= stmntEval stm1 >>>= pop
                                        else
                                            push >>>= stmntEval stm2 >>>= pop
        | While (b, stm)         -> boolEval b >>= fun x ->
                                        if x then
                                            push >>>= stmntEval stm >>>= (stmntEval (While (b, stm)))
                                        else
                                            ret()
(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let prog = new StateBuilder()

    let rec arithEval2 a = prog {
        match a with
        | N n         -> return n
        | V v         -> return! lookup v
        | WL          -> return! wordLength
        | PV a        -> let! ax = arithEval2 a
                         return! pointValue ax
        | Add (a, b)  -> return! add (arithEval2 a) (arithEval2 b)
        | Sub (a, b)  -> return! sub (arithEval2 a) (arithEval2 b)
        | Mul (a, b)  -> return! mul (arithEval2 a) (arithEval2 b)
        | Div (a, b)  -> return! div (arithEval2 a) (arithEval2 b)
        | Mod (a, b)  -> return! modu (arithEval2 a) (arithEval2 b)
        | CharToInt c -> let! ch = charEval2 c
                         return (ch |> int)
        }
    and charEval2 c : SM<char> = prog {
        match c with
        | C c         -> return c
        | CV a        -> let! ax = arithEval2 a
                         return! characterValue ax
        | ToUpper c   -> let! ch = charEval c
                         return System.Char.ToUpper ch
        | ToLower c   -> let! ch = charEval c
                         return System.Char.ToLower ch
        | IntToChar a -> let! ax = arithEval2 a
                         return (ax |> char)
        }

    let rec boolEval2 b : SM<bool> = prog {
        match b with
        | TT           -> return true
        | FF           -> return false
        | AEq (a, a1)  -> let! ax = arithEval2 a
                          let! ax2 = arithEval2 a1
                          return (ax = ax2)
        | ALt (a, a1)  -> let! ax = arithEval2 a
                          let! ax2 = arithEval2 a1
                          return (ax < ax2)
        | Not b        -> let! bx = boolEval2 b
                          return (not bx)
        | Conj (b, b1) -> let! bx = boolEval2 b
                          let! bx2 = boolEval2 b1
                          return (bx && bx2)
        | IsLetter c   -> let! ch = charEval2 c
                          return (System.Char.IsLetter ch)
        | IsDigit c    -> let! ch = charEval2 c
                          return (System.Char.IsDigit ch)
        }

    let rec stmntEval2 stm : SM<unit> = prog {
        match stm with
        | Declare s              -> do! declare s
        | Ass (s, a)             -> let! ax = arithEval2 a
                                    do! update s ax
        | Skip                   -> return ()
        | Seq (stm1, stm2)       -> do! stmntEval2 stm1
                                    do! stmntEval2 stm2
                                    return ()
        | ITE (bExp, stm1, stm2) -> let! bx = boolEval2 bExp
                                    if bx then do! (stmntEval2 stm1)
                                    else do! (stmntEval2 stm2)
        | While (bExp, stm)      -> let! bx = boolEval2 bExp
                                    if bx then do! (stmntEval2 (While (bExp, stm)))
                                    else do! (stmntEval2 stm)
        }

(* Part 4 *)

    type word = (char * int) list
    type squareFun = word -> int -> int -> int
    type square = Map<int, word -> int -> int -> int>

    let check = function
    | Success a -> a
    | Failure b -> 0

    let check2 = function
    | Success a -> a
    | Failure b -> None

    let stmntToSquareFunOld stm =
        fun w pos acc -> (stmntEval stm) >>>=
            lookup "_result_"
            |> evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"])

    let stmntToSquareFun stm =
        fun w pos acc -> (stmntEval stm) >>>=
            lookup "_result_"
            |> evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"])
            |> check

    type coord = int * int

    type boardFun = coord -> Result<square option, Error>

    let stmntToBoardFun stm squares =
        fun coord -> (stmntEval stm) >>>=
            lookup "_result_" >>=
            (fun x -> ret ( Map.tryFind x squares))
            |> evalSM (mkState [("_x_", fst(coord)); ("_y_", snd(coord)); ("_result_", 0)] word.Empty ["_x_"; "_y_"; "_result_"])
            |> check2

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }
