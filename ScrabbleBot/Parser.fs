// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"     
    let pPointValue = pstring "pointValue"
    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter        = satisfy System.Char.IsLetter
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p  = pchar '(' >*>. p .>*> pchar ')'
    let curlybrackets p = pchar '{' >*>. p .>*> pchar '}'
    let apostrophise p = pchar ''' >>. p .>> pchar '''

    let implode cs =
        let folder = fun c s -> (string c) + s
        List.foldBack folder cs ""

    let tupleToList (c: char, l:char list) =
        c :: l

    let punderscore = pchar '_'

    let pid = (pletter <|> punderscore) .>>. (palphanumeric <|> punderscore |> many) |>> tupleToList |>> implode

    let unop a b = a >*>. b
    let binop op a b = a .>*> op .>*>. b

    // Aexp Parsers
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegParse = pchar '-'  >>. pint32 |>> (fun x -> Mul (N -1, N x)) <?> "Neg"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Variable"
    let VParse1  = pletter .>>. many1 palphanumeric |>> tupleToList |>> implode |>> V <?> "Variable"
    let ParParse = parenthesise TermParse
    let PVParse  = pPointValue >*>. ParParse |>> PV <?> "PointValue"

    let AexpParse = TermParse

    // Cexp parsers
    // It is written in a confusing way below this point, this is due to the mutual recursion between Aexp and Cexp.
    // there is probably a better way :-)

    let CharacterParse, cref = createParserForwardedToRef<cExp>()

    let CTIParse = pCharToInt >*>. parenthesise CharacterParse |>> CharToInt <?> "CharToInt"

    do aref := choice [NegParse; PVParse; NParse; CTIParse; ParParse; VParse]

    let CexpParse = CharacterParse
    let CParse    = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let CVParse   = pCharValue >*>. parenthesise TermParse |>> CV <?> "CharacterValue"
    let TUParse   = pToUpper  >*>. parenthesise CharacterParse |>> ToUpper <?> "ToUpper"
    let TLParse   = pToLower  >*>. parenthesise CharacterParse |>> ToLower <?> "ToLower"
    let ITCParse  = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    do cref := choice [TUParse; TLParse; ITCParse; CVParse; CParse]


    // Bexp parsers
    let AndOrParse, aoref = createParserForwardedToRef<bExp>()
    let EqualityParse, eref = createParserForwardedToRef<bExp>()
    let AtomBParse, abref = createParserForwardedToRef<bExp>()

    let BexpParse = AndOrParse

    let TParse               = pTrue |>> (fun _ -> TT) <?> "True"
    let FParse               = pFalse |>> (fun _ -> FF) <?> "False"
    let EqualParse           = TermParse .>*> pchar '=' .>*>. TermParse |>> AEq <?> "AEq"
    let NotEqualParse        = TermParse .>*> pstring "<>" .>*>. TermParse |>> (fun (x,y) -> x .<>. y) <?> "NotAEq"
    let LessThanParse        = TermParse .>*> pchar '<' .>*>. TermParse |>> ALt <?> "ALt"
    let MoreThanParse        = TermParse .>*> pchar '>' .>*>. TermParse |>> (fun (x,y) -> x .>. y) <?> "AMt"
    let LessThanOrEqualParse = TermParse .>*> pstring "<=" .>*>. TermParse |>> (fun (x,y) -> x .<=. y)  <?> "LTOEq"
    let MoreThanOrEqualParse = TermParse .>*> pstring ">=" .>*>. TermParse |>> (fun (x,y) -> x .>=. y) <?> "MTOEq"
    let NotParse             = pchar '~' >*>. AndOrParse |>> Not <?> "Not"
    let ConjParse            = EqualityParse .>*> pstring "/\\" .>*>. AndOrParse |>> Conj <?> "Conj"
    let DisParse             = EqualityParse .>*> pstring "\\/" .>*>. AndOrParse |>> (fun (x,y) -> x .||. y) <?> "Dis"
    let IsDigitParse         = pIsDigit >*>. parenthesise CharacterParse |>> IsDigit <?> "IsDigit"
    let IsLetterParse        = pIsLetter >*>. parenthesise CharacterParse |>> IsLetter <?> "IsLetter"
    let BParParse            = parenthesise AndOrParse

    do aoref := choice [ConjParse; DisParse; EqualityParse]
    do eref := choice [EqualParse; NotEqualParse; LessThanOrEqualParse; MoreThanOrEqualParse; LessThanParse; MoreThanParse; AtomBParse]
    do abref := choice [IsDigitParse; IsLetterParse; NotParse; TParse; FParse; BParParse]

    //Statement parsers

    let stmntParse, seref = createParserForwardedToRef<stm>()
    let SeqParse, sref = createParserForwardedToRef<stm>()

    let DeclareParse  = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let SequenceParse = SeqParse .>*> pchar ';' .>*>. stmntParse |>> (fun (x, y) -> Seq(x, y)) <?> "Seq"
    let ITEParse      = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlybrackets stmntParse .>*> pelse .>*>. curlybrackets stmntParse |>> (fun ((x,y), z) -> ITE (x, y, z)) <?> "ITE"
    let ITParse       = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlybrackets stmntParse |>> (fun (x, y) -> ITE (x, y, Skip)) <?> "IT"
    let WhileParse    = pwhile >*>. parenthesise BexpParse .>*> pstring "do" .>*>. curlybrackets stmntParse |>> While <?> "While"
    let SkipParse     = pstring "Skip" |>> (fun _ -> Skip) <?> "Skip"
    let AssParse      = pid .>*> (pstring ":=") .>*>. TermParse |>> (fun (x, y) -> Ass (x, y)) <?> "Ass"
    do seref := choice [SequenceParse; SeqParse]
    do sref := choice [AssParse; DeclareParse; ITEParse; ITParse; WhileParse; SkipParse;]


(* These five types will move out of this file once you start working on the project *)
    (* type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord

            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        } *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    type boardFun = coord -> square option

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    //type squareFun = word -> int -> int -> int

    // 7.12
    // TODO, it prints the correct values, but what happens to n?
    let parseSquareFun (sqp: squareProg) =
        Map.map (fun n str -> ((stmntToSquareFun (run stmntParse str |> getSuccess)))) sqp

    //7.13
    // This terminates locally, and gives the correct output, but on code-judge it says time limit exceeded after a few minutes.
    (*
    The output I get locally:
    Some "single letter score"
    Some "double letter score"
    Some "triple letter score"
    Some "double word score"
    Some "triple word score"
    None
    *)
    let parseBoardFun str m =
         stmntToBoardFun (run stmntParse str |> getSuccess) m

    (*
    The output I get on my computer:

    # # # # # # # # # # # # # # # # # # # # #
    # # # # # # # # # # # # # # # # # # # # #
    # # # # # # # # # # # # # # # # # # # # #
    # # # 9 3 3 4 3 3 3 9 3 3 3 4 3 3 9 # # #
    # # # 3 6 3 3 3 5 3 3 3 5 3 3 3 6 3 # # #
    # # # 3 3 6 3 3 3 4 3 4 3 3 3 6 3 3 # # #
    # # # 4 3 3 6 3 3 3 4 3 3 3 6 3 3 4 # # #
    # # # 3 3 3 3 6 3 3 3 3 3 6 3 3 3 3 # # #
    # # # 3 5 3 3 3 5 3 3 3 5 3 3 3 5 3 # # #
    # # # 3 3 4 3 3 3 4 3 4 3 3 3 4 3 3 # # #
    # # # 9 3 3 4 3 3 3 3 3 3 3 4 3 3 9 # # #
    # # # 3 3 4 3 3 3 4 3 4 3 3 3 4 3 3 # # #
    # # # 3 5 3 3 3 5 3 3 3 5 3 3 3 5 3 # # #
    # # # 3 3 3 3 6 3 3 3 3 3 6 3 3 3 3 # # #
    # # # 4 3 3 6 3 3 3 4 3 3 3 6 3 3 4 # # #
    # # # 3 3 6 3 3 3 4 3 4 3 3 3 6 3 3 # # #
    # # # 3 6 3 3 3 5 3 3 3 5 3 3 3 6 3 # # #
    # # # 9 3 3 4 3 3 3 9 3 3 3 4 3 3 9 # # #
    # # # # # # # # # # # # # # # # # # # # #
    # # # # # # # # # # # # # # # # # # # # #
    # # # # # # # # # # # # # # # # # # # # #
    *)
    let parseBoardProg (bp : boardProg) : board =
        let nm = Map.map (fun n sqp -> (parseSquareFun sqp)) bp.squares
        { center        = bp.center
          defaultSquare = (nm.TryFind bp.usedSquare |> Option.get)
          squares       = parseBoardFun bp.prog nm }
