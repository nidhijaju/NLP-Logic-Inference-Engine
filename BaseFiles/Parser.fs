module Parser
open TypesShared

let unWrapToken (inp:Token) =
    match inp with
    | N s -> s
    | Q s -> s
    | QR s -> s
    | A s -> s
    | C s -> s
    | R s -> s
    | V s -> s
    | O s -> s
    | I s -> s
    | U s -> s
    | _ -> failwith "unfound token"

/// LEXER ------------- ///
// e.g. "Hari ia a dog" -> ["Hari"; "is"; "a"; "dog"]
// e.g. "All-!X are happy" -> ["All-!X"; "are"; "happy"]
let lexer (str:string) = str.Split() |> Array.toList

/// PARSER ----------- ///
let pEBindExp msg = Some(None, (Error msg))
let optError msg = (Some << Error) msg

let pMap f inp =
    match inp with
    | Error msg -> optError msg
    | Ok inp -> Some <| f inp

let (|PMATCH|_|) (w) = 
    (fun inp ->
        match inp with
        | [] -> Error []
        | s :: rest -> 
            if (s = (w (unWrapToken s)))
            then Ok (Ok rest, unWrapToken s) 
            else Error rest)
    |> pMap

let PMatch = (|PMATCH|_|)

// N("dog") -> NOUN("dog")
let rec (|PNOUN|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH N (Ok (lst, str)) -> 
        match lst with
        | Ok _ -> Some ((Some (NOUN str), lst))
        | _ -> Some(None, lst)
    | _ -> Some (None, inp)
 
let rec (|POBJ|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH O (Ok (lst, str)) -> 
        match lst with
        | Ok _ -> Some ((Some (OBJ str), lst))
        | _ -> Some (None, lst)
    | _ -> Some (None, inp)

let rec (|PRAD|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH R (Ok (lst, str)) -> 
        match lst with
        | Ok _ -> Some ((Some (RAD str), lst))
        | _ -> Some (None, lst)
    | _ -> Some (None, inp)

let rec (|PADJ|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH A (Ok (lst, str)) -> 
        match lst with 
        | Ok _ -> Some ((Some (ADJ str), lst))
        | _ -> Some (None, lst)
    | _ -> Some (None, inp)
    
and (|PVERB|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PNOUN ((Some non1, PMATCH V (Ok (PNOUN ((Some non2, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, non1, non2)), lstRest)
        | _ -> Some (None, lstRest)
    | POBJ ((Some obj, PMATCH V (Ok (PNOUN ((Some non, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, obj, non)), lstRest)
        | _ -> Some (None, lstRest)
    | PNOUN ((Some non, PMATCH V (Ok (PADJ ((Some adj, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, non, adj)), lstRest)
        | _ -> Some (None, lstRest)
    | PNOUN ((Some non, PMATCH V (Ok (POBJ ((Some obj, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, non, obj)), lstRest)
        | _ -> Some (None, lstRest)
    | POBJ ((Some obj, PMATCH V (Ok (PADJ ((Some adj, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, obj, adj)), lstRest)
        | _ -> Some (None, lstRest)
    | POBJ ((Some obj, PMATCH V (Ok (POBJ ((Some obj2, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, obj, obj2)), lstRest)
        | _ -> Some (None, lstRest)
    | PRAD ((Some rad, PMATCH V (Ok (PNOUN ((Some non, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, rad, non)), lstRest)
        | _ -> Some (None, lstRest)
    | PRAD ((Some rad, PMATCH V (Ok (PADJ ((Some adj, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, rad, adj)), lstRest)
        | _ -> Some (None, lstRest)
    | PRAD ((Some rad1, PMATCH V (Ok (PRAD ((Some rad2, lstRest)), strV)))) ->
        match lstRest with
        | Ok _ -> Some (Some (VERB (strV, rad1, rad2)), lstRest)
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PIMPL|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH I (Ok (PSTDEXP (Some exp, lstRest), strI)) ->
        match lstRest with
        | Ok _ -> Some (Some (IMPL (strI, exp)), lstRest)
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PQUANT|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH Q (Ok (PSTDEXP (Some exp, lstRest), strQ)) ->
        match lstRest with
        | Ok _ -> Some (Some (QUANT (strQ, exp)), lstRest)
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PCOND|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH C (Ok (PSTDEXP (Some exp, (PIMPL (Some impl, lstRest))), strC)) ->
        match lstRest with
        | Ok _ -> Some (Some (COND (strC, exp, impl)), lstRest)
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PQUANTR|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PMATCH QR (Ok (PSTDEXP (Some exp, lstRest), strQR)) ->
        match lstRest with
        | Ok _ ->  
            let splitter = strQR.Split() |> Array.toList
            match splitter with
            | all::rad::tl -> 
                Some (Some (QUANTR (all, rad, exp)), lstRest)
            | _ -> failwith "QUANTR Splitting Error"
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PUNION|_|) inp =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PVERB (Some verb, PMATCH U (Ok (PSTDEXP (Some exp, lstRest), strU))) ->
        match lstRest with
        | Ok _ -> Some (Some (UNION (strU, verb, exp)), lstRest)
        | _ -> Some (None, lstRest)
    | _ -> Some (None, inp)

and (|PSTDEXP|_|) inp : (Ast option * Result<Token list, Token list>) option =
    match inp with
    | Error unparsed -> Some (None, Error unparsed)
    | PQUANTR (Some ast, Ok rest) -> Some (Some ast, Ok rest)
    | PQUANT (Some ast, Ok rest) -> Some (Some ast, Ok rest)
    | PCOND (Some ast, Ok rest) -> Some (Some ast, Ok rest)
    | PUNION (Some ast, Ok rest) -> Some (Some ast, Ok rest)
    | PVERB (Some ast, Ok rest) -> Some (Some ast, Ok rest)
    // | PNOUN (Ok (Some ast, Ok rest)) -> Some (Some ast, Ok rest)
    // | POBJ (Ok (Some ast, Ok rest)) -> Some (Some ast, Ok rest)
    // | PRAD (Ok (Some ast, Ok rest)) -> Some (Some ast, Ok rest)
    | Ok(inp) -> Some(None, Error inp)

/// PAP Functions --------- ///
let StdExp = (|PSTDEXP|_|)
let PQuantr = (|PQUANTR|_|)
let PQuant = (|PQUANT|_|)
let PCond = (|PCOND|_|)
let PUnion = (|PUNION|_|)
let PVerb = (|PVERB|_|)

let getSome inp = inp |> List.choose id

/// Parser Top Level --------- ///
let parseMatch inp =
    let paplst = [PQuantr, "Radical Quanitifier"; PQuant, "Quanitifier"; PCond, "Conditional"; PUnion, "Union"; PVerb, "Verb"]
    let errorHandler lexinp (state:Result< 'a , int option * string>) inp =
        match state with
        | Ok y -> Ok y
        | Error (Some (count), str) ->
            match (fst inp lexinp) with
            | Some (Some ast, Ok []) -> Ok ast
            | Some (Some ast, Ok lst) -> 
                if count < lst.Length then Error(Some count, str) else Error(Some lst.Length, snd inp)
            | Some (None, Ok lst) -> 
                if count < lst.Length then Error(Some count, str) else Error(Some lst.Length, snd inp)
            | _ -> failwith "shouldn't happen"
        | Error (None, str) ->
            match (fst inp lexinp) with
            | Some (Some ast, Ok []) -> Ok ast
            | Some (Some ast, Ok lst) -> Error(Some lst.Length, snd inp)
            | Some (None, Ok lst) -> Error(Some lst.Length, snd inp)
            | _ -> failwith "shouldn't happen"
    let maxParse = List.fold (errorHandler inp) (Error (None, "")) paplst
    maxParse

let parOutput inp = 
    match inp with
    | Ok ast -> Ok ast
    | Error (Some length, str) -> Error <| sprintf "Attempted to parse %A expression but failed with %i tokens left" str length
    | _ -> Error <| sprintf "shouldn't happen"

let parser inp=
    let parIn = getSome inp
    let parOut = List.map parseMatch <| parIn
    let parsedExps = List.map parOutput <| parOut
    parsedExps