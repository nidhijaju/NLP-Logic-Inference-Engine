module Evaluator

open TypesShared

// for making pairs of preds and objects (to replace List.AllPairs in Fable)
let makeAllPairs zlst alst = 
    let makepairs x = 
        List.map (fun y -> (x,y)) alst
    List.collect makepairs zlst 

// unwrapping the Result type and removing Errors
let unwrap lst = 
    List.where (fun x -> match x with
                         | Ok _ -> true
                         | Error _ -> false) lst 
    |> List.map (fun x -> match x with
                          | Ok y ->  y
                          | Error _ -> failwithf "SHOULD NEVER HAPPEN")
                          
// separating Result list into facts and "no info"
let selectresult lst = 
    let tmp =  List.partition (fun x -> match x with
                                        | Some(Ok _) -> true
                                        | Some(Error _ )-> false
                                        | _ -> failwithf "SHOULD NEVER HAPPEN") lst
    if List.isEmpty (fst tmp) 
    then snd tmp 
    else fst tmp

// checking if the proposed predicate/object combo is true
let factchecker (workinglist : Result<(Predicate*(Object list)*bool),string> list)(basedon : Predicate*(Object list) )
    : Result<Predicate*(Object list),string> option =
    let finder lst = 
        match List.tryFind (fun (a,b,_) -> (a,b)=basedon)lst with
        | Some (x,y,z) -> match z with
                          | true -> Some( Ok (x,y))
                          | false -> Some( Error "found fact not true")
        | None -> Some (Error "this is not true")
    workinglist|> unwrap |> finder

// finds out if the proposed base case predicate is in the ground truth list
let predicatematcher truthlist workinglist prd objs: bool = 
    (List.exists (fun elem -> GroundTruth(prd,objs) = elem) truthlist )
    || List.exists (fun inp -> match inp with
                                | Ok(a,b,_) -> (a,b) = (prd,objs)
                                | Error _-> false) workinglist

// takes proposed implications and checks the base case is found in the ground truths list or the working list
let compare (truthlist : GroundTruth list)(pred1 : WFF)(pred2 : WFF)(workinglist : Result<(Predicate*(Object list)*bool),string> list)(objects : Object list) 
            : Result<(Predicate*(Object list)*bool),string> list=
    let predmatcher = predicatematcher truthlist workinglist
    let rec scan o pred : Result<(Predicate*(Object list)*bool),string> list = // decompose input to base case predicate and return evaluation
        match (pred, pred2) with 
        | (AND (x,y),_) -> match (scan o x),(scan o y) with
                           | ([Ok(_,_,true)], [Ok(c,d,true)]) -> [Ok(c,d,true)]
                           | ([Ok(_,_,true)], [Ok(c,d,false)]) -> [Ok(c,d,false)]
                           | ([Ok(c,d,false)], [Ok(_,_,true)]) -> [Ok(c,d,false)]
                           | ([Ok(a,b,false)], [Ok(c,d,false)]) -> [Ok(c,d,false); Ok(a,b,false)]
                           | _ -> [Error "NOMATCH"]
        | (OR (x,y),_) -> match (scan o x),(scan o y) with 
                           | ([Ok(_,_,true)], [Ok(c,d,true)]) -> [Ok(c,d,true)]
                           | ([Ok(c,d,true)], [Ok(_,_,false)]) -> [Ok(c,d,true)]
                           | ([Ok(_,_,false)], [Ok(c,d,true)]) -> [Ok(c,d,true)]
                           | ([Ok(a,b,false)], [Ok(c,d,false)]) -> [Ok(c,d,false); Ok(a,b,false)]
                           | ([Ok(c,d,true)], [Error "NOMATCH"]) -> [Ok(c,d,true)]
                           | ([Error "NOMATCH"], [Ok(c,d,true)]) -> [Ok(c,d,true)]
                           | _ -> [Error "NOMATCH"]
        | (PREDICATER (Predicate (x,bl),y), PREDICATER (Predicate (a,bl2),b)) -> if predmatcher (Predicate (x,bl)) o
                                                                                 then [Ok( (Predicate (a,bl2)), [Map.find (b.Head) (Map.ofList (List.zip y o))], true)]
                                                                                 elif predmatcher (Predicate (a, not bl2)) o
                                                                                 then [Ok((Predicate (x, not bl)), o, true)]
                                                                                 elif predmatcher (Predicate (a,bl2)) o
                                                                                 then [Ok((Predicate (x,bl)), o, false)]
                                                                                 else [Error "NOMATCH"]
        | (PREDICATEEXP (Predicate (prd,bl)), PREDICATEEXP (Predicate (prd2,bl2))) -> if predmatcher (Predicate (prd,bl)) o
                                                                                       then [Ok(Predicate (prd2,bl2), o, true)]
                                                                                       elif predmatcher (Predicate (prd2, not bl2)) o
                                                                                       then [Ok(Predicate (prd, not bl), o, true)]
                                                                                       elif predmatcher (Predicate (prd2,bl2)) o
                                                                                       then [Ok(Predicate (prd,bl), o, false)]
                                                                                       else [Error "NOMATCH"]
        | _ -> failwithf "INCONSITENT WORLD" // any other combination of WFFs is logically incorrect            
    scan objects pred1 

// top-level function                               
let evaluate (truthlist : GroundTruth list) 
             (objs :(Object list) list) 
             (preds : Predicate  list) 
             (funcs : (WFF*WFF) list) 
             : Result<(Predicate*Object list),string> option list = 
    try 
        let scanner = compare truthlist // scanner curried with ground truth list
        let evals = List.map (fun (x,y) -> scanner x y ) funcs // list of scanners curried with implications
        let folder inp =  ([], evals) ||> List.fold (fun lst s -> lst @ List.concat [s lst inp]) // list of working lists
        let worklists = objs |> List.map folder
        let predlst = ([], preds) ||> List.fold (fun lst (Predicate (x,_)) -> lst @ [Predicate(x,true);Predicate(x,false)])
        makeAllPairs worklists (makeAllPairs predlst objs)
        |> List.map (fun (x,y) -> factchecker x y)
        |> selectresult 
    with
        | msg -> [Some (Error "Hello")]
