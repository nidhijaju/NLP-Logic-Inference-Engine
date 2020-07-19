module LogicConversion
open SharedTypes

/// Unwrapping functions for result types to extract relevant feature 
/// from the AST tree in the image ///
let unwrapResultTypes = 
    function 
    | Ok(x) -> Some(x)
    | _ -> None

/// Unwrapping Quant type from the rule list facts ///
let unwrapQuantType inp = 
    let rec unwrapQuantRec radList =
        match inp with 
        | QUANT(x,y) -> Some(y), (Radical x::radList)
        | QUANTR(x,y,z) -> Some(z), (Radical y::radList)
        | _ -> None, radList
    unwrapQuantRec []

/// The functions below are the core of the ground truth logic conversion,
/// they unwrap on the verb types and produce Object List, GroundTruth List 
/// and Predicate List where each are DU types (taken from SharedTypes.fs) ///

let objectMatcher inp objList = 
    match inp with
    | OBJ x -> 
        (Object x::objList, Object x)
        |> Some
    | _ -> None

let simplePredMatcher tryObj tryNoun  =
    let rec simpleRec objList gtList prList = 
        let firstArg = objectMatcher tryObj objList
        match firstArg with
        | Some (newOBJList, objTerm) ->
            match tryNoun with
            | NOUN x | ADJ x -> 
                let newGTList = (Predicate x, [objTerm])::gtList
                let newPRList = Predicate x::prList
                (newOBJList, newGTList, newPRList) |> Some
            | _ -> None
        | None -> None
    simpleRec [] [] []

let complexPredMatcher verbStr tryObj1 tryObj2 =
    let rec complexRec objList gtList prList = 
        let firstArg = objectMatcher tryObj1 objList
        match firstArg with
        | Some (newObjList, objTerm) -> 
            let secondArg = objectMatcher tryObj2 newObjList
            match secondArg with
            | Some (newObjList2, objTerm2) ->
                let newGTList = ((Predicate verbStr), [objTerm; objTerm2])::gtList
                let newPRList = Predicate verbStr::prList 
                (newObjList2, newGTList, newPRList) |> Some
            | None -> None
        | None -> None
    complexRec [] [] []

let ruleCasePredMatcher verbstr tryNoun tryNext =
    let rec ruleCaseRec prList prList1  =
        match tryNoun with
        | NOUN x ->
            match tryNext with
            | NOUN y | ADJ y | OBJ y ->
                match verbstr with 
                | "Are" ->  let newfactList = [PREDICATEEXP (Predicate x);
                            PREDICATEEXP (Predicate y)]@prList
                            let newprList = [Predicate x; Predicate y]@prList1
                            (newfactList, newprList)
                | _ -> let newfactList = [PREDICATEEXP (Predicate verbstr); 
                                          PREDICATEEXP (Predicate x);
                                          OBJECTEXP (Object y)]@prList
                       let newprList = [Predicate verbstr;Predicate x]@prList1
                       (newfactList, newprList)
                |> Some
            | _ -> None
        | _ -> None
    ruleCaseRec [] []

let unwrapVerbSimple = 
    function
    | VERB(x,y,z) when (x="Is") ->
        simplePredMatcher y z
    | VERB(x,y,z) -> 
        complexPredMatcher x y z
    | _ -> None

let unwrapVerbFact = 
    function
    | VERB(x,y,z) -> ruleCasePredMatcher x y z 
    | _ -> None      

/// Below are the high level functions that produce each of the DU type lists ////

let returnGrammar inp inp1 = 
    (List.map inp inp1) 
    |> List.choose id

let genericUnwrapping inp = 
    inp |> List.choose id
        |> returnGrammar unwrapResultTypes
        |> returnGrammar unwrapVerbSimple

let produceGroundTruth inp = 
    let scnd (_,x,_) = x 
    let genGT = genericUnwrapping inp
    genGT |> List.map (scnd >> List.map GroundTruth)
          |> List.collect id

let genericObjPred inp fn = 
    inp |> List.map fn 
        |> List.collect id
        |> List.distinct

let produceObj inp = 
    let frst (x,_,_) = x
    let genObj = genericUnwrapping inp
    genericObjPred genObj frst |> List.map (fun x -> [x])

let producePred inp = 
    let thrd (_,_,x) = x
    let genPred = genericUnwrapping inp
    genericObjPred genPred thrd

/// The functions below are implemented to extract logic statements 
/// from the rule list that is received from the parser module
let genericFactType inp = 
    inp |> List.choose id
        |> returnGrammar unwrapResultTypes
        |> returnGrammar (unwrapQuantType >> fst)

let findConditional inp = 
    match inp with 
    | COND(_,x,y) -> Some (x,y)
    | _ -> None

/// The functions below are to match with complex radical type sentences 
/// and are mainly preparation for group phase of the project, hence they 
/// are not tested until all modules are put together


/// The functions below can take a rule list and give out a WFF List List 
/// for each of the rules that will be received from parser module ///

let produceFactPred inp = 
    let simpleFact = genericFactType inp
    (simpleFact)  |> List.map (unwrapVerbFact)
                  |> List.choose id
                //|> List.collect id

let quickFix a =
    match a with
    | hd1::hd2::tl ->
        (hd1,hd2)
    | _ -> failwith "Errorsdmjkflsdjohif"

let produceWFFList (inp:(WFF list * Predicate list) list) = 
    inp |> List.map (quickFix << fst)

let appendPredList (inp:('a * Predicate list) list) predList = 
    inp |> List.map snd 
        |> List.collect id
        |> List.append predList

let radicalMatcher inp radList =
    match inp with
    | RAD x -> 
        (Radical x::radList)
        |> Some
    | _ -> None

let trial (inp: Ast*Ast) = 
    match inp with 
    | (x,y) -> match y with 
               | IMPL(_,a) -> Some(x,a)
               | _ -> None             


let simpleFactMatcher tryRad tryNext  =
    let rec simpleRecF radList predRList = 
        let firstArg = radicalMatcher tryRad radList
        match firstArg with
        | Some(newRadList) -> 
            match tryNext with
            | NOUN x | ADJ x -> 
                (PREDICATER(Predicate x, newRadList))::predRList
                |> Some
            | _ -> None     
        | None -> None
    simpleRecF [] []

let extractRadicalFacts inp = 
    match inp with 
    | VERB(x,y,z) -> simpleFactMatcher y z 
    | _ -> None

let produceComplexFactPred inp = 
    let complexFact = genericFactType inp 
                      |> returnGrammar findConditional
                      |> List.map trial
                      |> List.choose id
    let firstEle = List.map (fst >> extractRadicalFacts) complexFact
                  |> List.choose id
                  |> List.collect id 
    let secondEle = List.map (snd >> extractRadicalFacts) complexFact
                  |> List.choose id
                  |> List.collect id
    List.zip firstEle secondEle

let complexPredList (inp: (WFF*WFF) list) =
    let takeFirstPred = List.map (fst >> fun x -> 
                                        match x with 
                                        | PREDICATER (x,_) -> Some x 
                                        | _ -> None) inp
                        |> List.choose id
    let takeSecondPred = List.map (snd >> fun x -> 
                                        match x with 
                                        | PREDICATER (x,_) -> Some x 
                                        | _ -> None) inp     
                        |> List.choose id               
    (takeFirstPred @ takeSecondPred)

let getGT (inp: Result<Ast, string> option list) =
    let gtAgrim = inp |> produceGroundTruth
    let objAgrim = inp |> produceObj
    let predAgrim = inp |> producePred
    (gtAgrim, objAgrim, predAgrim)

let getRl (inp: Result<Ast, string> option list) oldPredList =
    let initial = produceFactPred inp
    let agrim1 = produceComplexFactPred inp
    let newTry = produceWFFList initial
    let agrim2 = complexPredList agrim1
    let g = agrim2 @ (appendPredList initial oldPredList)
            |> List.distinct
    (agrim1 @ newTry, g)