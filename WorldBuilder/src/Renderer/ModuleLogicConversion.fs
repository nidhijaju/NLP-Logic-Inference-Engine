module LogicConversion
open TypesShared

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

/// Function that matches on object and creates an Object list /// 
let objectMatcher inp objList = 
    match inp with
    | OBJ x -> 
        (Object x::objList, Object x)
        |> Some
    | _ -> None

/// Function that changes boolType based on if predicate is NOT or not ///
let simpleNegatedChecker inp boolType newOBJList objTerm gtList prList = 
    match inp with 
    | NOUN x | ADJ x -> 
        let newGTList = (Predicate (x,boolType), [objTerm])::gtList
        let newPRList = Predicate (x,boolType)::prList
        (newOBJList, newGTList, newPRList) |> Some
    | _ -> None

/// Function that matches on simple predicate types and outputs Object and Predicate list ///
let simplePredMatcher tryObj tryNoun =
    let rec simpleRec objList gtList prList = 
        let firstArg = objectMatcher tryObj objList
        match firstArg with
        | Some (newOBJList, objTerm) ->
            match tryNoun with 
            | NOT(x) -> simpleNegatedChecker x false newOBJList objTerm gtList prList
            | x -> simpleNegatedChecker x true newOBJList objTerm gtList prList
        | None -> None
    simpleRec [] [] []

/// Helper function for complex predicate matcher function ///
let objectMatcherComplex inp objList = 
    match inp with
    | OBJ x | NOUN x | ADJ x -> 
        (Object x::objList, Object x)
        |> Some
    | _ -> None

/// Function specifically for complex predicate sentences that require more processing on data types ///
let complexPredMatcher verbStr tryObj1 tryObj2 =
    let rec complexRec objList gtList prList = 
        let firstArg = objectMatcher tryObj1 objList
        match firstArg with
        | Some (newObjList, objTerm) -> 
            let secondArg = objectMatcherComplex tryObj2 newObjList
            match secondArg with
            | Some (newObjList2, objTerm2) ->
                let newGTList = ((Predicate verbStr), [objTerm; objTerm2])::gtList
                let newPRList = Predicate verbStr::prList 
                (newObjList2, newGTList, newPRList) |> Some
            | None -> None
        | None -> None
    complexRec [] [] []

/// Function specifically designed for rule list AST as these sentences will be more complex to handle ///
let ruleCasePredMatcher verbstr tryNoun tryNext =
    let rec ruleCaseRec prList prList1  =
        match tryNoun with
        | NOUN x ->
            match tryNext with
            | NOUN y | ADJ y | OBJ y ->
                match verbstr with 
                | "Are" ->  let newfactList = [PREDICATEEXP (Predicate (x,true));
                            PREDICATEEXP (Predicate (y,true))]@prList
                            let newprList = [Predicate (x,true); Predicate (y,true)]@prList1
                            (newfactList, newprList)
                | _ -> let newfactList = [PREDICATEEXP (Predicate (verbstr,true)); 
                                          PREDICATEEXP (Predicate (x,true));
                                          OBJECTEXP (Object y)]@prList
                       let newprList = [Predicate (verbstr,true);Predicate (x,true)]@prList1
                       (newfactList, newprList)
                |> Some
            | _ -> None
        | _ -> None
    ruleCaseRec [] []

/// Function to unwrap on Verb types and to call relevant function base don complexity ///
let unwrapVerbSimple = 
    function
    | VERB(x,y,z) when (x="Is") ->
        simplePredMatcher y z
    | VERB(x,y,z) -> 
        complexPredMatcher (x,true) y z
    | _ -> None

let unwrapVerbFact = 
    function
    | VERB(x,y,z) -> ruleCasePredMatcher x y z 
    | _ -> None      

/// Below set of high level functions mainly for unwrapping parts of Ast, and to produce 
/// GroundTruth list, Object list list and Predicate list ///
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
/// 
/// They are designed to extract sentences to support the features below:
/// Simple inference cases 
/// Conditional radical statements 
/// Union statements 
/// Not statements

/// Function that unwraps on the QUANT type of all rule sentences ///
let genericFactType inp = 
    inp |> List.choose id
        |> returnGrammar unwrapResultTypes
        |> returnGrammar (unwrapQuantType >> fst)

/// Function that matches on all conditional types ///
let findConditional inp = 
    match inp with 
    | COND(_,x,y) -> Some (x,y)
    | _ -> None

/// Function that matches on UNION cases or otherwise passes empty string to indicate simple case ///
let findConjunctive (inp: (Ast*Ast)) = 
    match inp with
    | (UNION(x,y,z),a) -> match x with 
                          | "And" | "Or" -> Some(x,y,z,a) 
                          | _ -> None
    | (x,y) -> Some("", x,x,y)

/// Function that matches on Verb type to find the type of sentences being passed ///
let produceFactPred inp = 
    let simpleFact = genericFactType inp
    (simpleFact)  |> List.map (unwrapVerbFact)
                  |> List.choose id
                //|> List.collect id


/// Below two functions extract relevant information from tuple to create a compatible (WFF*WFF) list ///
let quickFix a =
    match a with
    | hd1::hd2::_ ->
        (hd1,hd2)
    | _ -> failwith "Should not happen"

let produceWFFList (inp:(WFF list * Predicate list) list) = 
    inp |> List.map (quickFix << fst)

/// Function that adds old predicate list to new predicate list from all data types ///
let appendPredList (inp:('a * Predicate list) list) predList = 
    inp |> List.map snd 
        |> List.collect id
        |> List.append predList

/// Function that matches on RAD types and creates a Radical list ///
let radicalMatcher inp radList =
    match inp with
    | RAD x -> 
        (Radical x::radList)
        |> Some
    | _ -> None

/// Function that matches on implied types and creates sentences ready to be analysed for logic conversion ///
let findImply (inp: string*Ast*Ast*Ast) = 
    match inp with 
    | (b,x,y,z) -> match z with 
                       | IMPL(_,a) -> Some(b,x,y,a)
                       | _ -> None             

/// Function that checks whether a not implied statement has been passed here ///
let simpleFactNegateChecker inp boolType newRadlst predRlst = 
    match inp with 
    | NOUN x | ADJ x -> 
                (PREDICATER(Predicate (x,boolType), newRadlst))::predRlst
                |> Some
    | _ -> None

/// Function that tail recursively generates a WFF from simple facts passed ///
let simpleFactMatcher tryRad tryNext  =
    let rec simpleRecF radList predRList = 
        let firstArg = radicalMatcher tryRad radList
        match firstArg with
        | Some(newRadList) -> 
            match tryNext with    
            | NOT(x) -> simpleFactNegateChecker x false newRadList predRList
            | x -> simpleFactNegateChecker x true newRadList predRList
        | None -> None
    simpleRecF [] []

/// Function that calls simpleFactMatcher to extract the WFF ///
let extractRadicalFacts inp = 
    match inp with 
    | VERB(_,y,z) -> simpleFactMatcher y z 
    | _ -> None

/// Function that matches on more complex fact statements containing UNION cases ///
let produceComplexFactPred inp = 
    let complexFact = genericFactType inp
                      |> returnGrammar findConditional
                      |> returnGrammar findConjunctive
                      |> List.map findImply
                      |> List.choose id
    //LangDictionary.print <| complexFact
    let first (x,_,_,_) = x
    let second (_,x,_,_) = x
    let third (_,_,x,_) = x
    let fourth (_,_,_,x) = x

    let firstEle = List.map (second >> extractRadicalFacts) complexFact
                   |> List.choose id
                   |> List.collect id
    
    let secondEle = List.map (third >> extractRadicalFacts) complexFact
                   |> List.choose id
                   |> List.collect id 

    let thirdEle = List.map (fourth >> extractRadicalFacts) complexFact
                   |> List.choose id
                   |> List.collect id 
//    LangDictionary.print <| firstEle
//    LangDictionary.print <| secondEle
//    LangDictionary.print <| thirdEle
    let typeEle = List.map (first) complexFact
    let final = List.zip3 firstEle secondEle thirdEle
    List.zip typeEle final

/// Function that matches on union types and wraps them in customed defined WFF D.U ///
let obtainType (inp: (string*(WFF*WFF*WFF))) = 
    match inp with 
    | ("And",(y,z,a)) ->        
        Some (AND(y,z),a)
    | ("Or",(y,z,a)) -> Some (OR(y,z),a)
    | ("", (y,_,z)) -> Some(y,z)
    | _ -> None

/// Function that matches on AND/OR D.U. types and extracts Predicate information ///
let complexPredList (inp: (WFF*WFF) list) =
    let findType = List.map(fst >> fun x -> match x with 
                                            | AND(PREDICATER(x,_), PREDICATER(y,_)) -> Some(x,y)
                                            | OR(PREDICATER(x,_), PREDICATER(y,_)) -> Some(x,y)
                                            | PREDICATER(x,_) -> Some(x,x)
                                            | _ -> None ) inp
                   |> List.choose id 

    let extractType1 = List.map fst findType
    let extractType2 = List.map snd findType

    let takeSecondPred = List.map (snd >> fun x -> 
                                        match x with 
                                        | PREDICATER (x,_) -> Some x 
                                        | _ -> None) inp     
                        |> List.choose id
    (extractType1 @ extractType2 @ takeSecondPred)


/// Top level function to transform Ast into GroundTruth list * Object list list * Predicate list ///
let getGT (inp: Result<Ast, string> option list) =
    let gtList = inp |> produceGroundTruth
    let objList = inp |> produceObj
    let predList = inp |> producePred
    //LangDictionary.print <| gtList
    (gtList, objList, predList)


/// Top level function to transform Ast into (WFF*WFF) list * Predicate list ///
let getRl (inp: Result<Ast, string> option list) oldPredList =
    let simpleFacts = produceFactPred inp    
    let complexFacts = produceComplexFactPred inp
    let outCFacts = List.map obtainType complexFacts
                    |> List.choose id    
    let outSFacts = produceWFFList simpleFacts
    let factPredList = complexPredList outCFacts
    let newPredList = factPredList @ (appendPredList simpleFacts oldPredList)
                      |> List.distinct
    LangDictionary.print <| (outCFacts @ outSFacts, newPredList)
    (outCFacts @ outSFacts, newPredList)