module FullStack
/// Importing relevant modules

open System
open System
open System.Diagnostics
open LangDictionary
open Parser
open LogicConversion
open Evaluator
//open FsCheck
//open FsCheck
//open FsCheck
open TypesShared

/// Utility print function ///
let print x =
    printfn "%A" <| x
 
/// Provisionally named: SpiderMonkey ///

module helperPackage =
    /// Module based error lookup ---------- ///
    let errorMessageLookup = Map [
        "Dictionary" , "Check and Validate dictionary files"
    ]
    /// Error handler ---------------------- ///
    let criticalErrorHandler (dsg: string) checkVal=
        match checkVal with
        | Some valid ->
            valid
        | None ->
            //print <| sprintf "Critical Error encountered in %s module, please %s" dsg (errorMessageLookup.[dsg])
            failwithf "Critical Error encountered in %s module, please %s" dsg (errorMessageLookup.[dsg])
            

    let generatePlainTextGroundTruth (inpRaw: string list) =
        inpRaw |> List.map (Parser.lexer)
    
    let generatePlainTextFacts (inpPtGroundTruth: string list list) =
        
        inpPtGroundTruth |> List.map (LangDictionary.dictionaryMode2)
                         |> List.map (criticalErrorHandler "Dictionary")
                         |> List.collect (id)
                         |> List.concat
                         |> List.map (Parser.lexer)



type SpiderMonkey() =
    /// Helper function for error propagation ///
    let parseCage inp =
        match inp with
        | Core.Option.Some (Core.Result.Ok(x)) ->
            Core.Option.Some(Core.Result.Ok(x))
        | Core.Option.Some (Core.Result.Error msg) ->
//            print <| "How"
            failwithf "%A" msg
        | _ -> failwithf "Critical Error in Dictionary // Parser interface"
    
    let parseCage2 inp =        
        match inp with
        | Core.Option.Some (Core.Result.Ok(x)) ->
            Core.Option.Some(Core.Result.Ok(x))
        | Core.Option.Some (Core.Result.Error msg) ->
//            print <| "How"
            //print <| msg
            None
        | _ -> failwithf "Critical Error in Dictionary // Parser interface"
        
    
    let checkEmpty lst =
        let ifEmpty x =
            match x with
            | "\"\"" -> Core.Option.None
            | _ -> Some x
        lst |> List.map (ifEmpty)
            |> List.choose id                     
    
    let generateGTLogic (rawString: string list list) =
        rawString |> List.map (checkEmpty)
                  |> List.map (LangDictionary.dictionaryMode1)
                  |> List.map (parseCage)
                  |> Parser.parser
                  |> List.map (Some)
                  |> LogicConversion.getGT
    
    let generateRLLogic (rawString: string list list) =
        rawString |> List.map (LangDictionary.dictionaryMode1)
                  |> List.map (parseCage2)                  
                  |> Parser.parser
                  |> List.map (Some)
                  |> LogicConversion.getRl 
    
    let localEvaluate (gtLogic: GroundTruth list * Object list list * Predicate list)
                      (rlLogic: (Predicate list -> (WFF*WFF) list * Predicate list)) =
        let (gtList,objList,oldPred) = gtLogic
        let (implList, predList) = rlLogic oldPred        
        Evaluator.evaluate gtList objList predList implList
    
    let errorCatcher lst =
        let errorFolder state x =
            match state with
            | Core.Option.Some msg ->
                Core.Option.Some msg
            | Core.Option.None ->
                match x with
                | Some (Core.Result.Ok _) ->
                    Core.Option.None
                | Some (Core.Result.Error msg) ->
                    Core.Option.Some msg
        lst |> List.fold (errorFolder) Core.Option.None
    
    let stripEmpty lst =
        let emptyCheck x =
            match x with
            | hd1::hd2::tl -> Core.Option.Some x
            | _ -> Core.Option.None 
        let optionFolder state qRest =
            match qRest with
            | Some lst ->
                state |> List.append ([lst])
            | None ->
                state
        lst |> List.map (emptyCheck) |> List.fold (optionFolder) []
    
    member x.runSMNoSafety (rawInput: string list) =
        let ptGT = rawInput |> helperPackage.generatePlainTextGroundTruth
        let ptRL = ptGT |> helperPackage.generatePlainTextFacts       
        localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
        |> List.choose id
        |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
                  
    member x.runSMSafe (rawInput: string list) =
        try 
            let ptGT = rawInput |> helperPackage.generatePlainTextGroundTruth
            let ptRL = ptGT |> helperPackage.generatePlainTextFacts |> stripEmpty
            
            //print <| localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
            //print <| localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
            match ptGT |> List.map (checkEmpty) |> List.map (LangDictionary.dictionaryMode1) |> errorCatcher with                
            | Some msg ->
                Error msg
            | _ -> 
                localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
                
                |> List.choose id
                |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
                |> List.map (fun x ->
                                match x with
                                | Some y ->
                                    y |> List.fold (fun state inp ->
                                                state + inp + " "
                                                ) ("")
                                | _ -> "Error")
                |> Ok
        with
            | msg ->
                Error ("Error in run")
    
    member x.adHocInference (gTInput: string list) (factsInput: string list) =
        try 
            let ptGT = gTInput |> helperPackage.generatePlainTextGroundTruth
            let ptRL = factsInput |> helperPackage.generatePlainTextGroundTruth
            match ptGT |> List.map (checkEmpty) |> List.map (LangDictionary.dictionaryMode1) |> errorCatcher with
            | Some msg ->
                Error msg
            | None ->
                match ptRL |> List.map (checkEmpty) |> List.map (LangDictionary.dictionaryMode1) |> errorCatcher with
                | Some msg ->
                    Error msg
                | None ->
                    localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
                    |> List.choose id
                    |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
                    |> List.map (fun x ->
                                match x with
                                | Some y ->
                                    y |> List.fold (fun state inp ->
                                                state + inp + " "
                                                ) ("")
                                | _ -> "Error")
                    |> Ok
        with
            | msg ->
                Error ("Error message")
            
type SideWinder() =
    let parseCage inp =
        match inp with
        | Some (Ok(x)) ->
            Some (Ok(x))
        | Some (Error msg) -> Some (Error msg)
        | _ ->

            failwithf "Critical Error in Dictionary // Parser interface"
    /// For parsing based errors ///
    let getDictLookup (rawInput: string) =
        let gtSplit = helperPackage.generatePlainTextGroundTruth [rawInput]
        match gtSplit with
        | hd::tl ->
            hd |> LangDictionary.dictionaryMode1
               |> parseCage
        | _ ->

            failwithf "Critical Error in Dictionary"
    
    let parseInput lexed =
        match lexed with
        | Some (Ok x) ->
            match [(Some (Ok x))] |> Parser.parser with
            | hd::tl ->
                match hd with
                | Error msg ->
                    Error msg
                | Ok ast -> Ok (sprintf "%A" ast)
            | _ -> Error "Critical error in dictionary parser interface"
        | Some (Error msg) ->
            Error msg
        | _ -> Error "Critical error in dictionary parser interface"
    
    let getStrTyp inpTok=
        match inpTok with
        | N value ->
            sprintf "%s (Noun)" value
        | Q value ->
            sprintf "%s (Quantifier)" value
        | QR value ->
            sprintf "%s (Radical Quantifier)" value
        | A value ->
            sprintf "%s (Adjective)" value
        | C value ->
            sprintf "%s (Conditional)" value
        | R value ->
            sprintf "%s (Radical)" value
        | V value ->
            sprintf "%s (Verb)" value
        | I value ->
            sprintf "%s (Implication)" value
        | O value ->
            sprintf "%s (Object)" value
        | U value ->
            sprintf "%s (Union)" value
        | _ -> sprintf "Unknown type"
    member x.ParseCheck (line: string) =
        try 
            line |> getDictLookup
                 |> parseInput
        with
            | msg ->
                Error "Failed parse"
    member x.getLex (line: string) =
        try
            match line |> getDictLookup with
            | Some (Error msg) ->
                Error msg
            | Some (Ok x) ->                
                let typesList =
                    x |> List.map (getStrTyp)               
                let descFinder = LangDictionary.dictionaryFileInterface()
                let descList =
                    line |> Parser.lexer |> List.map (descFinder.getDesc)                
                let amalgamateErrors vals =
                    match vals with
                    | Ok desc ->
                        desc
                    | Error msg ->
                        msg
                let outList = 
                    descList |> List.map (amalgamateErrors)               
                Ok (typesList, outList)
            | None ->
                Error "Critical"
        with
            |  msg ->
                Error "Error found"
                
module frontEndIntegration=
    type BackEndInterface() =
        
        let resultUnwrapStr inp =
            match inp with
            | Ok x ->
                sprintf "%A" x
            | Error x ->
                sprintf "An error was encountered: %A" x 
        
        let resultUnwrapLst inp =
            match inp with
            | Ok lst ->
                lst , true
            | Error x ->
                [x] , false
        
        member x.getInferences (rawInput: string list)=
            let SM = SpiderMonkey()
            let InferenceList =
                SM.runSMSafe(rawInput)
            let data, res = resultUnwrapLst InferenceList
            (data |> List.fold (fun x y-> x + y + "\n") "", res)
            
        
        member x.getWordTypes (rawInput: string list)=
            let SW = SideWinder()
            let TypesList =
                rawInput |> List.map (SW.getLex)
            let LexedOut inp =
                match inp with
                | Ok(x,_) ->
                    sprintf "%A" x
                | Error y ->
                    y
            TypesList |> List.map (LexedOut)
        
        member x.getParseResult (rawInput: string list) =
            let SW = SideWinder()
            let TypesList =
                rawInput |> List.map (SW.getLex)
            let LexedOut inp =
                match inp with
                | Ok(_,x) ->
                    sprintf "%A" x
                | Error y ->
                    y
            TypesList |> List.map (LexedOut)
                
        member x.getAdHocResult (rawRules: string list) (rawWorld: string list) =
            let SM = SpiderMonkey()
            let InferenceList =
                SM.adHocInference rawRules rawWorld
            let data, res = resultUnwrapLst InferenceList
            (data |> List.fold (fun x y-> x + y + "\n") "", res)