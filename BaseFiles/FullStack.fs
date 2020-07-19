module FullStack
/// Importing relevant modules

open System
open System
open System.Diagnostics
open LangDictionary
open Parser
open LogicConversion
open Evaluator
open FsCheck
open FsCheck
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
        | None -> failwithf "Critical Error encountered in %s module, please %s" dsg (errorMessageLookup.[dsg])

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
        | Some (Ok(x)) ->
            Some(Ok(x))
        | Some (Error msg) -> failwithf "%A" msg
        | _ -> failwithf "Critical Error in Dictionary // Parser interface"
    
                         
    
    let generateGTLogic (rawString: string list list) =
        rawString |> List.map (LangDictionary.dictionaryMode1)
                  |> List.map (parseCage)
                  |> Parser.parser
                  |> List.map (Some)
                  |> LogicConversion.getGT
    
    let generateRLLogic (rawString: string list list) =
        rawString |> List.map (LangDictionary.dictionaryMode1)
                  |> List.map (parseCage)
                  |> Parser.parser
                  |> List.map (Some)
                  |> LogicConversion.getRl 
    
    let localEvaluate (gtLogic: GroundTruth list * Object list list * Predicate list)
                      (rlLogic: (Predicate list -> (WFF*WFF) list * Predicate list)) =
        let (gtList,objList,oldPred) = gtLogic
        let (implList, predList) = rlLogic oldPred        
        Evaluator.evaluate gtList objList predList implList
    
    member x.runSMNoSafety (rawInput: string list) =
        let ptGT = rawInput |> helperPackage.generatePlainTextGroundTruth
        let ptRL = ptGT |> helperPackage.generatePlainTextFacts       
        localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
        |> List.choose id
        |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
                  
    member x.runSMSafe (rawInput: string list) =
        try 
            let ptGT = rawInput |> helperPackage.generatePlainTextGroundTruth
            let ptRL = ptGT |> helperPackage.generatePlainTextFacts       
            localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
            |> List.choose id
            |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
            |> Ok
        with
            | Failure msg ->
                Error (msg)
    
    member x.adHocInference (gTInput: string list) (factsInput: string list) =
        try 
            let ptGT = gTInput |> helperPackage.generatePlainTextGroundTruth
            let ptRL = factsInput |> helperPackage.generatePlainTextGroundTruth
            localEvaluate (generateGTLogic ptGT) (generateRLLogic ptRL)
            |> List.choose id
            |> List.map (fun x -> LangDictionary.dictionaryMode3 x ptGT ptRL)
            |> Ok
        with
            | Failure msg ->
                Error (msg)
            
type SideWinder() =
    let parseCage inp =
        match inp with
        | Some (Ok(x)) ->
            Some (Ok(x))
        | Some (Error msg) -> Some (Error msg)
        | _ -> failwithf "Critical Error in Dictionary // Parser interface"
    /// For parsing based errors ///
    let getDictLookup (rawInput: string) =
        let gtSplit = helperPackage.generatePlainTextGroundTruth [rawInput]
        match gtSplit with
        | hd::tl ->
            hd |> LangDictionary.dictionaryMode1
               |> parseCage
        | _ -> failwithf "Critcal Error in Dictionary"
    
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
            sprintf "Noun: %s" value
        | Q value ->
            sprintf "Quantifier: %s" value
        | QR value ->
            sprintf "Radical Quantifier: %s" value
        | A value ->
            sprintf "Adjective: %s" value
        | C value ->
            sprintf "Conditional: %s" value
        | R value ->
            sprintf "Radical: %s" value
        | V value ->
            sprintf "Verb: %s" value
        | I value ->
            sprintf "Implication: %s" value
        | O value ->
            sprintf "Object: %s" value
        | U value ->
            sprintf "Union: %s" value
        | _ -> sprintf "Unknown type"
    member x.ParseCheck (line: string) =
        try 
            line |> getDictLookup
                 |> parseInput
        with
            | Failure msg ->
                Error msg
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
                let outList = descList |> List.map (amalgamateErrors)
                Ok (typesList, outList)
            | None ->
                Error "Critical"
        with
            | Failure msg ->
                Error msg
                
    