module LangDictionary 
open System
open System.IO
open System.Text
open TypesShared
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Patterns
open Fable.Import.Browser
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Fable
open Fable.Import.Node
open Fable.Import.Node
open Html
open Node.Exports

/// Print function --------- ///
let print x =
    printfn "%A" x  

module internalLexer =
    
    /// Lex Pattern for words
    let Word =
        [['0'..'9']@['a'..'z']@['A'..'Z']@['-']@['\''],true
         ]
    
    /// Lex pattern for Word Types
    let TypeNorm =
        [['a'..'z']@[' ']@['—'],true
         ['.'],false]
    
    /// Lex pattern for encylopedia headers
    let headerTab =
        [
            ['/']@[' '], true
            ['A'; 'C'], false
            ['l'], false
            ['i'; 'a'], false
            ['a'; 's'], false
            ['s'], false
            [':'], false
            [' '], false
        ]
    
    /// Lexer, using patterns to lex different entires in dictionary    
    let lexNGram (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option =
        let takeIfInChars chars (tokL,inL) : (char list * char list) option =
            match inL with
            | hd::tl -> match chars |>  List.tryFind (fun x -> x = hd) with
                        | Some valid_char -> Some ((tokL |> List.rev |> List.append [valid_char] |> List.rev) , tl)
                        | Core.Option.None -> Core.Option.None
            | [] -> Core.Option.None
            
        let rec takeWhileInChars (chars: char list) (tokL,inL) =
            match inL with
            | hd::tl -> match chars |> List.tryFind (fun x -> x = hd) with
                            | Some valid_char ->
                                let tape_1 = tokL |> List.rev |> List.append [valid_char] |> List.rev
                                Core.Option.orElse (Some (tape_1, tl)) (takeWhileInChars chars (tape_1, tl))
                            | Core.Option.None -> Core.Option.None
            | [] -> Core.Option.None
                    
        let tryMatch state (charsLst,canRepeat) =
            match state with
            | Some state_U ->
                if canRepeat
                then 
                     takeWhileInChars charsLst state_U
                else takeIfInChars charsLst state_U
            | Core.Option.None -> Core.Option.None
        ngram |> List.fold (tryMatch) (Some ([],cLst))
    
    /// Type definition for Lexer    
    type Lexer = char list -> (char list * char list) option
    
    /// Lexer combinator
    let (<|>) lex1 lex2 =
        fun clst ->
        Core.Option.orElse (Core.Option.orElse Core.Option.None (lex2 clst)) (lex1 clst)
    
    /// Lexer combinator
    let (>=>) (lex1 : Lexer) (lex2 : Lexer) =
        fun clst ->
            let First= lex1 clst
            match First with
            | Some (x,y) -> let Second= lex2 y
                            match Second with
                            | Some (w,z) -> Some (List.append x w, z)                                            
                            | Core.Option.None -> Core.Option.None
            | Core.Option.None -> Core.Option.None
    
    /// Top level word lexing
    let lexWord = lexNGram Word
    
    /// Top level function for Type lexing
    let lexType = lexNGram TypeNorm
    
    /// Top level function for Word & Type lexing
    let lexWordType = lexWord >=> lexType 
    
    /// Top level function for Header lexing
    let lexHeaderpt1 = lexNGram headerTab
    
    /// Sequence -> List conversion + function call for Lexing Words
    let lexWordString x =
        lexWord (x |> Seq.toList)
    
    /// Sequence -> List conversion + function call for Lexing Words and types
    let lexWordTypeString x =
        lexWordType (x |> Seq.toList)
    
    /// Sequence -> List conversion + function call for Lexing headers
    let lexWordHeader x =
        let first = lexHeaderpt1 (x |> Seq.toList)
        match first with
        | Some (header, restWord) ->
            lexWord restWord
        | Core.Option.None -> Core.Option.None
            
/// Utility toolbox -------------- ///
module utilityTools =
    
    /// Replacement function for IsUpper (Missing from Fable)
    let IsUpper ch =
        let charToInt c = int c - int 'A'
        if charToInt ch >= 0 && charToInt ch < 26
        then true
        else false
    
    /// Utility function to check if strings lists have empty entries, then strip them out
    let checkEmpty lst =
        let ifEmpty x =
            match x with
            | "" -> Core.Option.None
            | _ -> Some x
        lst |> List.map (ifEmpty)
            |> List.choose id
    
    /// Replacement function for ToUpper (Missing from Fable)
    let ToUpper ch =
        let charToInt c = int c - int 'a'
        //print <| (charToInt ch + 65)
        char (charToInt ch + 65)
    
    /// Utility function to convert char list to string    
    let convertToString lst =    
        String.concat "" <| List.map string lst
    
    /// Utility function to turn null types to None types
    let removeNull wordWithToken =    
        match wordWithToken with
        | NULL _ -> Core.Option.None 
        | other -> Some (other)
    
    /// Checks if starting letter is capital, if not converts it to capital
    let checkCap word =       
        match (word |> Seq.toList) with
        | hd::tl ->
            if hd |> IsUpper
            then hd::tl
            else (ToUpper hd)::tl
        | _ ->
            print <| "Here"
            failwith "Empty string entered, please check input"
    
    /// Specials list to match on special phrases
    let specialsList = Map [    
        "A", NULL "A"
        "All-!X", QR "ALL !X"
        "All-!Y", QR "All !Y"
        "All-!Z", QR "All !Z"
        "F#", O "F#"
    ]
   
    /// Buffer handling function to convert fileBuffer -> string
    let bufferHandler (fileData: Node.Buffer.Buffer) =
        fileData.toString("utf8")

        
/// File interface ------------- ///
type dictionaryFileInterface()=
    
    /// Utility function for convering characters -> integers
    let charToInt c = int c - int '0'            
    
    /// Calculates appropriate file names for searching in file structure    
    let locator word =                               
        let charLst = word |> Seq.toList             
        match charLst with
        | firstLetter::tl ->
            match tl with
            | secondLetter::_ ->
                Some (string firstLetter,
                      (string firstLetter) + (string (utilityTools.ToUpper secondLetter)))
            | _ ->
                if firstLetter = 'A'
                then Some ("A", "AA")
                else Core.Option.None           
        | _ -> Core.Option.None   
        
    /// Reads filename given to it with relative directory structure (Electron version)
    let readFile y =
       Fable.Import.Node.Exports.Fs.readFileSync (y, (fun err data ->                
            ()
        )) |> utilityTools.bufferHandler        
        
    /// Builds file name according to word, calls the readFile function
    let getFile firstChar firstSecondChar =        
        let m = Fable.Import.Node.Exports.Path.resolve(Globals.__dirname, ("Dictionary/" + firstChar + "/" + firstSecondChar + ".dict"))
        let str = m    
        readFile str
    
    /// Converts plaintext type in dictionary into (string -> Token types) for parser
    let getType = Map [            
        'n' , N    
        'v' , V
        'j' , A
        'q' , Q
        'c' , C
        'i' , I
        'x' , NULL
        'u' , U
        'p', Not
    ]
    
    /// Checks and submits Map data if available for types
    let identify b =                    
        match Map.containsKey b getType with
        | true -> Some getType.[b]
        | false -> Core.Option.None
    
    /// Searches through file data for word and type
    let fileSearcher capWord ((wordType): (string->Token) option) dictionaryLine=    
        match wordType with
        | Some l -> Some l
        | Core.Option.None ->
            let matchWord =
                match internalLexer.lexWordString dictionaryLine with
                | Some charList ->
                    utilityTools.convertToString (fst charList)
                | Core.Option.None ->
                    dictionaryLine
            if dictionaryLine |> String.length = 0
            then wordType
            elif matchWord = capWord
            then
                let matchType =
                    match internalLexer.lexWordTypeString dictionaryLine with
                    | Some charList -> fst charList
                    | Core.Option.None ->
                        []                         
                match (matchType |> List.rev) with 
                | _::typeChar::_ ->
                    identify typeChar
                | _ ->
                    Core.Option.None
            else Core.Option.None
    
    /// Searches dictionary data for a valid description of the word
    let descSearcher capWord ((wordDesc): (string) option) dictionaryLine =
        match wordDesc with
        | Some l -> Some l
        | Core.Option.None ->
            let matchWord =
                match internalLexer.lexWordString dictionaryLine with
                | Some charList ->
                    utilityTools.convertToString (fst charList)
                | Core.Option.None ->
                    "__NULL"
            if matchWord = capWord
            then
                Some dictionaryLine
            else Core.Option.None
    
    /// Gets token for lookup word and propogates errors if found
    let rec tryFindExt lookupWord =        
        let capWord = (utilityTools.checkCap lookupWord) |> utilityTools.convertToString
        if Map.containsKey capWord utilityTools.specialsList
        then            
            Core.Result.Ok <| utilityTools.specialsList.[capWord]                
        else
            match locator capWord with
            | Some (firstChar,firstSecondChar) ->
                let data = getFile firstChar firstSecondChar
                let splitLine = (fun (line: string) -> Seq.toList (line.Split '\n'))
                let delimit = (data) |> splitLine
                match delimit |> List.fold (fileSearcher capWord) (Core.Option.None) with
                    | Core.Option.None ->                        
                        if (utilityTools.checkCap lookupWord) |> utilityTools.convertToString = lookupWord
                        then Core.Result.Ok (O lookupWord)
                        else
                            let piecewiseWord = lookupWord |> Seq.toList |> List.rev                            
                            match piecewiseWord with
                            | hd::tl ->
                                if hd = 's'
                                then
                                    tryFindExt (tl |> List.rev |> utilityTools.convertToString)
                                else
                                    Core.Result.Error <| sprintf "Could not find word: %A in dictionary" lookupWord
                            | _ ->
                                Core.Result.Error <| sprintf "Could not find word: %A in dictionary" lookupWord
                    | Some wordType ->
                        Core.Result.Ok <| wordType capWord
            | Core.Option.None ->
                Core.Result.Error <| sprintf "Input word: %A too short" lookupWord
    
    /// Tries to find descriptions for words being input, propogates error if not found
    let rec tryFindDesc lookupWord =
        let capWord = (utilityTools.checkCap lookupWord) |> utilityTools.convertToString
        if Map.containsKey capWord utilityTools.specialsList
        then
            if capWord = "A"
            then
                Core.Result.Ok ("a: determiner")
            else
                Core.Result.Ok (sprintf "%s is a Radical Quantifier" capWord)
        else
            match locator capWord with
            | Some (firstChar,firstSecondChar) ->
                let data = getFile firstChar firstSecondChar
                let splitLine = (fun (line: string) -> Seq.toList (line.Split '\n'))                
                let removeReturn (state: string) (inp) =
                    if inp = '\r'
                    then
                        state
                    else
                        state + (utilityTools.convertToString [inp])
                let delimit = (data)  |> splitLine |> List.map (fun x -> x |> Seq.toList |> List.fold (removeReturn) "")                
                match delimit |> List.fold (descSearcher capWord) (Core.Option.None) with
                    | Core.Option.None ->
                        if (utilityTools.checkCap lookupWord) |> utilityTools.convertToString = lookupWord
                        then Core.Result.Ok (sprintf "%s is a proper noun" capWord)
                        else
                            let piecewiseWord = lookupWord |> Seq.toList |> List.rev                            
                            match piecewiseWord with
                            | hd::tl ->
                                if hd = 's'
                                then
                                    match tryFindDesc (tl |> List.rev |> utilityTools.convertToString) with
                                    | Core.Result.Ok y-> Core.Result.Ok (y + "This is a plural")
                                    | Core.Result.Error _ -> Core.Result.Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                                else Core.Result.Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                            | _ -> Core.Result.Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                    | Some desc ->
                        Core.Result.Ok desc
            | Core.Option.None -> Core.Result.Error (sprintf "Input word: %A too short" lookupWord)
    
    /// Wrapper function for word lookup   
    member x.findWord2 lookupWord =    
        let p = lookupWord |> Seq.toList
        match p with
        | hd::tl ->
            if hd = '<'
            then Core.Result.Ok <| V (lookupWord)
            elif hd = '{'
            then Core.Result.Ok <| N (lookupWord)
            elif hd = '!'
            then Core.Result.Ok <| R (lookupWord)
            else tryFindExt lookupWord
        |_ ->
            Core.Result.Error "Input is empty or additional space input" 
    
    /// Wrapper function for Description lookup            
    member x.getDesc lookupWord =
        let p = lookupWord |> Seq.toList
        match p with
        | hd::tl ->
            if hd = '<'
            then Core.Result.Ok (sprintf "%s is a Custom Verb" lookupWord)
            elif hd = '{'
            then Core.Result.Ok (sprintf "%s is a Custom Noun" lookupWord)
            elif hd = '!'
            then Core.Result.Ok (sprintf "%s is a Radical variable" lookupWord)
            else tryFindDesc lookupWord
        |_ ->
            failwith "Empty input" 
    

/// Knowledge bank -----------  ///
type knowledgeBankFileInterface()=
    
    /// Reads filename given to it with relative directory structure 
    let readFile y =
        try
            Fable.Import.Node.Exports.Fs.readFileSync (y, (fun err data ->                   
                ()
            )) |> utilityTools.bufferHandler            
        with
            | exn ->
                "__NULL"       /// Simple error handling, due to Fable restrictions
    
    /// Builds file name according to word form
    let getFile2 inp =
        try 
            let m = Fable.Import.Node.Exports.Path.resolve(Globals.__dirname, inp)
            let str = m    
            readFile str
        with
            | exn ->
                "__NULL"
    
    /// Builds the file name from the word and gets alias and parent types
    let getFile firstChar word =                    
        let str = "Dictionary/" + firstChar + "/ENC/" + word + ".dict"      
        let data = getFile2 str
        if data = "__NULL"
        then []
        else
            let splitLine = (fun (line: string) -> Seq.toList (line.Split '\n'))
            let removeReturn (state: string) (inp) =
                    if inp = '\r'
                    then
                        state
                    else
                        state + (utilityTools.convertToString [inp])
            let delimit = (data)  |> splitLine |> List.map (fun x -> x |> Seq.toList |> List.fold (removeReturn) "")            
            match delimit with
            | alias::parent::tl ->            
                let aliasLex =                        /// Extension material: //TODO: Implement alias searching
                    match internalLexer.lexWordHeader alias with
                        | Some x ->
                            Some (utilityTools.convertToString (fst x))
                        | Core.Option.None -> Core.Option.None
                let parentLex =                        /// Potential extension material --> Implemented
                    match internalLexer.lexWordHeader parent with
                    | Core.Option.Some x ->                        
                        Core.Option.Some (utilityTools.convertToString (fst x))
                    | Core.Option.None -> Core.Option.None
                match parentLex with
                | Core.Option.Some parentClass ->
                    let addedRules =
                        let baseList = tl |> List.append [(sprintf "All %s are a %s" word parentClass)]
                        let str1 = "Dictionary/" + "CLASSENC/" + parentClass + ".dict"
                        let data = getFile2 str1
                        if data = "__NULL"
                        then                            
                            baseList
                        else                            
                            let splitLine = (fun (line: string) -> Seq.toList (line.Split '\n'))
                            let removeReturn (state: string) (inp) =
                                    if inp = '\r'
                                    then
                                        state
                                    else
                                        state + (utilityTools.convertToString [inp])
                            let delimit = (data)  |> splitLine |> List.map (fun x -> x |> Seq.toList |> List.fold (removeReturn) "")                            
                            baseList |> List.append delimit
                    addedRules
                | Core.Option.None ->
                    tl                                   
            | _ -> []
        
    /// Wrapper function for finding rule files
    member x.FindFile t =                
        let p = (t |> Seq.toList)
        match p with
        | hd::_ ->
            let firstChar = string hd
            getFile firstChar t 
        | _ -> failwith "No lead character"

/// Reconstruction module ---- ///
module Petrichor =    
    
    /// Generates map for each groundtruth/ Rule
    type dictGenerator() =            
        
        /// Turns 2 word pair into searchable key-elements in map
        let twoWordSeq originalMap (word1,word2) =        
            Map.add word1 word2 originalMap   
        
        /// Wrapper function to return reconstruction dictionary
        member x.getDict (gtList: string list list) =        
            let wrapList lst =                                /// Offsets and wraps list with itself
                match lst with
                | hd::tl ->
                    (tl |> List.map (Some)) @ [Core.Option.None]   
                | _ -> failwith ("Empty lists")
            
            let pairWrap (x,y)  =                              /// unwraps option type
                match y with
                | Some p -> Some (x,p)
                | Core.Option.None -> Core.Option.None
            
            let capList = gtList |> List.map (List.map (fun x -> utilityTools.convertToString (utilityTools.checkCap x)))
            capList |> List.map (wrapList)
                   |> List.zip capList
                   |> List.map (fun (x,y) -> (List.zip x y |> List.map (pairWrap)))
                   |> List.map (List.choose id)
                   |> List.map (List.fold (twoWordSeq) (Map []) ) 
    
    let unwrapObj (obj:Object) =    /// Simple DU unwrapper
            match obj with
            | Object str -> str
            
    let unwrapPred (pre:Predicate) =    /// Simple DU unwrapper
        match pre with
        | Predicate (str, negation) -> (str, negation) 
    
    /// Uses List.Fold and Option state to search through map
    let findKey (obj: string) (dictList: Map<string, string> list) =    
            let folderKey state mp =
                match state with
                | Some x -> Some x
                | Core.Option.None ->
                    Map.tryFind obj mp             
            dictList |> List.fold (folderKey) (Core.Option.None)
       
    /// Top level function for attaining the connectors for a given logic statement
    let getConnectors (objList, pred) plainTextGT plainTextRL=
        let gen = dictGenerator()
        let lookupGT = gen.getDict plainTextGT
        let lookupRL = gen.getDict plainTextRL       
        
        let formatSelector predicate lst =
            let str, negation = unwrapPred predicate
            match lst with
            | t1::t2::_ ->
                match findKey (str) lookupRL with
                | Some connector ->
                    if negation
                    then [unwrapObj t1; (str) + "s"; connector; unwrapObj t2]
                    else [unwrapObj t1; "Doesn't"; str; connector; unwrapObj t2]
                | Core.Option.None ->
                    if negation 
                    then [unwrapObj t1; str + "s"; unwrapObj t2]
                    else [unwrapObj t1; "Doesn't"; str ; unwrapObj t2]
            | t1::_ ->
                match findKey (unwrapObj t1) lookupGT with
                | Some connector ->
                    if negation
                    then [unwrapObj t1; connector; str]
                    else [unwrapObj t1; connector; "Not"; str]
                | Core.Option.None ->
                    [unwrapObj t1; "Not";  str]
            | _ -> ["Failed Inference"]
        objList |> (formatSelector pred)                  
          
/// Interface functions ---------- ///

/// Interface function for Mode 1 usage of dictionary 
let dictionaryMode1 (x: string list) =                
    let Dict = dictionaryFileInterface()              /// Class initialisation of dictionary file interface
    let errorCatcher lst =                            /// For the purposes of error propogation
        let errorFolder state x =
            match state with
            | Core.Option.Some msg ->
                Core.Option.Some msg
            | Core.Option.None ->
                match x with
                | Core.Result.Ok _ ->
                    Core.Option.None
                | Core.Result.Error msg ->
                    Core.Option.Some msg
        lst |> List.fold (errorFolder) Core.Option.None        
    try
        let tryFind = x |> List.map (Dict.findWord2)
        match errorCatcher tryFind with
          | Core.Option.Some msg ->
              Core.Option.Some <| Core.Result.Error msg
          | Core.Option.None ->
              tryFind |> List.map (fun x ->
                        match x with
                        | Core.Result.Ok x -> x
                        | _ -> failwithf "Critical Error")
                      |> List.map (utilityTools.removeNull)
                      |> List.choose id
                      |> Core.Result.Ok
                      |> Core.Option.Some
          
    with
        | msg ->
             /// Mode 1 allows error propagation back to user
            Some <| Core.Result.Error "Error check console"

/// Interface function for Mode 2 usage of dictionary
let dictionaryMode2 (x: string list) =                
    try
        let know = knowledgeBankFileInterface()
        let p r = know.FindFile r
        x |> List.map p |> Core.Option.Some
    with
    | ex ->                                 /// No meaningful error propagation, since most words won't look up
        Core.Option.None

/// Interface function for Mode 3 usage of dictionary
let dictionaryMode3 inp plaintextGTList plaintextRLList =        
    try
        let checkEmpty2 lst =
            match lst with
            | hd::hd2::tl ->
                Core.Option.Some lst
            | _ ->                
                Core.Option.None
        let correctedRL =
            plaintextRLList
            |> List.map (checkEmpty2)
            |> List.choose (id)
        match inp with
        | Core.Result.Ok objPred ->
            Petrichor.getConnectors (snd objPred, fst objPred) plaintextGTList correctedRL
        | Core.Result.Error msg ->
            failwith msg
        |> Some
    with
    | msg ->
        Core.Option.None                /// No meaningful error propagation, since errors represent issues which would
                                        /// have been flagged by Mode 1
