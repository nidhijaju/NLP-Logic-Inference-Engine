module LangDictionary 
open System
open System.IO
open System.Text
open TypesShared


/// Print function --------- ///
let print x =
    printfn "%A" x  

module internalLexer =
    
    let Word =
        [['0'..'9']@['a'..'z']@['A'..'Z']@['-']@['\''],true
         ]
    
    let TypeNorm =
        [['a'..'z']@[' ']@['—'],true
         ['.'],false]
    
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
    
        
    let lexNGram (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option =
        let takeIfInChars chars (tokL,inL) : (char list * char list) option =
            match inL with
            | hd::tl -> match chars |>  List.tryFind (fun x -> x = hd) with
                        | Some valid_char -> Some ((tokL |> List.rev |> List.append [valid_char] |> List.rev) , tl)
                        | None -> None
            | [] -> None
            
        let rec takeWhileInChars (chars: char list) (tokL,inL) =
            match inL with
            | hd::tl -> match chars |> List.tryFind (fun x -> x = hd) with
                            | Some valid_char ->
                                let tape_1 = tokL |> List.rev |> List.append [valid_char] |> List.rev
                                Option.orElse (Some (tape_1, tl)) (takeWhileInChars chars (tape_1, tl))
                            | None -> None
            | [] -> None
                    
        let tryMatch state (charsLst,canRepeat) =
            match state with
            | Some state_U ->
                if canRepeat
                then 
                     takeWhileInChars charsLst state_U
                else takeIfInChars charsLst state_U
            | None -> None 
        ngram |> List.fold (tryMatch) (Some ([],cLst))
        
    type Lexer = char list -> (char list * char list) option 
    let (<|>) lex1 lex2 =
        fun clst ->
        Option.orElse (Option.orElse None (lex2 clst)) (lex1 clst)

    let (>=>) (lex1 : Lexer) (lex2 : Lexer) =
        fun clst ->
            let First= lex1 clst
            match First with
            | Some (x,y) -> let Second= lex2 y
                            match Second with
                            | Some (w,z) -> Some (List.append x w, z)                                            
                            | None -> None
            | None -> None

    let lexWord = lexNGram Word

    let lexType = lexNGram TypeNorm

    let lexWordType = lexWord >=> lexType 
    
    let lexHeaderpt1 = lexNGram headerTab
    
    let lexWordString x =
        lexWord (x |> Seq.toList)

    let lexWordTypeString x =
        lexWordType (x |> Seq.toList)
    
    let lexWordHeader x =
        let first = lexHeaderpt1 (x |> Seq.toList)
        match first with
        | Some (header, restWord) ->
            lexWord restWord
        | None -> None
            
/// Utility toolbox -------------- ///
module utilityTools =
    let convertToString lst =    /// Utility function to convert char list to string
        String.concat "" <| List.map string lst     
    let removeNull wordWithToken =    /// Utility function to turn null types to None types
        match wordWithToken with
        | NULL _ -> None
        | other -> Some (other)
    let checkCap word =       /// Checks if starting letter is capital, if not converts it to capital
        match (word |> Seq.toList) with
        | hd::tl ->
            if hd |> Char.IsUpper
            then hd::tl
            else (Char.ToUpper hd)::tl
        | _ -> failwith "Empty string entered, please check input"
    
    let specialsList = Map [    /// Specials list to match on special phrases
        "A", NULL "A"
        "All-!X", QR "ALL !X"
        "All-!Y", QR "All !Y"
        "All-!Z", QR "All !Z"
    ]

/// File interface ------------- ///
type dictionaryFileInterface()=   
    
    let registerCoding =                        /// Fallback encoding in-case UTF doesn't work
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
        Encoding.GetEncoding(1252)
      
    let charToInt c = int c - int '0'            
        
    let locator word =                               /// Calculates appropriate file names for searching in file structure
        let charLst = word |> Seq.toList             /// Converts string to char list that can be indexed 
        match charLst with
        | firstLetter::tl ->
            match tl with
            | secondLetter::_ ->
                Some (Char.ToString firstLetter,
                      (Char.ToString firstLetter) + (Char.ToString (Char.ToUpper secondLetter)))
            | _ ->
                if firstLetter = 'A'
                then Some ("A", "AA")
                else None                
        | _ -> None
        
    let readFile y =                        /// Reads filename given to it with relative directory structure
        let enc1252 = registerCoding
        File.ReadLines (y, enc1252) |> Seq.toList
    
    let getFile firstChar firstSecondChar =        /// Builds file name according to word form
        let str = "Dictionary\\" + firstChar + "\\" + firstSecondChar + ".dict"
        readFile str
    
    let getType = Map [            /// Maps the dictionary word type characters into Tokens for Parser
        'n' , N    
        'v' , V
        'j' , A
        'q' , Q
        'c' , C
        'i' , I
        'x' , NULL
        'u' , U
    ]
    
    let identify b =                    /// Checks and submits Map data if available for types
        match Map.containsKey b getType with
        | true -> Some getType.[b]
        | false -> None
    
    let fileSearcher capWord ((wordType): (string->Token) option) inputWord=    /// Searches through file for word and type
        match wordType with
        | Some l -> Some l
        | None ->
            let matchWord =
                match internalLexer.lexWordString inputWord with
                | Some charList ->
                    utilityTools.convertToString (fst charList)
                | None -> failwithf "The desc.: %A could not be lexed for a word, please validate dictionary files" inputWord
            if matchWord = capWord
            then
                let matchType =
                    match internalLexer.lexWordTypeString inputWord with
                    | Some charList -> fst charList
                    | None -> failwithf "The desc.: %A could not be lexed for a type, please validate dictionary files" inputWord                            
                match (matchType |> List.rev) with 
                | _::typeChar::_ ->
                    identify typeChar
                | _ -> failwithf "The desc.: %A did not contian a valid type format, please validate dictionary files" inputWord
            else None
    
    let descSearcher capWord ((wordDesc): (string) option) inputWord =
        match wordDesc with
        | Some l -> Some l
        | None ->
            let matchWord =
                match internalLexer.lexWordString inputWord with
                | Some charList ->
                    utilityTools.convertToString (fst charList)
                | None -> failwithf "The desc.: %A could not be lexed for a word, please validate dictionary files" inputWord
            if matchWord = capWord
            then
                Some inputWord
            else None
    
    
    let rec tryFindExt lookupWord =        /// Gets token for lookup word and throws exception if not found
        let capWord = (utilityTools.checkCap lookupWord) |> utilityTools.convertToString
        if Map.containsKey capWord utilityTools.specialsList
        then  utilityTools.specialsList.[capWord]
        else
            match locator capWord with
            | Some (firstChar,firstSecondChar) ->
                let data = getFile firstChar firstSecondChar
                match data |> List.fold (fileSearcher capWord) (None) with
                    | None ->
                        if (utilityTools.checkCap lookupWord) |> utilityTools.convertToString = lookupWord
                        then O lookupWord
                        else
                            let piecewiseWord = lookupWord |> Seq.toList |> List.rev                            
                            match piecewiseWord with
                            | hd::tl ->
                                if hd = 's'
                                then
                                    tryFindExt (tl |> List.rev |> utilityTools.convertToString)
                                else failwithf "Could not find word: %A in dictionary" lookupWord
                            | _ -> failwithf "Could not find word: %A in dictionary" lookupWord
                    | Some wordType ->
                        wordType capWord
            | None -> failwithf "Input word: %A too short" lookupWord
    
    let rec tryFindDesc lookupWord =
        let capWord = (utilityTools.checkCap lookupWord) |> utilityTools.convertToString
        if Map.containsKey capWord utilityTools.specialsList
        then
            if capWord = "A"
            then
                Ok ("a: determiner")
            else
                Ok (sprintf "%s is a Radical Quantifier" capWord)
        else
            match locator capWord with
            | Some (firstChar,firstSecondChar) ->
                let data = getFile firstChar firstSecondChar
                match data |> List.fold (descSearcher capWord) (None) with
                    | None ->
                        if (utilityTools.checkCap lookupWord) |> utilityTools.convertToString = lookupWord
                        then Ok (sprintf "%s is a proper noun" capWord)
                        else
                            let piecewiseWord = lookupWord |> Seq.toList |> List.rev                            
                            match piecewiseWord with
                            | hd::tl ->
                                if hd = 's'
                                then
                                    match tryFindDesc (tl |> List.rev |> utilityTools.convertToString) with
                                    | Ok y-> Ok (y + "This is a plural")
                                    | Error _ -> Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                                else Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                            | _ -> Error (sprintf "Could not find word: %A in dictionary" lookupWord)
                    | Some desc ->
                        Ok desc
            | None -> Error (sprintf "Input word: %A too short" lookupWord)
    
        
    member x.findWord2 lookupWord =    /// Wrapper function for word lookup, used member function to reduce stack depth
        let p = lookupWord |> Seq.toList
        match p with
        | hd::tl ->
            if hd = '<'
            then V (lookupWord)
            elif hd = '{'
            then N (lookupWord)
            elif hd = '!'
            then R (lookupWord)
            else tryFindExt lookupWord
        |_ -> failwith "Empty input" 
                
    member x.getDesc lookupWord =
        let p = lookupWord |> Seq.toList
        match p with
        | hd::tl ->
            if hd = '<'
            then Ok (sprintf "%s is a Custom Verb" lookupWord)
            elif hd = '{'
            then Ok (sprintf "%s is a Custom Noun" lookupWord)
            elif hd = '!'
            then Ok (sprintf "%s is a Radical variable" lookupWord)
            else tryFindDesc lookupWord
        |_ -> failwith "Empty input" 
    
    
    member x.CheckFiles() =            /// Member function to check dictionary is where it should be, for group stage
        let filestring = "Dictionary\\A\\AA.dict"
        print <| File.ReadLines (filestring) 

/// Knowledge bank -----------  ///
type knowledgeBankFileInterface()=
    let readFile y =            /// Simple file reader, handles exception since it is expected for most words
        try
            File.ReadLines (y) |> Seq.toList
        with
        | :? System.IO.FileNotFoundException as ex ->
            []
    
    let getFile firstChar word =                    /// Builds the file name from the word and gets alias and parent types
        let str = "Dictionary\\" + firstChar + "\\ENC\\" + word + ".dict"
        let xi = readFile str
        match xi with
        | alias::parent::tl ->            
            let aliasLex =                        /// Potential extension material
                match internalLexer.lexWordHeader alias with
                    | Some x ->
                        Some (utilityTools.convertToString (fst x))
                    | None -> None
            let parentLex =                        /// Potential extension material
                match internalLexer.lexWordHeader parent with
                | Some x ->
                    Some (utilityTools.convertToString (fst x))
                | None -> None
            tl                    
        | _ -> []
        
    
    member x.FindFile t =                /// OOP accessor functions for helping stack depth  
        let p = (t |> Seq.toList)
        match p with
        | hd::_ ->
            let firstChar = Char.ToString hd
            getFile firstChar t 
        | _ -> failwith "No lead character"

/// Reconstruction module ---- ///

module Petrichor =    

    type dictGenerator() =            /// Generates map for each groundtruth/ Rule
        
        let twoWordSeq originalMap (word1,word2) =        /// Turns 2 word pair into searchable key-elements in map
            Map.add word1 word2 originalMap   
        
        member x.getDict (gtList: string list list) =        /// OOP accessor function for class
            let wrapList lst =                                /// Offsets and wraps list with itself
                match lst with
                | hd::tl ->
                    (tl |> List.map (Some)) @ [None]   
                | _ -> failwith ("Empty lists")
            
            let pairWrap (x,y)  =                              /// unwraps option type
                match y with
                | Some p -> Some (x,p)
                | None -> None
            
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
        | Predicate p -> p 
    
    let findKey (obj: string) (dictList: Map<string, string> list) =    /// Uses List.Fold and Option state to search through map
            let folderKey state mp =
                match state with
                | Some x -> Some x
                | None ->
                    Map.tryFind obj mp
             
            dictList |> List.fold (folderKey) (None)
       
    
    let getConnectors (objList, pred) plainTextGT plainTextRL=
        let gen = dictGenerator()
        let lookupGT = gen.getDict plainTextGT
        let lookupRL = gen.getDict plainTextRL       
        
        let formatSelector predicate lst =
            match lst with
            | t1::t2::_ ->
                match findKey (unwrapPred predicate) lookupRL with
                | Some connector ->
                    [unwrapObj t1; (unwrapPred predicate) + "s"; connector; unwrapObj t2]
                | None -> [unwrapObj t1; unwrapPred predicate + "s"; unwrapObj t2]
            | t1::_ ->
                match findKey (unwrapObj t1) lookupGT with
                | Some connector -> [unwrapObj t1; connector; unwrapPred predicate]
                | None -> [unwrapObj t1; unwrapPred predicate]
            | _ -> ["Failed Inference"]
        objList |> (formatSelector pred)                  
          
/// Interface functions ---------- ///
 
let dictionaryMode1 (x: string list) =                /// Interface function for Mode 1 usage of dictionary 
    let Dict = dictionaryFileInterface()              /// Class initialisation  
    try
        x |> List.map (Dict.findWord2)
          |> List.map (utilityTools.removeNull)
          |> List.choose id
          |> Ok
          |> Some
          
    with
        | Failure msg ->                            /// Mode 1 allows error propogation back to user
            Some <| Error msg

let dictionaryMode2 (x: string list) =                /// Interface function for Mode 2 usage of dictionary
    try
        let know = knowledgeBankFileInterface()
        let p r = know.FindFile r
        x |> List.map p |> Some
    with
    | Failure msg ->                                 /// No implemented error handling since errors represent major failure in Parser
        None

let dictionaryMode3 inp plaintextGTList plaintextRLList =        /// Interface function for Mode 3 usage of dictionary
    try
        match inp with
        | Ok objPred ->
            Petrichor.getConnectors (snd objPred, fst objPred) plaintextGTList plaintextRLList
        | Error msg ->
            failwith msg
        |> Some
    with
    | Failure msg ->
        None                /// For now, in group stage, we will use these propogated errors
                            /// and display them on screen in this module 
