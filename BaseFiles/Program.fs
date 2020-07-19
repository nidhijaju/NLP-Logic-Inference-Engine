// Learn more about F# at http://fsharp.org

open System
open LangDictionary
open Parser
open LogicConversion
open Evaluator
open Expecto.Logging
open FullStack
let print x =
    printfn "%A" x
   
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let sm = FullStack.SpiderMonkey()
    print <| sm.runSMSafe (["Ramon is a dog"; "Nidhi is audio"; "Neelesh is a clown"])
    print <| sm.runSMSafe (["Joao is a dog"])
    print <| sm.adHocInference (["Mike is a clown"]) (["All clowns are cold"; "All clowns are weird"])
    let sw = FullStack.SideWinder()
    print <| sw.ParseCheck ("Ramon is a dog")
    print <| sw.getLex ("Ramon is a dog")
    
     
    
    0 // return an integer exit code