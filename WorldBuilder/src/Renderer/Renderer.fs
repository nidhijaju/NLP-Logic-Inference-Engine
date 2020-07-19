module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron

open Fable.Import
open Fable.Import
open Fable.Import
open Fable.Import
open Node.Exports
open Fable.PowerPack
open FullStack
open LangDictionary

/// Counter variable for help tooltip
let mutable count = 0
/// Ground truths collected from front end
let mutable (groundtruths: string list) = []
/// Rule definitions collected from the front end
let mutable (world: string list) = []



// Reference the element of the View used by the application
module Ref =
    
    let cage = Browser.document.getElementById("cage")

    let automatic = (downcast Browser.document.getElementById("automatic") : Browser.HTMLInputElement)
    let manual = (downcast Browser.document.getElementById("manual") : Browser.HTMLInputElement)

    let saveText = Browser.document.getElementById("act-save-text")
    let inputText = (downcast Browser.document.getElementById("input-text") : Browser.HTMLInputElement)
    inputText.className <- "nonParse form-control"
    let saveWorld = Browser.document.getElementById("act-save-world")
    let inputWorld = (downcast Browser.document.getElementById("input-world") : Browser.HTMLInputElement)
    inputWorld.className <- "nonParse form-control"
    let worldTitle = Browser.document.getElementById("world-title")
    let submitText = Browser.document.getElementById("act-submit-text")
    let clearWorld = Browser.document.getElementById("act-clear-text")    
    
    let input = Browser.document.getElementById("ground-truths")
    let world = Browser.document.getElementById("world")
    let output = Browser.document.getElementById("output")
    let parseOut = (downcast Browser.document.getElementById("parseDisplay") : Browser.HTMLInputElement)
    let mutable Auto = true
    let parseColours = Browser.document.getElementById("parseColours")
// Used to storage a reference of the about window
// Otherwise, the window is destroy by the Garbage Collector
let mutable aboutWindow : BrowserWindow option = Option.None


// Reference to the electron process
// This allow us to create new window later for example
let remote = importMember<Remote> "electron"

/// Generator for row of Grount Truth or Rule List table
let generateInputRow inp =
    let root = Browser.document.createElement_tr()
    let stringval = Browser.document.createElement_td()
    stringval.innerText <- inp

    Html.replaceChildren root [stringval]
    root
/// Utility function to unwrap Error types into a string for display
let unWrapTypes inp =
        match inp with
        | Core.Result.Error x ->
            sprintf "%A" x
        | Core.Result.Ok (a,b) ->
            sprintf "%A" a
let init () =           
    
    Browser.document.getElementById("card1").style.padding <- "20px"
    Ref.worldTitle.style.padding <- "20px"
    
    Ref.inputWorld.readOnly <- true
    Ref.saveWorld.hidden <- true
    Ref.world.hidden <- true
    Ref.worldTitle.hidden <- true
    Ref.cage.hidden <- true
    
    /// Reactive element, on change in Ground Truth box input, this function attempts to parse and displays undeerlining
    /// and error bar messages appropriate to input
    Ref.inputText.addEventListener_input(fun _ ->
        let SW = SideWinder()
        let res = SW.ParseCheck Ref.inputText.value
        let typy = SW.getLex Ref.inputText.value |> unWrapTypes
        match res with
        | Core.Result.Ok s ->
            Ref.inputText.className <- "goodParse form-control"
            Ref.saveText.className <- "btn btn-primary mr-1"
            Ref.inputText.title <- typy
            Ref.parseOut.value <- "No Errors detected"
        | Core.Result.Error x ->
            Ref.inputText.className <- "nonParse form-control"
            Ref.saveText.className <- "btn btn-secondary mr-1"
            Ref.inputText.title <- "Error in type checking"
            Ref.parseOut.value <- (sprintf "%A" res)
        null
        )
    
    /// Reactive element, on change in World Builder box input, this function attempts to parse and displays undeerlining
    /// and error bar messages appropriate to input
    Ref.inputWorld.addEventListener_input(fun _ ->
        let SW = SideWinder()
        let res = SW.ParseCheck Ref.inputWorld.value
        let typy = SW.getLex Ref.inputWorld.value |> unWrapTypes        
        match res with
        | Core.Result.Ok s ->
            Ref.inputWorld.className <- "goodParse form-control"
            Ref.saveWorld.className <- "btn btn-primary mr-1"
            Ref.inputWorld.title <- typy
            Ref.parseOut.value <- "No Errors detected"
        | Core.Result.Error x ->
            Ref.inputWorld.className <- "nonParse form-control"
            Ref.saveWorld.className <- "btn btn-secondary mr-1"
            Ref.inputText.title <- "Error in type checking"
            Ref.parseOut.value <- (sprintf "%A" res)
        null
        )
    
    /// Radio button handler for automatic mode
    Ref.automatic.addEventListener_click(fun _ ->
        Ref.inputWorld.readOnly <- true
        Ref.saveWorld.hidden <- true
        Ref.world.hidden <- true
        Ref.worldTitle.hidden <- true
        Ref.Auto <- true
        null
    )
    
    /// Radio button handler for manual mode
    Ref.manual.addEventListener_click(fun _ ->
        Ref.inputWorld.readOnly <- false
        Ref.saveWorld.hidden <- false
        Ref.world.hidden <- false
        Ref.worldTitle.hidden <- false
        Ref.Auto <- false
        null
    )
    
    /// Button click response to add Rule to the World list
    Ref.saveWorld.addEventListener_click(fun _ ->   
        world <- List.append world [Ref.inputWorld.value]
        Ref.inputWorld.value <- ""  
        List.map generateInputRow world
        |> Html.replaceChildren Ref.world
        |> ignore
        Ref.submitText.className <- "btn btn-primary mr-1"
        Ref.saveWorld.className <- "btn btn-secondary mr-1"
        null
    )
    
    /// Button click response to add ground truth to Truth list
    Ref.saveText.addEventListener_click(fun _ ->
        groundtruths <- List.append groundtruths [Ref.inputText.value]
        Ref.inputText.value <- ""   
        List.map generateInputRow groundtruths
        |> Html.replaceChildren Ref.input
        |> ignore
        Ref.submitText.className <- "btn btn-primary mr-1"
        Ref.saveText.className <- "btn btn-secondary mr-1"
        null
    )
    
    /// Runs actual fullstack program using tje BackEndInterface() class
    Ref.submitText.addEventListener_click(fun _ ->
        let elem = Browser.document.createElement_a()
        let BackEnd = FullStack.frontEndIntegration.BackEndInterface()
        if Ref.Auto
        then            
            let data, valid = BackEnd.getInferences groundtruths
            if valid
            then elem.innerText <- data
            else
                elem.innerText <- data
                Ref.inputText.innerText <- ""
                groundtruths <- []
                let temp = Browser.document.createElement_a()
                temp.innerText <- "Ground Truths Cleared"
                Html.replaceChildren Ref.input [temp]
        else
            let data, valid = BackEnd.getAdHocResult groundtruths world
            if valid
            then elem.innerText <- data
            else
                elem.innerText <- data
                Ref.inputText.innerText <- ""
                groundtruths <- []
                world <- []
                let temp = Browser.document.createElement_a()
                temp.innerText <- "Ground Truths Cleared"
                Html.replaceChildren Ref.input [temp]
                let temp2 = Browser.document.createElement_a()
                temp2.innerText <- "World Cleared"
                Html.replaceChildren Ref.world [temp2]
        Html.replaceChildren Ref.output [elem]
        Ref.submitText.className <- "btn btn-secondary mr-1"
        null
    )
    
    /// Response for clear world command function
    Ref.clearWorld.addEventListener_click(fun _ ->
        Ref.inputText.innerText <- ""
        groundtruths <- []
        world <- []
        let temp = Browser.document.createElement_a()
        temp.innerText <- "Ground Truths Cleared"
        Html.replaceChildren Ref.input [temp]
        let temp2 = Browser.document.createElement_a()
        temp2.innerText <- "World Cleared"
        Html.replaceChildren Ref.world [temp2]
        let elem = Browser.document.createElement_a()
        elem.innerText <- "Workspace cleared"
        Html.replaceChildren Ref.output [elem]
        Ref.submitText.className <- "btn btn-secondary mr-1"
        null        
        )

init()