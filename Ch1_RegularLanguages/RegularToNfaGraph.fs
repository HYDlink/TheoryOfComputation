module TheoryOfComputation.RegularToNfaGraph

open System
open TheoryOfComputation.NfaGraph
open DotNetGraph.Compiler
open TheoryOfComputation.RegularLanguage
open Utilities

    
let rec ToNfaGraph =
    function
    | Item i ->
        let start = Guid.NewGuid()
        let next = Guid.NewGuid()
        { States = [ start; next ]        
          Rules = [ { From = start; By = Some i; To = next } ] }
    | AddReg (left, right) -> NfaGraph.Concat (ToNfaGraph left) (ToNfaGraph right)
    | OrReg (up, down) -> NfaGraph.Parallel (ToNfaGraph up) (ToNfaGraph down)
    | GroupReg groupItem -> ToNfaGraph groupItem
    | Closure item -> NfaGraph.LoopWithOut (ToNfaGraph item)
    
let exportToSvg reg name =
    let nfa = ToNfaGraph reg
    // let name = ToStr reg
    let graph = NfaGraph.Visualize nfa name
    let compiled = DotCompiler(graph).Compile()
    Graph.exportDotToSvg compiled name "svg"

let TestRegToString() =
    let abc= (GroupReg (Item "a" <|> Item "b")) >> (Item "c")
    printfn $"%A{ToStr abc}" 
    exportToSvg abc "abc"
