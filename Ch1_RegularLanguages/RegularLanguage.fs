module TheoryOfComputation.RegularLanguage

open System
open System.Linq
open System.Text.RegularExpressions
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Compiler
open DotNetGraph.Edge
open DotNetGraph.Node
open Utilities

type RegularItem<'a> =
    | AddReg of (RegularItem<'a> * RegularItem<'a>)
    | OrReg of (RegularItem<'a> * RegularItem<'a>)
    | GroupReg of RegularItem<'a>
    | Closure of RegularItem<'a>
    | Item of 'a

type RegularString = RegularItem<string>

let (>>) left right =
    AddReg (left, right)

let (<|>) left right =
    OrReg (left, right)
    
let (~-) item = Closure item

let rec ToStr =
    function
    | Item i ->
        i.ToString()
    | AddReg (left, right) -> (ToStr left) + (ToStr right)
    | OrReg (up, down) -> (ToStr up) + "|" + (ToStr down)
    | GroupReg groupItem -> "(" + ToStr groupItem + ")"

type NfaInputItem<'a> =
    | Some of 'a
    | Epsilon

type Transition<'State, 'Input> =
    { From: 'State
      By: 'Input NfaInputItem
      To: 'State }
    
module Transition =
    let FromTo<'i> (from:'a) (next: 'a): Transition<'a, 'i> =
              {
                From = from
                By = Epsilon
                To = next }
    
    

type NfaGraph<'State, 'Input> =
    { States: 'State list
      Rules: Transition<'State, 'Input> list }

module NfaGraph =
    let StartState graph = graph.States.Head
    let EndState graph = List.last graph.States

    let Concat (left: NfaGraph<'a, 'i>) (right: NfaGraph<'a, 'i>) =
        let (concatRule: Transition<'a, 'i>) =
            { From = EndState left
              By = Epsilon
              To = StartState right }

        { States = left.States @ right.States
          Rules = concatRule :: (left.Rules @ right.Rules) }
    
    ///           / up.start   --- up.end   \
    /// newStart -- down.start --- down.end -- newEnd
    let Parallel (up: NfaGraph<'a, 'i>) (down: NfaGraph<'a, 'i>) =
        let newStart = Guid.NewGuid()
        let newEnd = Guid.NewGuid()
        let start2up = Transition.FromTo newStart (StartState up)
        let start2down = Transition.FromTo newStart (StartState down)
        let up2end = Transition.FromTo (EndState up) newEnd
        let down2end = Transition.FromTo (EndState down) newEnd
        
        { States = [newStart] @ up.States @ down.States @ [newEnd]
          Rules = [ start2up; start2down ] @ (up.Rules @ down.Rules) @ [ up2end; down2end ] }
    
    let Loop nfa =
        let loopTranslation = Transition.FromTo (EndState nfa) (EndState nfa)
        { nfa with Rules = loopTranslation :: nfa.Rules }
        
    let LoopWithOut nfa =
        let loop1 = Transition.FromTo (StartState nfa) (EndState nfa)
        let loop2 = Transition.FromTo (EndState nfa) (StartState nfa)
        { nfa with Rules = loop1 :: loop2 :: nfa.Rules }
        
    let Visualize nfa name =
        let graph = DotGraph(name, true)
        let state2node state =
            DotNode(state.ToString(), Shape = DotNodeShapeAttribute(DotNodeShape.Circle), Label = "")
    
        List.map state2node nfa.States
        |> Enumerable.Cast
        |> graph.Elements.AddRange
        
        let transition2edge  { From=s; By=i;To=r } =
            let label = match i with Some some -> some.ToString() | Epsilon -> "ε" 
            DotEdge(s.ToString(), r.ToString(), Label = DotLabelAttribute(label))
        
        nfa.Rules 
        |> List.map transition2edge
        |> Enumerable.Cast
        |> graph.Elements.AddRange
        
        graph
    
let rec ToNFA =
    function
    | Item i ->
        let start = Guid.NewGuid()
        let next = Guid.NewGuid()
        { States = [ start; next ]        
          Rules = [ { From = start; By = Some i; To = next } ] }
    | AddReg (left, right) -> NfaGraph.Concat (ToNFA left) (ToNFA right)
    | OrReg (up, down) -> NfaGraph.Parallel (ToNFA up) (ToNFA down)
    | GroupReg groupItem -> ToNFA groupItem
    | Closure item -> NfaGraph.LoopWithOut (ToNFA item)
    
let exportToSvg reg name =
    let nfa = ToNFA reg
    // let name = ToStr reg
    let graph = NfaGraph.Visualize nfa name
    let compiled = DotCompiler(graph).Compile()
    Graph.exportDotToSvg compiled name "svg"

let TestRegToString() =
    let abc= (GroupReg (Item "a" <|> Item "b")) >> (Item "c")
    printfn $"%A{ToStr abc}" 
    exportToSvg abc "abc"
