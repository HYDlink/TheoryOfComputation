module TheoryOfComputation.ContextFreeGrammar

open System.Linq
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Compiler
open DotNetGraph.Edge
open DotNetGraph.Node
open TheoryOfComputation.FiniteAutomata
open Utilities

// [<Literal>]
// let Empty = '\000'
[<Literal>]
let CharEpsilon = '\001'
// let EpsilonNode = Terminal Epsilon

type Variable = string

type CFGNode =
    | Variable of Variable
    | Terminal of char
    | Epsilon
    | Empty

module CFGNode =
    let ToString node =
        match node with
        | Epsilon -> "ε"
        | Empty -> "$"
        | Variable s -> s
        | Terminal c -> c.ToString()

type CFGRule =
    { Variable: Variable
      Generate: CFGNode list list }

type CFG =
    { Start: Variable
      Rules: CFGRule list
      TerminalSet: char list }


let rec runCFGAt (cfg: CFG) (input: char list) (stack: CFGNode list) (indent: int) =
    if List.isEmpty input then
        true
    else
        printfn "%*s%A" indent "" stack.Head

        match stack.Head with
        | Variable v ->
            let rule =
                List.find (fun r -> r.Variable = v) cfg.Rules

            let nextRule r =
                runCFGAt cfg input (r @ stack.Tail) (indent + 1)

            List.exists nextRule rule.Generate
        | Terminal c ->
            if input.Head = c then
                runCFGAt cfg input.Tail stack.Tail indent
            else
                false
        | Epsilon -> runCFGAt cfg input stack.Tail indent
        | Empty -> List.isEmpty input

// S -> a.Bc, means { Start = S; Before = [ Terminal 'a']; Later = [ Variable "B"; Terminal 'c'] }
[<StructuredFormatDisplay("{Start} → {Before}.{Later}")>]
type DotRule =
    { Start: Variable
      Before: CFGNode list
      Later: CFGNode list }

type TransitionGroup<'S, 'I> = { From: 'S; Way: 'I; To: 'S }

let IsRuleLeftRecursion (cfgRule: CFGRule) =
    cfgRule.Generate |> List.exists (fun r -> r.Head = Variable cfgRule.Variable)

let EliminateLeftRecursiveProduction (cfgRule: CFGRule) : CFGRule list =
    let leftRecursiveRule =  cfgRule.Generate
                             |> List.where (fun r -> r.Head = Variable cfgRule.Variable)
    let notRecursiveRule = List.except leftRecursiveRule cfgRule.Generate
    if leftRecursiveRule.IsEmpty then
        [cfgRule]
    else
        let newVariable = cfgRule.Variable + "'"
        let formalVariableGenerate = notRecursiveRule
                                     |> List.map (fun r -> r @ [(Variable newVariable)])
        let formalVariableRule = { Variable = cfgRule.Variable; Generate = formalVariableGenerate }
        
        let newVariableGenerate = leftRecursiveRule
                                  |> List.map (fun r -> r.Tail @ [(Variable newVariable)])
        let newVariableRule = { Variable = newVariable; Generate = [Epsilon] :: newVariableGenerate }
        [formalVariableRule; newVariableRule]



let DK (cfg: CFG) =
    let CfgRuleToDotRule cfgRule =
        let selectSingleRule rule =
            { Start = cfgRule.Variable
              Before = []
              Later = rule }

        List.map selectSingleRule cfgRule.Generate

    let allRulesFromBegin =
        List.map CfgRuleToDotRule cfg.Rules
        |> List.collect id

    let getRulesStartWith start =
        List.where (fun r -> r.Start = start && r.Before.IsEmpty) allRulesFromBegin 

    let rec DFS dotRuleList =
        let getRulesHasLater =
            List.where (fun d -> not d.Later.IsEmpty)

        let nextRuleWithNode dotRule =
            let next = dotRule.Later.Head

            let nextLater = dotRule.Later.Tail
            next,
            { dotRule with
                Before = dotRule.Before @ [ next ]
                Later = nextLater }
        
        let extendNextRuleList dotRuleList =
            getRulesHasLater dotRuleList
            |> List.map (fun d -> d.Later.Head)
            
            // extract variable from CfgNodes
            |> List.distinct
            |> List.map (function Variable v -> Some v | _ -> None)
            |> List.choose id
            
            |> List.map getRulesStartWith
            |> List.concat
            |> (@) dotRuleList

        let groupByFst pairList =
            List.groupBy fst pairList
            |> List.map (fun (key, pairGroup) -> key, List.map snd pairGroup)

        let nextRuleGroups =
            getRulesHasLater dotRuleList
            |> List.map nextRuleWithNode
            |> groupByFst
            |> List.map (fun (translate, rules) -> (translate, extendNextRuleList rules))

        let pairToTransition (transition, dotRules) =
            { From = dotRuleList
              Way = transition
              To = dotRules }

        let currentResult =
            List.map pairToTransition nextRuleGroups

        let innerDfsResult =
            List.map snd nextRuleGroups
            |> List.map DFS
            |> List.collect id

        currentResult @ innerDfsResult

    let allDotRuleTransition =
        DFS allRulesFromBegin

    let startState = allRulesFromBegin

    let allStates =
        let allTranslatedStates =
            List.map (fun t -> t.To) allDotRuleTransition

        startState :: allTranslatedStates

    let acceptStates =
        List.where (List.exists (fun dotRule -> dotRule.Later.IsEmpty)) allStates

    let TransitionRuleList =
        List.map (fun t -> (t.From, t.Way, t.To)) allDotRuleTransition

    { States = allStates
      InputSets = List.map (fun t -> t.Way) allDotRuleTransition
      Rules = RuleList TransitionRuleList
      StartState = startState
      AcceptStates = acceptStates }

let exportDKGraph (dfa: DFA<DotRule list,CFGNode>) name =
    let CfgNodeDirectString =
        function Variable s -> s.ToString() | Terminal c -> c.ToString() | Epsilon -> "ε" | Empty -> "" 

    let graph = DotGraph(name, true)
    
    let CfgNodeListDirectString cfgNodeList =
        List.map CfgNodeDirectString cfgNodeList
        // |> Array.ofList
        |> String.concat ""
    
    let dotRuleToString dotRule =
        $"{dotRule.Start} → {CfgNodeListDirectString dotRule.Before} . {CfgNodeListDirectString dotRule.Later}"
    
    let dotRuleListToString dotRuleList =
        List.map dotRuleToString dotRuleList
        |> String.concat "\n"

    let state2node state =
        let node =
            DotNode(dotRuleListToString state, Shape = DotNodeShapeAttribute(DotNodeShape.Rectangle))

        if List.contains state dfa.AcceptStates then
            node.SetCustomAttribute("peripheries", "2")
        else
            node

    List.map state2node dfa.States
    |> Enumerable.Cast
    |> graph.Elements.AddRange

    match dfa.Rules with
    | RuleList ruleList ->
        List.map (fun (s, i, r) -> DotEdge(dotRuleListToString s, dotRuleListToString r, Label = (CfgNodeDirectString i))) ruleList
        |> Enumerable.Cast
        |> graph.Elements.AddRange
    | _ -> failwith "Not support Rule func"

    graph

let VisualizeDK cfg name =
    let dk = DK cfg
    let graph = exportDKGraph dk name
    let compiled = DotCompiler(graph).Compile()
    Graph.exportDotToSvg compiled name "svg"
    
let runCFG (cfg: CFG) (input: string) =
    runCFGAt cfg (Seq.toList input) [ Variable cfg.Start; Empty ] 0

let TryCFG (cfg: CFG) (input: string) = runCFG cfg input |> printfn "%A"
