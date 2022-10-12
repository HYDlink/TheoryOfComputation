module TheoryOfComputation.ContextFreeGrammar

open System
open System.Diagnostics.CodeAnalysis
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
[<Literal>]
let TerminalEpsilon = "\001"
// let EpsilonNode = Terminal Epsilon

type Variable = string
type Terminal = string

let toTerminalList str =
    Seq.map (fun c -> c.ToString()) str |> Seq.toList


type CFGNode =
    | Variable of Variable
    | Terminal of Terminal
    | Epsilon
    | Empty

type CFGProduction = { Name: string; Production: CFGNode list } 

module CFGNode =
    let ToString node =
        match node with
        | Epsilon -> "ε"
        | Empty -> "$"
        | Variable s -> s
        | Terminal c -> c.ToString()

type CFGRule =
    { Variable: Variable
      Generate: CFGProduction list }

type CFG =
    { Start: Variable
      Rules: CFGRule list
      TerminalSet: Terminal list }

module CFG =
    let TopologySort cfg =
        cfg
    
    let isSingle list =
        List.length list = 1
    
    let splitBy predicate list =
        let accept = List.where predicate list
        let reject = List.except accept list
        (accept, reject)
    
    let CfgProdHead prod =
        match List.tryHead prod.Production with
        | Some head -> head
        | None -> Epsilon
        
    let CfgProdTail prod =
        { prod with Production = List.tail prod.Production }
    
    let EliminateLeftFactor (cfgRule: CFGRule) =
        let sorted = List.sort cfgRule.Generate
        let rec innerFn rule =
            if rule.Generate.Length <= 1 then
                [rule]
            else
                let genNewVariable factor =
                    $"{rule.Variable}'{CFGNode.ToString factor}"
                // only factor head most
                let grouped = List.groupBy CfgProdHead rule.Generate
                let (nonFactor, toBeFactor) = (splitBy (fun (key, group) -> List.length group = 1) grouped)
                assert List.forall (fun (k, v) -> isSingle v) nonFactor
                let oldProductions = List.map (fun (k, v) -> List.head v) nonFactor
                /// new rule with new variable
                let newRules =
                    List.map (fun (factor, prod) ->
                        { Variable = genNewVariable factor
                          Generate = List.map CfgProdTail prod }) toBeFactor
                    |> List.map innerFn
                    |> List.concat
                /// new production for old rule
                let newProduction = List.map (fun (factor, prod) ->
                        [ factor; Variable (genNewVariable factor) ]) toBeFactor
                let changedOldRule = { rule with Generate = oldProductions @ newProduction}
                changedOldRule :: newRules
        innerFn cfgRule
                 
        
        
    let EliminateLeftRecursiveProduction (cfgRule: CFGRule) : CFGRule list =
        let leftRecursiveProduction =
            cfgRule.Generate
            |> List.where (fun r -> CfgProdHead r = Variable cfgRule.Variable)
        let notRecursiveProduction = List.except leftRecursiveProduction cfgRule.Generate
        if leftRecursiveProduction.IsEmpty then
            [cfgRule]
        else
            let newVariable = cfgRule.Variable + "'"
            let formalVariableGenerate = notRecursiveProduction
                                         |> List.map (fun r -> r @ [(Variable newVariable)])
            let formalVariableRule = { Variable = cfgRule.Variable; Generate = formalVariableGenerate }
            
            let newVariableGenerate = leftRecursiveProduction
                                      |> List.map (fun r -> r.Tail @ [(Variable newVariable)])
            let newVariableRule = { Variable = newVariable; Generate = [Epsilon] :: newVariableGenerate }
            [formalVariableRule; newVariableRule]

    let GenerateFirstAndFollowDict cfg =
        cfg
    
    let LL1Parse cfg input =
        cfg

/// ASTNode could become inheritance generic

let rec runCFGAt (cfg: CFG) (input: Terminal list) (stack: CFGNode list) (indent: int) =
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

module DotRule =
    let CanReduce rule = List.isEmpty rule.Later
    let CanShift = CanReduce >> not

type TransitionGroup<'S, 'I> = { From: 'S; Way: 'I; To: 'S }

let IsRuleLeftRecursion (cfgRule: CFGRule) =
    cfgRule.Generate |> List.exists (fun r -> r.Head = Variable cfgRule.Variable)

type DK = DFA<DotRule list,CFGNode>

module DK = 
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
    
    [<Flags>]
    type Conflict =
        | NoConflict = 0
        | ShiftReduceConflict = 1
        | ReduceReduceConflict = 2
    
    let HasShiftReduceConflict (state:DotRule list) =
        (List.exists DotRule.CanReduce state)
        &&
        (List.exists DotRule.CanShift state)
    
    let HasReduceReduceConflict (state:DotRule list) =
        (List.where DotRule.CanReduce state).Length > 1
    let VerifyDK (dk:DK) =
    
        let (<*>) projectionA projectionB a =
            (projectionA a, projectionB a)
        
        // Flag function composite result
        let (<|>) funcFlagA funcFlagB input =
            (funcFlagA input)
            |||
            (funcFlagB input)
        
        let checkConflict (state:DotRule list) =
            (HasShiftReduceConflict >> (function |true -> Conflict.ShiftReduceConflict | false -> Conflict.NoConflict))
            <|>
            (HasReduceReduceConflict >> (fun b -> if b then Conflict.ReduceReduceConflict else Conflict.NoConflict))
            
        List.map (checkConflict <*> id) dk.States
    
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
    
let runCFGAtTerminals (cfg: CFG) (terminalList: Terminal list) =
    runCFGAt cfg terminalList [ Variable cfg.Start; Empty ] 0

/// run cfg parsing single world
let runCFG (cfg: CFG) (input: string) =
    toTerminalList input
    |> runCFGAtTerminals cfg

let TryCFG (cfg: CFG) (input: string) = runCFG cfg input |> printfn "%A"
