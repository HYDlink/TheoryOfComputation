module TheoryOfComputation.FiniteAutomata


open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Compiler
open DotNetGraph.Edge
open DotNetGraph.Node
open Utilities

type RuleFun<'State, 'Input> = 'State -> 'Input -> 'State option
type Transition<'State, 'Input> = 'State * 'Input * 'State
type RuleList<'State, 'Input> = Transition<'State, 'Input> list

type RuleSet<'State, 'Input> =
    | RuleFun of RuleFun<'State, 'Input>
    | RuleList of RuleList<'State, 'Input>

type DFA<'State, 'Input> =
    { States: ('State list)
      InputSets: 'Input list
      Rules: RuleSet<'State, 'Input>
      StartState: 'State
      AcceptStates: 'State list }

type FiniteAutomataType =
    | DFA
    | NFA

type StateParam<'S> =
    | State of 'S
    | StateList of 'S list

module StateParam =
    let checkState p s =
        match p with
        | State se -> se = s
        | StateList l -> l.Contains s

[<Literal>]
let Epsilon: char = '\000'


let isDFA dfa =
    let FiniteAutomataType b = if b = true then DFA else NFA

    match dfa.Rules with
    | RuleList list ->
        List.exists (fun (_, i, _) -> i = Epsilon) list
        || List.distinctBy (fun (s, i, r) -> (s, i)) list
           |> List.length
           |> (=) list.Length
    // |> FiniteAutomataType
    | _ -> failwith "todo"


/// get rules if state match and input is Epsilon,
/// add to new states, get new matchedRules,
/// check again
/// <param name="exceptStates">Should contain s</param>
let rec getRule (li: RuleList<'S, char>) (innerState: 'S) (exceptStates: 'S list) : RuleList<'S, char> =
    let matchedRules =
        List.filter (fun (s, i, _) -> innerState = s) li

    let epsilonRules =
        List.filter (fun (_, i, _) -> i = Epsilon) matchedRules

    let extendStates: 'S list =
        List.map (fun (_, _, r) -> r) epsilonRules
        |> List.except exceptStates

    let validRules =
        List.except epsilonRules matchedRules

    if extendStates.IsEmpty then
        validRules
    else
        let nextRules =
            List.map (fun s -> getRule li s (s :: exceptStates)) extendStates
            |> List.concat

        validRules @ nextRules

let runDfaRule rule state input =
    match rule with
    | RuleFun func -> func state input
    | RuleList l ->
        let rule =
            List.tryFind
                (fun item ->
                    let (s, i, _) = item
                    s = state && i = input)
                l

        match rule with
        | Some (_, _, r) -> Some r
        | None -> None

let rec runDfaAt (dfa: DFA<'State, 'Input>) (curState: 'State) (input: 'Input list) : 'State option =
    if input.IsEmpty then
        if List.contains curState dfa.AcceptStates then
            Some curState
        else
            None
    else
        let nextState =
            runDfaRule dfa.Rules curState (List.head input)

        match nextState with
        | Some n ->
            let nextInput = List.skip 1 input
            runDfaAt dfa n nextInput
        | None -> None

let runDfa dfa input = runDfaAt dfa dfa.StartState input

let runNfaRule rule (state) input =
    let (RuleList li) = rule

    let rules =
        match state with
        | StateList stateList ->
            List.map (fun s -> getRule li s [ s ]) stateList
            |> List.concat
        | State s -> getRule li s [ s ]

    List.filter (fun (_, i, _) -> i = input) rules
    |> List.map (fun (_, i, r) -> r)


let rec runNfaAt (dfa: DFA<'State, 'Input>) (curState: StateParam<'State>) (input: 'Input list) : 'State option =
    if input.IsEmpty then
        let getAccepted s =
            if List.contains s dfa.AcceptStates then
                Some s
            else
                None

        let extract o =
            match o with
            | Some v -> v
            | None -> None

        match curState with
        | State s -> getAccepted s
        | StateList l ->
            List.map getAccepted l
            |> List.tryFind Option.isSome
            |> extract

    else
        let nextStates =
            runNfaRule dfa.Rules curState (List.head input)

        if nextStates.IsEmpty then
            None
        else
            runNfaAt dfa (StateList nextStates) (List.tail input)

let rec runNFA nfa input =
    runNfaAt nfa (State nfa.StartState) input

let NfaToDfa (nfa:DFA<'S, char>) =
    let (RuleList ruleList) = nfa.Rules
    let extendState stateList =
        List.filter (fun (st, i, ns) -> List.contains st stateList && i = Epsilon) ruleList
        |> List.map (fun (_, _, ns) -> ns)
        |> (@) stateList
    let nextRules stateList =
        // get rules which start state is in stateList
        // group list with input, get all possible nextState in it, means stateList --input-> nextState List 
        List.filter (fun (st, _, _) -> List.contains st stateList) ruleList
        |> List.groupBy (fun (_, i, _) -> i)
        |> List.map (fun (i, rules) ->
            let nextStates = List.map (fun (_, _, n) -> n) rules
            (stateList, i, (extendState nextStates)))
    
    // if stateList already exists return
    // if List.exists (fun s -> nfa.AcceptStates.Contains s) stateList then addToAccept
    // get nextRules
    let calcStates = new List<'S list>() 
    let rec generateStates (stateList: 'S list) : (('S list * char * 'S list) list * 'S list list) =
        if calcStates.Contains(stateList) then
            ([], [])
        else
            Trace.WriteLine $"%A{stateList}"
            calcStates.Add(stateList)
            let isAccept = List.exists (fun s -> nfa.AcceptStates.Contains s) stateList
            let rules = nextRules stateList
            let nextStates = List.map (fun (_,_,n) ->n) rules
            List.map generateStates nextStates
            |> List.fold (fun (a, b) (c, d) -> (a@c, b@d))
                   (rules, if isAccept then [stateList] else [])
        

    let start = extendState [nfa.StartState]
    let (rules, acceptStateLists) = generateStates start 
    
    { States = Seq.toList calcStates
      InputSets = nfa.InputSets
      Rules = RuleList rules
      StartState= start
      AcceptStates = acceptStateLists }

let normalizeFaState (dfa: DFA<'S, 'I>) =
    let mapToNewStates = List.mapi (fun i s -> (s, i)) dfa.States
                        |> Map.ofList
    let newStartState = mapToNewStates[dfa.StartState]
    let newAcceptStates = List.map (fun s -> mapToNewStates[s]) dfa.AcceptStates
    let newStates = Map.toList mapToNewStates |> List.map (fun (k, v) -> v)
    let (RuleList rules) = dfa.Rules
    let newRules = List.map (fun (from, _i, dest) -> (mapToNewStates[from], _i, mapToNewStates[dest])) rules
    {
        StartState = newStartState
        States = newStates
        InputSets = dfa.InputSets
        Rules = RuleList newRules
        AcceptStates = newAcceptStates }

let minimizeDfa (dfa: DFA<'S, 'I>) = //: DFA<'S list, 'I> =
    let SplitState (stateList: 'S list) (otherStateListSets: 'S list list) =
        let getStateGroup input state =
            let nextOption =
                runDfaRule dfa.Rules state input

            match nextOption with
            | None -> None
            | Some next -> List.tryFind (List.contains next) otherStateListSets

        let SplitStateByInput input =
            List.groupBy (getStateGroup input) stateList
            |> List.map snd

        if stateList.Length = 0 then
            [stateList]
        else
            // get first split state
            // for loop with break return
            seq {
                for input in dfa.InputSets do
                    let split = SplitStateByInput input
                    if split.Length > 1 then
                        printfn "Split from %A %A" stateList split
                        yield split
                yield [stateList]
            }
            |> Seq.item 0

    let minimizeStates =
        let mutable startList = []

        let mutable calculatedList =
            [ (List.except dfa.AcceptStates dfa.States)
              dfa.AcceptStates ]

        while startList.Length <> calculatedList.Length do
            // startList = calculatedList
            let split =
                List.map (fun s -> SplitState s calculatedList) calculatedList
                |> List.collect id

            startList <- calculatedList
            calculatedList <- split

        calculatedList
    
    let getNewStateFromOld oldState = List.find (List.contains oldState) minimizeStates
    let newStartState = getNewStateFromOld dfa.StartState
    let newAcceptState = dfa.AcceptStates
    // stateList 是已经最小化的新状态（原来的状态集合的子集）
    // 因为是最小化的, 因此其中任何一个原有状态的规则 都等于现有的规则
    let getRule stateList =
         if List.isEmpty stateList then
             []
         else
             let (RuleList rules) = dfa.Rules
             List.where (fun (s, _, _) -> s = stateList[0]) rules
             |> List.map (fun (_, input, next) ->
                 let newNextState = getNewStateFromOld next
                 (stateList, input, newNextState))
    let newRules = List.map getRule minimizeStates |> List.collect id

    {
        States = minimizeStates
        StartState = newStartState
        Rules = RuleList newRules
        InputSets = dfa.InputSets
        AcceptStates = [dfa.AcceptStates]
    }
 

let exportGraphWithCustomLabel (dfa: DFA<'S, 'I>) name inputLabeler =
    let graph = DotGraph(name, true)

    let state2node state =
        let node =
            DotNode(state.ToString(), Shape = DotNodeShapeAttribute(DotNodeShape.Circle))

        if List.contains state dfa.AcceptStates then
            node.SetCustomAttribute("peripheries", "2")
        else
            node

    List.map state2node dfa.States
    |> Enumerable.Cast
    |> graph.Elements.AddRange

    match dfa.Rules with
    | RuleList ruleList ->
        List.map (fun (s, i, r) -> DotEdge(s.ToString(), r.ToString(), Label = DotLabelAttribute(inputLabeler i))) ruleList
        |> Enumerable.Cast
        |> graph.Elements.AddRange
    | _ -> failwith "Not support Rule func"

    graph

let exportGraph dfa name =
    let inputLabeler i = if i = Epsilon then "ε" else i.ToString()
    exportGraphWithCustomLabel dfa name inputLabeler

let exportGeneralGraph dfa name =
    let stringer i = i.ToString() 
    exportGraphWithCustomLabel dfa name stringer 
    
let exportToSvg dfa name =
    let graph = exportGeneralGraph dfa name
    let compiled = DotCompiler(graph).Compile()
    Graph.exportDotToSvg compiled name "svg"