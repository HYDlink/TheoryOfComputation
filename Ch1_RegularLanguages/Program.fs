// #r "nuget:FsCheck"
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
open FsCheck
open Utilities

type RuleFun<'State, 'Input> = 'State -> 'Input -> 'State option
type RuleList<'State, 'Input> = ('State * 'Input * 'State) list

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

module FA =
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
    /// <param name="exceptStates">Should contain innerState</param>
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

    let NfaToDfa (nfa: DFA<'S, char>) =
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
                let nextStates =
                    List.map (fun (_, _, n) -> n) rules

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

                let isAccept =
                    List.exists (fun s -> nfa.AcceptStates.Contains s) stateList

                let rules = nextRules stateList

                let nextStates =
                    List.map (fun (_, _, n) -> n) rules

                List.map generateStates nextStates
                |> List.fold (fun (a, b) (c, d) -> (a @ c, b @ d)) (rules, (if isAccept then [ stateList ] else []))


        let start = extendState [ nfa.StartState ]

        let (rules, acceptStateLists) =
            generateStates start

        { States = Seq.toList calcStates
          InputSets = nfa.InputSets
          Rules = RuleList rules
          StartState = start
          AcceptStates = acceptStateLists }

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

let exportGraph (dfa: DFA<'S, 'I>) name =
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
        List.map
            (fun (s, i, r) ->
                DotEdge(
                    s.ToString(),
                    r.ToString(),
                    Label =
                        (if i = Epsilon then
                             "ε"
                         else
                             i.ToString())
                ))
            ruleList
        |> Enumerable.Cast
        |> graph.Elements.AddRange
    | _ -> failwith "Not support Rule func"

    graph
// let compiled = DotCompiler(graph).Compile()
// File.WriteAllText($"{name}.dot", compiled)

let compileGraphToSvg graph name (format) =
    let compiled = DotCompiler(graph).Compile()
    let dot_file = $"{name}.dot"
    let bat_file = $"{name}.bat"
    let output_file = $"{name}.{format}"

    Console.WriteLine(compiled)
    File.WriteAllText(dot_file, compiled)

    let bat =
        $"dot -T{format} {dot_file} > {output_file}"

    File.WriteAllText(bat_file, bat)
    let result = Process.Start bat_file
    result.WaitForExit()

    File.Delete dot_file
    File.Delete bat_file

    Process.Start(ProcessStartInfo("irfanview.exe", output_file, UseShellExecute = true))


let exportToSvg dfa name =
    let graph = exportGraph dfa name
    let compiled = DotCompiler(graph).Compile()
    Graph.exportDotToSvg compiled name "svg"

let evenOddDfa: DFA<string, char> =
    { States = [ "Even"; "Odd" ]
      InputSets = [ '0'; '1' ]
      // Rules =
      //   RuleFun (fun s i ->
      //       match i with
      //       | '0' -> Some s
      //       | '1' ->
      //           match s with
      //           | "Even" -> Some "Odd"
      //           | "Odd" -> Some "Even"
      //           | _ -> None
      //       | _ -> None)
      Rules =
          RuleList [ ("Even", '0', "Even")
                     ("Even", '1', "Odd")
                     ("Odd", '0', "Odd")
                     ("Odd", '1', "Even") ]
      StartState = "Even"
      AcceptStates = [ "Even"; "Odd" ] }

let tryDFA dfa (input: string) =
    FA.runDfa dfa (Seq.toList input) |> printfn "%A"

let tryEvenOdd = tryDFA evenOddDfa

let try1 () =
    tryEvenOdd "100010"
    tryEvenOdd "11010"
    tryEvenOdd "1"
    tryEvenOdd ""
    tryEvenOdd "9"

// Example 1.11 M_4
let M4: DFA<string, char> =
    { States = [ "s"; "q1"; "q2"; "r1"; "r2" ]
      InputSets = [ 'a'; 'b' ]
      Rules =
        RuleList [ ("s", 'a', "q1")
                   ("s", 'b', "r1")
                   ("q1", 'a', "q1")
                   ("q1", 'b', "q2")
                   ("q2", 'a', "q1")
                   ("q2", 'b', "q2")
                   ("r1", 'b', "r1")
                   ("r1", 'a', "r2")
                   ("r2", 'b', "r1")
                   ("r2", 'a', "r2") ]
      StartState = "s"
      AcceptStates = [ "q1"; "r1" ] }

let tryM4 str = tryDFA M4 str

let testM4 (str: string) =
    let strList = Seq.toList str

    if List.isEmpty strList then
        None
    else
        let f = List.head strList
        let e = List.last strList

        if f = e then
            match f with
            | 'a' -> Some "q1"
            | 'b' -> Some "r1"
            | _ -> None
        else
            None

let arbTwoCharString =
    // helper function to create strings from a list of chars
    let listToString chars = chars |> List.toArray |> System.String

    // random lists of 'a's and 'b's
    let genListA =
        Gen.constant 'a' |> Gen.listOf

    let genListB =
        Gen.constant 'b' |> Gen.listOf

    (genListA, genListB)
    ||> Gen.map2 (fun listA listB -> listA @ listB)
    |> Gen.map listToString
    |> Arb.fromGen

let testM4Equality () =
    let test str =
        (FA.runDfa M4 (Seq.toList str)) = (testM4 str)

    Prop.forAll arbTwoCharString test |> Check.Quick


let try2 () =
    tryM4 "aba"
    tryM4 "aab"
    tryM4 "baa"

// arbTwoCharString.Generator |> Gen.sample 10 100 |> printfn "%A"

let ZeroOneLastThirdMustOneNFA =
    { States = [ "q1"; "q2"; "q3"; "q4" ]
      InputSets = [ '0'; '1' ]
      Rules =
        RuleList [ ("q1", '0', "q1")
                   ("q1", '1', "q1")
                   ("q1", '1', "q2")
                   ("q2", '0', "q3")
                   ("q2", '1', "q3")
                   ("q3", '0', "q4")
                   ("q3", '1', "q4") ]
      StartState = "q1"
      AcceptStates = [ "q4" ] }

let EvenOrThreeZeroNFA =
    { States = [ "s"; "e1"; "e2"; "t1"; "t2"; "t3" ]
      InputSets = [ '0' ]
      Rules =
        RuleList [ ("s", Epsilon, "e1")
                   ("s", Epsilon, "t1")
                   ("e1", '0', "e2")
                   ("e2", '0', "e1")
                   ("t1", '0', "t2")
                   ("t2", '0', "t3")
                   ("t3", '0', "t1") ]
      StartState = "s"
      AcceptStates = [ "e1"; "t1" ] }

let TryNFA nfa input =
    let seqInput = Seq.toList input
    FA.runNFA nfa seqInput |> printfn "%A"

let MultiRedundantZeroOne =
    { States = [ 'A'; 'B'; 'C'; 'D'; 'E' ]
      InputSets = [ '0'; '1' ]
      Rules = RuleList [
          ('A', '0', 'B')
          ('A', '1', 'C')
          ('B', '0', 'B')
          ('B', '1', 'D')
          ('C', '0', 'B')
          ('C', '1', 'C')
          ('D', '0', 'B')
          ('D', '1', 'E')
          ('E', '0', 'B')
          ('E', '1', 'C')
      ]
      StartState = 'A'
      AcceptStates = [ 'E' ] }

// testM4Equality ()

// exportToSvg evenOddDfa "EvenOdd"
// Console.WriteLine()
// exportToSvg M4 "M4"
let TryZeroOneLastThirdMustOneNFA() =
    // TryNFA ZeroOneLastThirdMustOneNFA "001011"
    exportToSvg ZeroOneLastThirdMustOneNFA "ZeroOne"
    let ZeroOneLastThirdMustOneDFA = FA.NfaToDfa ZeroOneLastThirdMustOneNFA |> FA.normalizeFaState
    exportToSvg ZeroOneLastThirdMustOneDFA "ZeroOneDFA"
    let minimizeDfa = FA.minimizeDfa ZeroOneLastThirdMustOneDFA
    exportToSvg minimizeDfa "ZeroOneDFAMinimized"

// TryZeroOneLastThirdMustOneNFA()
// next combine NFA

let TryEvenOrThreeZeroNFA() =
    // TryNFA EvenOrThreeZeroNFA "00"
    // TryNFA EvenOrThreeZeroNFA "000"
    // TryNFA EvenOrThreeZeroNFA "00000"
    // TryNFA EvenOrThreeZeroNFA "000000"
    exportToSvg EvenOrThreeZeroNFA "EvenOrThree0"
    let EvenOrThreeZeroDFA = FA.NfaToDfa EvenOrThreeZeroNFA |> FA.normalizeFaState
    let EvenOrThreeZeroDFAMinimized = FA.minimizeDfa EvenOrThreeZeroDFA
    exportToSvg EvenOrThreeZeroDFA "EvenOrThree0DFA"
    exportToSvg EvenOrThreeZeroDFAMinimized "EvenOrThree0DFAMinimized"

TryEvenOrThreeZeroNFA()

// exportToSvg MultiRedundantZeroOne "MultiRedundantZeroOne"
// exportToSvg (FA.minimizeDfa MultiRedundantZeroOne) "MinimizeMultiRedundantZeroOne"

// FA.NfaToDfa EvenOrThreeZeroNFA |> printfn "%A"

// let strList = new List<string list>()
// strList.Add(["e1"; "t1"])
// strList.Contains(["e1"; "t1"]) |> printfn "%A"
