// For more information see https://aka.ms/fsharp-console-apps

open System
open System.Collections.Generic
open System.Data
open System.Linq
open System.Runtime.CompilerServices
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Compiler
open DotNetGraph.Edge
open DotNetGraph.Node
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

type PDARule<'State, 'Input, 'StackElem> = ('State * 'Input * 'StackElem) * ('State * 'StackElem)

type PDA<'State, 'Input, 'StackElem> =
    {
      // generate states?
      StartState: 'State
      AcceptStates: 'State list
      States: 'State list
      InputSet: 'Input list
      Rules: PDARule<'State, 'Input, 'StackElem> list }

module PDA =
    let run (pda: PDA<'State, char, CFGNode>) (input: string) =
        let rec innerRun state (curInput: char list) (stack: CFGNode list) =
            if stack.Head = Empty then
                curInput.IsEmpty
            elif curInput.IsEmpty then
                false
            else
                // first check epsilon input and epsilon stack elem rule
                let currentStateRules =
                    List.filter (fun ((s, i, st), _) -> s = state) pda.Rules

                let epsilonRules =
                    List.filter (fun ((_, i, st), _) -> st = Epsilon && i = CharEpsilon) currentStateRules

                let matchRules =
                    List.filter (fun ((_, i, st), _) -> st = stack.Head && i = curInput.Head) currentStateRules

                let runRule rule =
                    let ((cur, i, st), (nextState, nst)) = rule
                    assert (cur = state)

                    let nextInput =
                        if i = CharEpsilon then
                            curInput
                        else
                            curInput.Tail

                    let poppedStack =
                        if st = Epsilon then
                            stack
                        else
                            stack.Tail

                    let nextStack =
                        if nst = Epsilon then
                            poppedStack
                        else
                            nst :: poppedStack

                    innerRun nextState nextInput nextStack

                List.map runRule currentStateRules
                |> List.reduce (||)

        innerRun pda.StartState (Seq.toList input) []

    let RuleToString (rule: (('State * char * CFGNode) * ('State * CFGNode))) =
        let ((_, curInput, stack), (_, nextStack)) =
            rule

        let inputStr =
            if curInput = CharEpsilon then
                "ε"
            else
                curInput.ToString()

        $"{inputStr}, {CFGNode.ToString stack} → {CFGNode.ToString nextStack}"

    let exportGraph (pda: PDA<string, char, CFGNode>) name =
        let graph = DotGraph(name, true)

        let state2node state =
            let node =
                DotNode(state.ToString(), Shape = DotNodeShapeAttribute(DotNodeShape.Circle))

            if List.contains state pda.AcceptStates then
                node.SetCustomAttribute("peripheries", "2")
            else
                node

        let groupedRules =
            List.groupBy (fun ((q, _, _), (n, _)) -> (q, n)) pda.Rules

        let rules2edge (ruleGroup: ((string * string) * ((string * char * CFGNode) * (string * CFGNode)) list)) =
            let ((start, nextState), _) = ruleGroup

            let combinedRuleStr =
                List.map RuleToString (snd ruleGroup)
                |> List.fold (fun a b -> a + "\n" + b) ""

            DotEdge(start.ToString(), nextState.ToString(), Label = combinedRuleStr)


        let rule2edge rule =
            let ((q, i, s), (n, ns)) = rule
            DotEdge(q.ToString(), n.ToString(), Label = (RuleToString rule))

        List.map state2node pda.States
        |> Enumerable.Cast
        |> graph.Elements.AddRange

        List.map rules2edge groupedRules
        |> Enumerable.Cast
        |> graph.Elements.AddRange

        graph

    let visualizePda pda name =
        let graph = exportGraph pda name
        Graph.compileGraphToSvg graph name "svg"

let generatePDAFromCFG (cfg: CFG) : PDA<string, char, CFGNode> =
    let START_STATE = "Start"
    let PREPARE_STATE = "Prepare"
    let LOOP_STATE = "Loop"
    let END_STATE = "End"

    let genEmptyRule from to_ nextStackElem : PDARule<string, char, CFGNode> =
        ((from, CharEpsilon, Epsilon), ((to_), nextStackElem))

    let genRule (cfgRule: CFGRule) =
        let { Variable = varName; Generate = rules } =
            cfgRule

        let expandRuleToState
            (rule: CFGNode list)
            (headName: string)
            (variable: string)
            : PDARule<string, char, CFGNode> list * string list =
            // aBc -> ((q1, e, e), (q2, c)) -> ((q2, e, e), (q3, B)) -> ((q3, e, e), (Loop, a))
            let Length = rule.Length

            if rule.Length = 0 then
                ([], [])
            else
                let stateName i =
                    if i = Length || i = 0 then
                        LOOP_STATE
                    else
                        $"{headName}{i}"

                let states =
                    [ for i in 0..Length do
                          yield stateName i ]

                let revRule = List.rev rule

                let initRule =
                    ((LOOP_STATE, CharEpsilon, Variable variable), ((stateName 1), revRule.Head))

                let cfgRuleToPdaRule i r =
                    (((stateName (i + 1)), CharEpsilon, Epsilon), ((stateName (i + 2)), r))

                let pdaRules =
                    List.skip 1 revRule |> List.mapi cfgRuleToPdaRule

                (initRule :: pdaRules, states)

        List.mapi (fun i r -> expandRuleToState r $"{varName}{i}" varName) rules
        |> List.fold (fun (a, b) (c, d) -> (a @ c, b @ d)) ([], [])

    let (fromCfgRules, states) =
        List.map genRule cfg.Rules
        |> List.fold (fun (a, b) (c, d) -> (a @ c, b @ d)) ([], [])

    let fromCfgTerminalRules: PDARule<string, char, CFGNode> list =
        let fromTerminal t =
            ((LOOP_STATE, t, Terminal t), (LOOP_STATE, Epsilon))

        List.map fromTerminal cfg.TerminalSet

    let defaultRules =
        [ genEmptyRule START_STATE PREPARE_STATE Empty
          genEmptyRule PREPARE_STATE LOOP_STATE (Variable cfg.Start)
          ((LOOP_STATE, CharEpsilon, Empty), (END_STATE, Epsilon)) ]

    { StartState = START_STATE
      AcceptStates = [ END_STATE ]
      States =
        [ START_STATE
          PREPARE_STATE
          LOOP_STATE
          END_STATE ]
        @ states
      InputSet = cfg.TerminalSet
      Rules = defaultRules @ fromCfgRules @ fromCfgTerminalRules }

/// stack 存储需要匹配的 CFGNode list，最前面的 Head 是当前需要匹配的元素
let rec runCFGAt (cfg: CFG) (input: char list) (stack: CFGNode list) (indent: int) =
    if List.isEmpty input then
        true
    elif indent >= 10 then
        false
    else
        // printfn "%*s%A Input: %A" indent "" stack.Head input

        match stack.Head with
        | Variable v ->
            // CFG 为了避免前置递归，导致无限深入，需要对 rule 排序一个优先级来执行
            // 比如 T -> Ta | e 应当先执行 T -> e 检查是否成功，然后再进入递归尝试
            let nextRule =
                List.find (fun r -> r.Variable = v) cfg.Rules

            let isNextRuleSuccess gen =
                let success = runCFGAt cfg input (gen @ stack.Tail) (indent + 1)
                if success then
                    printfn "%*s From %A Next Rule %A" indent "" (Variable v) gen
                    printfn "%*s%A Input: %A" indent "" stack.Head input
                success

            List.exists isNextRuleSuccess nextRule.Generate
        | Terminal c ->
            if input.Head = c then
                printfn "%*s%A Input: %A" indent "" stack.Head input
                runCFGAt cfg input.Tail stack.Tail indent
            else
                false
        | Epsilon -> runCFGAt cfg input stack.Tail indent
        | Empty -> List.isEmpty input


let runCFG (cfg: CFG) (input: string) =
    runCFGAt cfg (Seq.toList input) [ Variable cfg.Start; Empty ] 0

let TryCFG (cfg: CFG) (input: string) = runCFG cfg input |> printfn "%A"

let ZeroOneSymmetry =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Terminal '0'
                  Variable "S"
                  Terminal '1' ]
                [] ] } ]
      TerminalSet = [ '0'; '1' ] }

let ZeroOnePDA =
    {
      // generate states?
      StartState = "q1"
      AcceptStates = [ "q1"; "q4" ]
      States = [ "q1"; "q2"; "q3"; "q4" ]
      InputSet = [ '0'; '1' ]
      Rules =
        [ (("q1", CharEpsilon, Epsilon), ("q2", Empty))
          (("q2", '0', Epsilon), ("q2", Terminal '0'))
          (("q2", '1', Terminal '0'), ("q3", Epsilon))
          (("q3", '1', Terminal '0'), ("q3", Epsilon))
          (("q3", CharEpsilon, Empty), ("q4", Epsilon)) ] }

let TryZeroOne () =
    PDA.visualizePda ZeroOnePDA "ZeroOnePDA"
    PDA.visualizePda (generatePDAFromCFG ZeroOneSymmetry) "ZeroOnePDAfromCFG"

    TryCFG ZeroOneSymmetry "0011"
    TryCFG ZeroOneSymmetry "011"
    TryCFG ZeroOneSymmetry "0101"
    TryCFG ZeroOneSymmetry "0000011111"


let getDependencyGraph cfg = cfg

let ChomskyNormalize cfg = cfg

let G6 =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Variable "A"
                  Variable "S"
                  Variable "A" ]
                [ Terminal 'a'; Variable "B" ] ] }
          { Variable = "A"
            Generate = [ [ Variable "B" ]; [ Variable "S" ] ] }
          { Variable = "B"
            Generate = [ [ Epsilon ]; [ Terminal 'b' ] ] } ]
      TerminalSet = [ 'a'; 'b' ] }

/// S -> aTb | b
/// T -> Ta | e
let G7 =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Terminal 'a'
                  Variable "T"
                  Terminal 'b' ]
                [ Terminal 'b' ] ] }
          { Variable = "T"
            Generate =
              [ [ Epsilon ]
                [ Variable "T"; Terminal 'a' ] ] } ]
      TerminalSet = [ 'a'; 'b' ] }

let Calculate =
    { Start = "Expr"
      Rules =
        [ 
          { Variable = "Expr"
            Generate =
              [ [ Variable "Term" ]
                [ Variable "Expr"
                  Terminal '+'
                  Variable "Term" ] ] }
          { Variable = "Term"
            Generate =
              [ [ Variable "Factor" ]
                [ Variable "Term"
                  Terminal '*'
                  Variable "Factor" ] ] }
          { Variable = "Factor"
            Generate =
              [ [ Variable "Digit" ]
                [ Terminal '('
                  Variable "Expr"
                  Terminal ')' ] ] }
          { Variable = "Digit"
            Generate = (Seq.toList "0123456789") |> List.map (fun c -> [Terminal c]) } ]
      TerminalSet = Seq.toList "+-*/0123456789" }

let TryG7 () =
    TryCFG G7 "aaab"
    TryCFG G7 "ab"
    TryCFG G7 "b"
    TryCFG G7 "aa"
    PDA.visualizePda (generatePDAFromCFG G7) "G7"

let TryCalculate () =
    // TryCFG Calculate "9+5"
    // TryCFG Calculate "4*3"
    TryCFG Calculate "9+5+2"
    TryCFG Calculate "9*5+2"
    TryCFG Calculate "9*(5+2)"
    // PDA.visualizePda (generatePDAFromCFG Calculate) "Calculate"

// TryG7()

PDA.visualizePda (generatePDAFromCFG G6) "G6"

// TryCalculate()