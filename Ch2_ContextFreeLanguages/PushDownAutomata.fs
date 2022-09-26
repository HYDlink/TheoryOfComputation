module TheoryOfComputation.PushDownAutomata

open System.Linq
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Edge
open DotNetGraph.Node
open Utilities
open TheoryOfComputation.ContextFreeGrammar

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
                    List.skip 1 revRule
                    |> List.mapi cfgRuleToPdaRule
                    
                (initRule::pdaRules, states)

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