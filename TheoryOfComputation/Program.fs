// #r "nuget:FsCheck"
open System
open System.Diagnostics
open System.IO
open System.Linq
open DotNetGraph
open DotNetGraph.Attributes
open DotNetGraph.Compiler
open DotNetGraph.Edge
open DotNetGraph.Node
open FsCheck

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

let runRule rule state input =
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

let rec runAt (dfa: DFA<'State, 'Input>) (curState: 'State) (input: 'Input list) : 'State option =
    if input.IsEmpty then
        if List.contains curState dfa.AcceptStates then
            Some curState
        else
            None
    else
        let nextState =
            runRule dfa.Rules curState (List.head input)

        match nextState with
        | Some n ->
            let nextInput = List.skip 1 input
            runAt dfa n nextInput
        | None -> None

let run dfa input = runAt dfa dfa.StartState input

let exportGraph (dfa:DFA<'S, 'I>) name =
    let graph = DotGraph(name, true)
    let state2node state =
        let node = DotNode(state.ToString(), Shape=DotNodeShapeAttribute(DotNodeShape.Circle))
        if List.contains state dfa.AcceptStates then
            node.SetCustomAttribute("peripheries", "2")
        else
            node

    List.map state2node dfa.States
    |> Enumerable.Cast
    |> graph.Elements.AddRange
    
    match dfa.Rules with
    | RuleList ruleList ->
        List.map (fun (s, i, r) ->
            DotEdge(s.ToString(), r.ToString(), Label = i.ToString())) ruleList
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
    
    File.WriteAllText(dot_file, compiled) 
    let bat = $"dot -T{format} {dot_file} > {output_file}"
    File.WriteAllText(bat_file, bat)
    let result = Process.Start bat_file
    result.WaitForExit()
            
    File.Delete dot_file
    File.Delete bat_file
   

let exportToSvg dfa name =
    let graph = exportGraph dfa name
    compileGraphToSvg graph name "svg"

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
      Rules = RuleList [
          ("Even", '0', "Even")
          ("Even", '1', "Odd")
          ("Odd", '0', "Odd")
          ("Odd", '1', "Even")
      ]
      StartState = "Even"
      AcceptStates = [ "Even"; "Odd" ] }

let tryDFA dfa (input: string) =
    run dfa (Seq.toList input) |> printfn "%A"

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
        (run M4 (Seq.toList str)) = (testM4 str)
    Prop.forAll arbTwoCharString test
    |> Check.Quick


let try2 () =
    tryM4 "aba"
    tryM4 "aab"
    tryM4 "baa"

// arbTwoCharString.Generator |> Gen.sample 10 100 |> printfn "%A" 


// testM4Equality ()

exportToSvg evenOddDfa "EvenOdd"
Console.WriteLine()
exportToSvg M4 "M4"