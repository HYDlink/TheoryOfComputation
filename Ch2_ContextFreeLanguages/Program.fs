// For more information see https://aka.ms/fsharp-console-apps

open System.Collections.Generic

type Variable = string

type CFGNode =
    | Variable of Variable
    | Terminal of char

type CFGRule =
    { Variable: Variable
      Generate: CFGNode list list }

type CFG =
    { Start: Variable
      Rules: CFGRule list }

let ZeroOneSymmetry =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Terminal '0'
                  Variable "S"
                  Terminal '1' ]
                [] ] } ] }

[<Literal>]
let Empty = '\000'

let rec runCFGAt (cfg:CFG) (input: char list) (stack: CFGNode list) (indent:int) =
    if List.isEmpty input then
        true
    else
        printfn $"{string('\t', indent)}%A{stack.Head}"
        match stack.Head with
        | Variable v ->
            let rule = List.find (fun r -> r.Variable = v) cfg.Rules
            let nextRule r =
                runCFGAt cfg input (r @ stack.Tail) (indent + 1)
            List.exists nextRule rule.Generate
        | Terminal c ->
            if c = Empty then
                List.isEmpty input
            elif input.Head = c then
                runCFGAt cfg input.Tail stack.Tail indent
            else
                false

let runCFG (cfg:CFG) (input:string) =
    runCFGAt cfg (Seq.toList input) [Terminal Empty; Variable cfg.Start] 0

let TryCFG (cfg:CFG) (input:string) =
    runCFG cfg input
    |> printfn "%A"

TryCFG ZeroOneSymmetry "0011"
TryCFG ZeroOneSymmetry "011"
TryCFG ZeroOneSymmetry "0101"
TryCFG ZeroOneSymmetry "0000011111"
