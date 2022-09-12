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

let rec runCFGAt (cfg:CFG) (input: char list) (stack:Stack<CFGNode>) =
    if List.isEmpty input then
        true
    else
        match stack.Pop() with
        | Variable v ->
            let rule = List.find (fun r -> r.Variable = v) cfg.Rules
            true
        | Terminal c ->
            if c = Empty then
                List.isEmpty input
            elif input.Head = c then
                runCFGAt cfg input.Tail stack
            else
                false
        