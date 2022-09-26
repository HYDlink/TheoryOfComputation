module TheoryOfComputation.ContextFreeGrammar


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
type DotRule = { Start: Variable; Before: CFGNode list; Later: CFGNode list }
let DK (cfg: CFG) =
    let allRulesFromBegin = List. cfg.Rules

let runCFG (cfg: CFG) (input: string) =
    runCFGAt cfg (Seq.toList input) [ Variable cfg.Start; Empty ] 0

let TryCFG (cfg: CFG) (input: string) = runCFG cfg input |> printfn "%A"