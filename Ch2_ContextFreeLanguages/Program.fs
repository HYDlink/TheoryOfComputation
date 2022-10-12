// For more information see https://aka.ms/fsharp-console-apps
open TheoryOfComputation.ContextFreeGrammar
open TheoryOfComputation.PushDownAutomata
open TheoryOfComputation.CFGParser

let ZeroOneSymmetry =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Terminal "0"
                  Variable "S"
                  Terminal "1" ]
                [] ] } ]
      TerminalSet = toTerminalList "01"}

let ZeroOnePDA =
    {
      // generate states?
      StartState = "q1"
      AcceptStates = [ "q1"; "q4" ]
      States = [ "q1"; "q2"; "q3"; "q4" ]
      InputSet = [ "0"; "1" ]
      Rules =
        [ (("q1", TerminalEpsilon, Epsilon), ("q2", Empty))
          (("q2", "0", Epsilon), ("q2", Terminal "0"))
          (("q2", "1", Terminal "0"), ("q3", Epsilon))
          (("q3", "1", Terminal "0"), ("q3", Epsilon))
          (("q3", TerminalEpsilon, Empty), ("q4", Epsilon)) ] }

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
                [ Terminal "a"; Variable "B" ] ] }
          { Variable = "A"
            Generate = [ [ Variable "B" ]; [ Variable "S" ] ] }
          { Variable = "B"
            Generate = [ [ Epsilon ]; [ Terminal "b" ] ] } ]
      TerminalSet = toTerminalList "ab" }

/// S -> aTb | b
/// T -> Ta | e
let G7 =
    { Start = "S"
      Rules =
        [ { Variable = "S"
            Generate =
              [ [ Terminal "a"
                  Variable "T"
                  Terminal "b" ]
                [ Terminal "b" ] ] }
          { Variable = "T"
            Generate =
              [ [ Epsilon ]
                [ Variable "T"; Terminal "a" ] ] } ]
      TerminalSet = toTerminalList "ab" }

let Calculate =
    { Start = "Expr"
      Rules =
        [ { Variable = "Expr"
            Generate =
              [ [ Variable "Term" ]
                [ Variable "Expr"
                  Terminal "+"
                  Variable "Term" ] ] }
          { Variable = "Term"
            Generate =
              [ [ Variable "Factor" ]
                [ Variable "Term"
                  Terminal "*"
                  Variable "Factor" ] ] }
          { Variable = "Factor"
            Generate =
              [ [ Variable "Digit" ]
                [ Terminal "("
                  Variable "Expr"
                  Terminal ")" ] ] }
          { Variable = "Digit"
            Generate =
              (Seq.map
                 (fun c -> [ Terminal (c.ToString()) ]) "0123456789")
              |> Seq.toList } ]
      TerminalSet = toTerminalList "+-*/0123456789" }

let SimpleCalculate =
    { Start = "E"
      Rules =
        [ { Variable = "E"
            Generate =
              [ [ Variable "T" ]
                [ Variable "E"
                  Terminal "+"
                  Variable "T" ] ] }
          { Variable = "T"
            Generate =
              [ [ Terminal "a" ]
                [ Variable "T"
                  Terminal "*"
                  Terminal "a" ] ] } ]
      TerminalSet = toTerminalList "ab" }


let TryG7 () =
    TryCFG G7 "aaab"
    TryCFG G7 "ab"
    TryCFG G7 "b"
    TryCFG G7 "aa"
    PDA.visualizePda (generatePDAFromCFG G7) "G7"

let TryCalculate () =
    // TryCFG Calculate "9+5"
    // TryCFG Calculate "4*3"
    // TryCFG Calculate "9+5+2"
    // TryCFG Calculate "9*5+2"
    // TryCFG Calculate "9*(5+2)"
    PDA.visualizePda (generatePDAFromCFG Calculate) "Calculate"
    DK.VisualizeDK Calculate "CalculateDK"

let VisualizeSimpleCalculate () =
    PDA.visualizePda (generatePDAFromCFG SimpleCalculate) "SimpleCalculate"
    DK.VisualizeDK SimpleCalculate "SimpleCalculateDK"

let example () =
    let mutable outer = 1

    let innerEvaluate =
        let test = 2
        outer <- outer + 1
        test * outer

    printfn $"{innerEvaluate}"
    printfn $"{innerEvaluate}"
// TryG7()

// example()
// PDA.visualizePda (generatePDAFromCFG G6) "G6"
// VisualizeDK G6 "DKG6"

// VisualizeSimpleCalculate()
// TryCalculate()
// TryCalculate()

let TestEliminateLeftFactor() =
  let example = {
      Variable = "Z"
      Generate =
      [
        [ Terminal "b"; Terminal "c"; Variable "Y" ]
        [ Terminal "a"; Terminal "b"; Variable "X" ]
        [ Terminal "a"; Terminal "c"; Variable "Y" ]
        [ Terminal "a"; Terminal "b"; Variable "X"; Variable "Y" ]
      ]}
  CFG.EliminateLeftFactor example |> printfn "%A"

let TestEliminateLeftRecursion() =
    let LeftRecursionMostSimple =
        { Start = "A"
          Rules =
            [ { Variable = "A"
                Generate =
                  [ [ Variable "A"; Terminal "a" ]
                    [ Terminal "b" ] ] } ]
          TerminalSet = toTerminalList "ab" }
        
    let InDirectLeftRecursion =
        { Start = "S"
          Rules =
            [ { Variable = "S"
                Generate =
                  [ [ Variable "A"; Terminal "a" ]
                    [ Terminal "b" ] ]}
              { Variable = "A"
                Generate =
                  [ [ Variable "A"; Terminal "s" ]
                    [ Variable "S"; Terminal "d" ]
                    [ Terminal "b" ]
                    [ Epsilon ] ] } ]
          TerminalSet = toTerminalList "ab" }
      

    CFG.EliminateLeftRecursiveProduction LeftRecursionMostSimple.Rules.Head
    |> printfn "%A"

// TestEliminateLeftRecursion
    
let TestCfgParser() =
    test pDerivation "Value \"Example\""
    test pProduction "Value | \"Example\""
    test pGenerate @"Exp = Or" 
    test pGenerate @"Or 
  	= Concat ""|"" Concat -- haha
  	| Concat -- heihei"
    test pOhm @"
Regex {
   Exp = Or
   Or 
  	= Concat ""|"" Concat -- whynot
  	| Concat -- try anoter
}"