module TheoryOfComputation.FiniteAutomataProgram
open TheoryOfComputation.FiniteAutomata
open FsCheck

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
          RuleList [ { From = "Even"; By = '0'; To="Even" }
                     { From = "Even"; By = '1'; To="Odd" }
                     { From = "Odd"; By = '0'; To="Odd" }
                     { From = "Odd"; By = '1'; To="Even" } ]
      StartState = "Even"
      AcceptStates = [ "Even"; "Odd" ] }

let tryDFA dfa (input: string) =
    runDfa dfa (Seq.toList input) |> printfn "%A"

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
        RuleList [ { From = "s"; By = 'a'; To="q1" }
                   { From = "s"; By = 'b'; To="r1" }
                   { From = "q1"; By = 'a'; To="q1" }
                   { From = "q1"; By = 'b'; To="q2" }
                   { From = "q2"; By = 'a'; To="q1" }
                   { From = "q2"; By = 'b'; To="q2" }
                   { From = "r1"; By = 'b'; To="r1" }
                   { From = "r1"; By = 'a'; To="r2" }
                   { From = "r2"; By = 'b'; To="r1" }
                   { From = "r2"; By = 'a'; To="r2" } ]
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
        (runDfa M4 (Seq.toList str)) = (testM4 str)

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
        RuleList [ { From = "q1"; By = '0'; To="q1" }
                   { From = "q1"; By = '1'; To="q1" }
                   { From = "q1"; By = '1'; To="q2" }
                   { From = "q2"; By = '0'; To="q3" }
                   { From = "q2"; By = '1'; To="q3" }
                   { From = "q3"; By = '0'; To="q4" }
                   { From = "q3"; By = '1'; To="q4" } ]
      StartState = "q1"
      AcceptStates = [ "q4" ] }

let EvenOrThreeZeroNFA =
    { States = [ "s"; "e1"; "e2"; "t1"; "t2"; "t3" ]
      InputSets = [ '0' ]
      Rules =
        RuleList [ { From = "s"; By = Epsilon; To="e1" }
                   { From = "s"; By = Epsilon; To="t1" }
                   { From = "e1"; By = '0'; To="e2" }
                   { From = "e2"; By = '0'; To="e1" }
                   { From = "t1"; By = '0'; To="t2" }
                   { From = "t2"; By = '0'; To="t3" }
                   { From = "t3"; By = '0'; To="t1" } ]
      StartState = "s"
      AcceptStates = [ "e1"; "t1" ] }

let TryNFA nfa input =
    let seqInput = Seq.toList input
    runNFA nfa seqInput |> printfn "%A"

let TestSvg() =
    // testM4Equality ()
    
    // exportToSvg evenOddDfa "EvenOdd"
    // Console.WriteLine()
    // exportToSvg M4 "M4"
    TryNFA ZeroOneLastThirdMustOneNFA "001011"
    exportCharToSvg ZeroOneLastThirdMustOneNFA "ZeroOne"
    exportCharToSvg (NfaToDfa ZeroOneLastThirdMustOneNFA) "ZeroOneDFA"
    
    // next combine NFA
    
    // TryNFA EvenOrThreeZeroNFA "00"
    // TryNFA EvenOrThreeZeroNFA "000"
    // TryNFA EvenOrThreeZeroNFA "00000"
    // TryNFA EvenOrThreeZeroNFA "000000"
    exportCharToSvg EvenOrThreeZeroNFA "EvenOrThree0"
    exportCharToSvg (NfaToDfa EvenOrThreeZeroNFA) "EvenOrThree0DFA"

    // FA.NfaToDfa EvenOrThreeZeroNFA |> printfn "%A"
    
    // let strList = new List<string list>()
    // strList.Add(["e1"; "t1"])
    // strList.Contains(["e1"; "t1"]) |> printfn "%A"

TestSvg()