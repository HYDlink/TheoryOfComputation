module TheoryOfComputation.FiniteAutomata.Program
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
          RuleList [ ("Even", '0', "Even")
                     ("Even", '1', "Odd")
                     ("Odd", '0', "Odd")
                     ("Odd", '1', "Even") ]
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

// testM4Equality ()

// exportToSvg evenOddDfa "EvenOdd"
// Console.WriteLine()
// exportToSvg M4 "M4"
TryNFA ZeroOneLastThirdMustOneNFA "001011"
exportToSvg ZeroOneLastThirdMustOneNFA "ZeroOne"
exportToSvg (FA.NfaToDfa ZeroOneLastThirdMustOneNFA) "ZeroOneDFA"

// next combine NFA

// TryNFA EvenOrThreeZeroNFA "00"
// TryNFA EvenOrThreeZeroNFA "000"
// TryNFA EvenOrThreeZeroNFA "00000"
// TryNFA EvenOrThreeZeroNFA "000000"
exportToSvg EvenOrThreeZeroNFA "EvenOrThree0"
exportToSvg (FA.NfaToDfa EvenOrThreeZeroNFA) "EvenOrThree0DFA"

// FA.NfaToDfa EvenOrThreeZeroNFA |> printfn "%A"

// let strList = new List<string list>()
// strList.Add(["e1"; "t1"])
// strList.Contains(["e1"; "t1"]) |> printfn "%A"