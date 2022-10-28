module TheoryOfComputation.RegularToNfa

open System
open TheoryOfComputation.RegularLanguage
open TheoryOfComputation.FiniteAutomata

let Concat left right = failwith ""
let Parallel up down = failwith ""
let LoopWithOut item = failwith "" 

let rec ToNFA =
    function
    | Item i ->
        let start = Guid.NewGuid()
        let next = Guid.NewGuid()
        { States = [ start; next ]        
          Rules = RuleList [ { From = start; By = Some i; To = next } ]
          InputSets = failwith "todo"
          StartState = failwith "todo"
          AcceptStates = failwith "todo" }
    | AddReg (left, right) -> Concat (ToNFA left) (ToNFA right)
    | OrReg (up, down) -> Parallel (ToNFA up) (ToNFA down)
    | GroupReg groupItem -> ToNFA groupItem
    | Closure item -> LoopWithOut (ToNFA item)