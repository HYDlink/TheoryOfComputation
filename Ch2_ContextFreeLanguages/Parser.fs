module TheoryOfComputation.Parser
open TheoryOfComputation.ContextFreeGrammar
open TheoryOfComputation.FiniteAutomata

let bottomUpParse cfg input =
    let dk = DK.DK cfg
    // TODO check next state
    runDfaAt dk input
