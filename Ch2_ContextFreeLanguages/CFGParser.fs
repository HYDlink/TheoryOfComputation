module TheoryOfComputation.CFGParser

open FParsec.CharParsers
open TheoryOfComputation.ContextFreeGrammar
open FParsec

let PARSER_NAME = "ParserName"
let PRODUCTION = "Production"

let PRODUCTION_VARIABLE =
    "ProductionVariable"

let STRING_LITERAL = "StringLiteral"
let VARIABLE_NODE = "VariableNode"

let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let stringLiteral: Parser<string, unit> =
    let normalChar =
        satisfy (fun c -> c <> '\\' && c <> '"')

    let unescape c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c -> c

    let escapedChar =
        pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))
    <?> STRING_LITERAL

let pVariable =
    many1Satisfy2L isUpper isLetter VARIABLE_NODE
    |>> Variable

let pNode =
    pVariable <|> (stringLiteral |>> TerminalString)
    |>> fun s ->
        printfn $"pNode %A{s}"
        s

let pDerivation =
    // TODO support automatic failure check and turn to next
    sepBy pNode spaces1 <?> "Derivation"

let pNamedDerivation =
    pDerivation
    .>> (pstring "-- ")
    .>> (manySatisfy (isNoneOf "\n\r"))
    <?> "Named Derivation"

let pProduction =
    sepBy pDerivation ((pchar '|') .>>. spaces)
    <?> "Production"

let pGenerate =
    let pVar =
        pVariable
        .>> (spaces .>>. (pchar '=') .>>. spaces)

    pVar .>>. pProduction <?> "Generate"
