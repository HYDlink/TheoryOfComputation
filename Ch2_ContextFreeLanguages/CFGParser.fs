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

let withSpace p = (p .>> (many (anyOf " \t")))
let withAnySpaces p = (p .>> spaces)
let pNewLine = (many (anyOf "\r\n"))
let withNewLine p = (p .>> pNewLine)

let log s =
    printfn $"Log %A{s}"
    s

let withLog p = p |>> log

let pVariableName: Parser<string, unit> =
    many1Satisfy2L isUpper isLetter VARIABLE_NODE

let pVariable =
    pVariableName
    |>> Variable

let pNode =
    pVariable <|> (stringLiteral |>> Terminal)
    // |> withLog

let pDerivationName: Parser<string, unit> =
    (pstring "-- ") >>. (manySatisfy (isNoneOf "\n\r")) <?> "Derivation Name"
    
/// separate CFGNode only by spaces
let pNonNameDerivation =
    many (withSpace pNode) <?> "Non Name Derivation"

let pDerivation =
    pNonNameDerivation .>> (opt pDerivationName)

/// pDerivation with *-- name* until newline
let pNamedDerivation =
    pNonNameDerivation .>> pDerivationName
    <?> "Named Derivation"

/// separate different derivation with *newline or |*
let pProduction =
    // sepBy *cannot* precede with *spaces*, otherwise once hit spaces, would continue Sep check
    attempt (sepBy (withAnySpaces pNamedDerivation) ((pchar '|') .>>. spaces))
    <|>
    (pDerivation |>> (fun s -> [s]))
    <?> "Production"

/// pProduction with head variable name
let pGenerate =
    let pVar =
        (withAnySpaces pVariableName)
        .>> (withAnySpaces (pchar '='))
        // |> withLog

    pVar .>>. pProduction <?> "Generate"
    |>> (fun (var, generate) -> { Variable = var; Generate = generate })
    // |> withLog

let pOhm =
    let pName = many1Satisfy2L isUpper isLetter "Name"
    let pBegin = pchar '{'
    let pEnd = pchar '}'
    spaces >>. (withAnySpaces pName) .>> (withAnySpaces pBegin)
    >>. // temp ignore ParserName
    (many (pGenerate .>> pNewLine .>> spaces))
    .>> (withSpace pEnd)