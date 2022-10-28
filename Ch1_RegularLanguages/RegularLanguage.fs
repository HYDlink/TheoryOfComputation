module TheoryOfComputation.RegularLanguage

type RegularItem<'a> =
    | AddReg of (RegularItem<'a> * RegularItem<'a>)
    | OrReg of (RegularItem<'a> * RegularItem<'a>)
    | GroupReg of RegularItem<'a>
    | Closure of RegularItem<'a>
    | Item of 'a

type RegularString = RegularItem<string>

let (>>) left right =
    AddReg (left, right)

let (<|>) left right =
    OrReg (left, right)
    
let (~-) item = Closure item

let ToRegular str =
    let rec InnerFn = function
        | [] -> failwith "Empty input"
        | [ i ] -> Item i
        | h :: tail -> (Item h) >> (InnerFn tail)
    Seq.toList str
    |> InnerFn
    
let rec ToStr =
    function
    | Item i ->
        i.ToString()
    | AddReg (left, right) -> (ToStr left) + (ToStr right)
    | OrReg (up, down) -> (ToStr up) + "|" + (ToStr down)
    | GroupReg groupItem -> "(" + ToStr groupItem + ")"
    | Closure closureItem -> ToStr closureItem + "*"
