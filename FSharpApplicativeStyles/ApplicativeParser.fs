module ApplicativeParser

// (startIndex, stringToParse)
type SpanInfo = int * string

// State Applicative Parser (See also State Monad)
type Parser<'T> = Parser of runParser : (SpanInfo -> option<'T * SpanInfo>)

// Parser<'a> -> (SpanInfo -> option<'a * SpanInfo>)
let runParser (Parser p) = p

// runParser but use string in place of SpanInfo
// That is given a Parser<string> the result is: string -> (string * string) option
let run (p: Parser<'T>) =
    (fun (s: string) ->
        match runParser p <| (0, s) with
        | Some (x, (i, s)) -> Some (x, s.Substring(i, s.Length - i))
        | None -> None)

// private helper: apply a function to fst of pair
let private first f (x, y) = (f x, y)

//
// Functor
//

// ('a -> 'b) - Parser<'a> -> Parser<'b>
let map f (Parser p) =
    Parser <| (fun spanInfo -> Option.map (first f) (p spanInfo))

//
// Applicative
//

// Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>
let apply p1 p2 =
    let p3 = fun spanInfo ->
        match (runParser p1) spanInfo with
            | Some (f, restSpanInfo) -> runParser (map f p2) restSpanInfo  // uses map of Functor Parser
            | None -> None
    Parser p3

//let zip p1 p2 =

//
// Primitive parser
//

// make primitive character parser
let satisfy (predicate: char -> bool) : Parser<char> =
    // runParser :: (SpanInfo -> option<char * SpanInfo>)
    let f (i, s : string) : option<char * SpanInfo> =
        if (i < s.Length)
        then 
            let c = s.[i] 
            if (predicate c) then Some (c, (i + 1, s)) else None
        else None
    Parser f

// char -> Parser<char>
let char c = satisfy ((=) c) // This one will be used from now on!!!

/// Style A operators
module ParserOperators =

    // 'a -> Parser<'a>
    let pure' x = Parser <| (fun spanInfo -> Some (x, spanInfo))
    
    // ('a -> 'b) -> Parser<'a> -> Parser<'b>
    let (<!>) f p = map f p

    // Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>
    let (<*>) = apply

    // convenient functions: liftA, liftA2, and liftA3
    let liftA  g r1         = pure' g <*> r1                     // g <!> r1
    let liftA2 g r1 r2      = pure' g <*> r1 <*> r2              // g <!> r1 <*> r2
    let liftA3 g r1 r2 r3   = pure' g <*> r1 <*> r2 <*> r3       // g <!> r1 <*> r2 <*> r3
    // etc....


// CE for style C
type ParserBuilder() = 
    //member _.MergeSources(p1: Parser<'T>, p2: Parser<'T>) = zip p1 p2
    member _.BindReturn(p: Parser<'T>, f) = map f p

let parser = ParserBuilder()
