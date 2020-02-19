open ApplicativeParser

[<AutoOpen>]
module Combinators =

    //
    // Result (ROP)
    //

    let myfunction (a:int) (b:int) (c:int) =
        a + b - c
    
    //
    // Applicative Parsers
    //

    // 1 arg combinator
    let ascii (c: char) : int = int c

    // 2 arg combinator
    let sum (x: int) (y: int) = x + y

    // 3 arg combinator
    let cons3 (c1: char) (c2: char) (c3: char) = string c1 + string c2 + string c3

module ResultExamples =

    open ApplicativeResult
    open ApplicativeResult.ResultOperators

    // primitives of applicative
    let myFunctionA x y z = myfunction <!> x <*> y <*> z
    
    // Result<int -> int -> int -> int>, 'TError> -> Result<int, 'TError> -> Result<int, 'TError> -> Result<int, 'TError>
    // NOTE: <||| requires tupled function, and lifted function is curried!!!!
    let myFunctionB x y z = (liftA3 myfunction) x y z

    // NOTE: Preview feature of F# 5
    // See also https://github.com/fsharp/fslang-design/blob/master/RFCs/FS-1063-support-letbang-andbang-for-applicative-functors.md
    // and https://github.com/dsyme/fsharp-presentations/blob/master/design-notes/rethinking-applicatives.md
    let myFunctionC x y z =
        result { 
            // NOTE: This implementation of applicative CE is very much in  opposition of the Haskell way of doing the same
            //       The Result.apply definition is not used at all (BIG surprise here!!!)
            //       Also (because Monad is more powerfull and less common abstraction) Result.bind is not used either (no surprise here)

            // NOTE: let! ... and! ... is understood as "merge the sources on the right and bind them simultaneously"
            // Preparing values for BindReturn (= map). 
            //   desugars into 'let zippedBoundSources = result.MergeSources(x, y, z)'
            let! a = x 
            and! b = y
            and! c = z
            
            // Custom operation
            //check (a > b)
            //   desugars into 'result.BindReturn(zippedBoundSources, fun (a, b, c) -> a + b - c)'
            
            // Generally the workflow for desugaring this CE is
            // 1.
            //     result.Bind3Return(x, y, z, fun (a, b, c) -> a + b - c), if Bind3Return is defined
            // 2.
            //     result.BindReturn(builder.MergeSources3(x, y, z), fun (a, b, c) -> a + b - c), if MergeSources3 is defined
            // 3.
            //     result.BindReturn(result.MergeSources(x, result.MergeSources(y, z)), fun (a, (b, c)) -> a + b - c), otherwise
            return a + b - c 
        }

    // Using MergeSources3 and BindReturn
    let myFunctionD x y z =
        let result = ResultBuilder()
        // Result<int * int * int, 'TError>
        let zippedBoundSources = result.MergeSources3(x, y, z)
        // Result<int * int * int, 'TError> -> (int * int * int -> int) -> Result<int, 'TResult>
        result.BindReturn(zippedBoundSources, (fun (a : int, b: int, c: int) -> a + b - c))

    // Using only MergeSources and BindReturn => nested pairs!!!
    let myFunctionE x y z =
        let result = ResultBuilder()
        // The 3-args are a Result<int * (int * int), 'TError> nested pair
        let zippedBoundSources = result.MergeSources(x, result.MergeSources(y, z))
        // Result<int * (int * int), 'TError> -> (int * (int * int) -> int) -> Result<int, 'TResult>
        result.BindReturn(zippedBoundSources, (fun (a : int, (b: int, c: int)) -> a + b - c))

    // Using Bind3Return
    let myFunctionF x y z =
        let result = ResultBuilder()
        result.Bind3Return(x, y, z, (fun (a : int, b: int, c: int) -> a + b - c))
  
module ParserExamples  =

    open ApplicativeParser.ParserOperators

    // Combined parser using Style A    
    let asciiA (c: char) = ascii <!> (char c)

    let asciiD = pure' ascii <*> (char 'D')
    let asciiD' = ascii <!> (char 'D')

    // Example 1: Testing map
    // > run asciiD "Don Syme";;
    // val it : .... = Some (68, "on Syme")

    let asciiDo = sum <!> (asciiA 'D') <*> (asciiA 'o')

    // Example 2: Testing apply
    // Expected value is 86 + 111 = 179
    // > run asciiDo "Don Syme";;
    // val it : ..... = Some (179, "n Syme")

    // using pure and apply
    let parseDon = (pure' cons3) <*> (char 'D') <*> (char 'o') <*> (char 'n')

    // using map and apply
    let parseDon' = cons3 <!> char 'D' <*> char 'o' <*> char 'n'

    // Example 3: Testing apply
    // > "Don Syme" |> run parseDon;;
    // val it : .... = Some ("Don", " Syme")

module Main =
    [<EntryPoint>]
    let main _ =

        // Result example
        
        // See also https://github.com/dsyme/fsharp-presentations/blob/master/design-notes/rethinking-applicatives.md
        let resultValue1 = Ok 2
        let resultValue2 = Ok 3 // or: Error "fail!"
        let resultValue3 = Ok 4

        let resultA = ResultExamples.myFunctionA resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultA = %A" resultA

        let resultB = ResultExamples.myFunctionA resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultB = %A" resultB

        let resultC = ResultExamples.myFunctionC resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultC = %A" resultC

        let resultD = ResultExamples.myFunctionD resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultC = %A" resultD

        let resultE = ResultExamples.myFunctionE resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultC = %A" resultE

        let resultF = ResultExamples.myFunctionF resultValue1 resultValue2 resultValue3 // Ok 1
        printfn @"resultC = %A" resultF
        
        // Parser example

        let resOpt = run ParserExamples.asciiD "Don Syme"
        printfn @"run asciiD 'Don Syme' -> %A" resOpt

        0 // return an integer exit code
