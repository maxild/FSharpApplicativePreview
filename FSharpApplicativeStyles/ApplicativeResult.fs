module ApplicativeResult

//type Result<'T,'TError> = 
//    | Ok of ResultValue:'T 
//    | Error of ErrorValue:'TError

module Result = 
    // Result<'a -> b', 'TError> -> Result<'a, 'TError> -> Result<'b, 'TError> 
    //let apply g r =
    //    match g with
    //    | Ok f -> match r with 
    //              | Ok x -> Ok <| f x 
    //              | Error e -> Error e
    //    | Error e -> Error e

    // I like this one, compared to above
    let apply f x = 
        match f, x with
        | Ok fres, Ok xres -> Ok (fres xres)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    // propagation pattern!!!
    let zip x1 x2 = 
        match x1, x2 with
        | Ok x1res, Ok x2res -> Ok (x1res, x2res)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    let zip3 x1 x2 x3 = 
        match x1, x2, x3 with
        | Ok x1res, Ok x2res, Ok x3res -> Ok (x1res, x2res, x3res)
        | Error e, _, _ -> Error e
        | _, Error e, _ -> Error e
        | _, _, Error e -> Error e

    // Optimized convenient helpers (liftA2, liftA3 can be 'overriden' like this)

    let map2 f x1 x2 = 
        match x1, x2 with
        | Ok x1res, Ok x2res -> Ok (f x1res x2res)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    let map3 f x1 x2 x3 = 
        match x1, x2, x3 with
        | Ok x1res, Ok x2res, Ok x3res -> Ok (f x1res x2res x3res)
        | Error e, _, _ -> Error e
        | _, Error  e, _ -> Error e
        | _, _, Error e -> Error e

    // mapN etc...

module ResultOperators =
    // primitives
    let pure' x = Ok x
    let (<!>) f x = Result.map f x    
    let (<*>) f x = Result.apply f x
    // convenient functions: liftA, liftA2, and liftA3 (default definitions of map2, map3 etc...)
    let liftA  g r1         = pure' g <*> r1                     // g <!> r1
    let liftA2 g r1 r2      = pure' g <*> r1 <*> r2              // g <!> r1 <*> r2
    let liftA3 g r1 r2 r3   = pure' g <*> r1 <*> r2 <*> r3       // g <!> r1 <*> r2 <*> r3
    // etc....
 
// CE for style C
type ResultBuilder() = 
    // Applicative oprations (preview)
    member _.MergeSources(t1: Result<'T,'U>, t2: Result<'T1,'U>) = Result.zip t1 t2
    // Result<'a, 'TError> -> ('a -> 'b) -> Result<'b, 'TError>
    member _.BindReturn(x: Result<'T,'U>, f) = Result.map f x // map tupled and flipped

    // perf
    member _.MergeSources3(x1: Result<'T1, 'U>, x2: Result<'T2, 'U>, x3: Result<'T3, 'U>) : Result<'T1 * 'T2 * 'T3, 'U> = Result.zip3 x1 x2 x3

    // more perf
    member builder.Bind3Return(x1: Result<'T1, 'U>, x2: Result<'T2, 'U>, x3: Result<'T3, 'U>, f: 'T1 * 'T2 * 'T3 -> 'T4) : Result<'T4, 'U> = 
        Result.map3 (fun  x y z -> f (x, y, z)) x1 x2 x3 // we need to uncurry!!!
 
    // Monadic operations (required for Check member to work)
    
    // Result<'T, 'TError> -> ('T -> 'U) -> Result<'U, 'TError> 
    // Result<int * int * int, 'TError> -> (int * int * int -> int) -> Result<'U, 'TError> 
    //member _.Bind(x, f) = Result.bind f x // flipped!!!
    //member _.Return(x) = Ok x

    //[<CustomOperation("check", MaintainsVariableSpaceUsingBind = true) >]
    //member _.Check(x: Result<'T,string>, [<ProjectionParameter>] f: 'T -> bool) =
    //     match x with 
    //     | Ok xRes -> if f xRes then x else Error "check failed"
    //     | Error err -> Error err

let result = ResultBuilder()

// dummy wrapper to make compiler happy
type M<'T> = M of 'T

// This is a prototype/interface/class/trait or whatnot for the applicative builder members
// IT does not work, the laws are bend to the extreme:-) Only here to have nice docs with type annotations
// NOTE: All members are tupled functions
type ApplicativeBuilder() =
    //inherit TraceCore()

    // Helper for the simplest Monad of all in the world
    let Unwrap (M x) = x // is and should be private

    //
    // Functor
    //

    // If you have Bind, then BindReturn can be added for performance
    //
    // If you don't have Bind, then adding BindReturn allows a single let! bind followed by a return,
    // i.e. an applicative.
    member builder.BindReturn(x: M<'T1>, f: 'T1 -> 'T2) : M<'T2> = M <| (f << Unwrap) x

    //
    // Primitive if more than Functor
    //

    // The standard Return
    member builder.Return(x: 'T) : M<'T> = M x

    //
    // Primitive Applicative operations: Map2 = Bind << MergeSources
    //                                   Apply g x = (Return g) 
    
    // If you have Bind or BindReturn, MergeSources adds support for `and!`. 
    // NOTE: Struct tuples can be used
    member builder.MergeSources(x1: M<'T1>, x2: M<'T2>) : M<'T1 * 'T2> = 
        M (Unwrap x1, Unwrap x2)

    //
    // Performance optimizations 
    //
    // NOTE: convenience helpers: liftA2=map2, liftA3=map3 etc.. are called Bind2Return, Bind3Return
    //
    // Map3 = Bind << MergeSources3
    // Map4 = Bind << MergeSources4
    //
    // Map3 = Bind3Return
    // Map4 = Bind4Return
    //
    // Bind2 is monadic, why do we need that one?

    // If you have MergeSources, then a MergeSources3 can be added for performance 
    // NOTE: Struct tuples can be used
    member builder.MergeSources3(x1: M<'T1>, x2: M<'T2>, x3: M<'T3>) : M<'T1 * 'T2 * 'T3> = 
        M (Unwrap x1, Unwrap x2, Unwrap x3)

    // MergeSources4, MergeSources5 etc. 

    // If you have BindReturn and MergeSources, then Bind2Return can be added for performance
    member builder.Bind2Return(x1: M<'T1>, x2: M<'T2>, f: 'T1 * 'T2 -> 'T3) : M<'T3> = 
        M <| f (Unwrap x1, Unwrap x2)

    // If you have BindReturn and MergeSources, then Bind3Return can be added for performance
    member builder.Bind3Return(x1: M<'T1>, x2: M<'T2>, x3: M<'T3>, f: 'T1 * 'T2 * 'T3 -> 'T4) : M<'T4> = 
        M <| f (Unwrap x1, Unwrap x2, Unwrap x3)

    // Bind4Return, Bind5Return etc.

    //
    // Primitive Monad operations
    //

    // The standard Bind
    member builder.Bind(x1: M<'T1>, f: 'T1 -> M<'T2>) : M<'T2> = f <| Unwrap x1

    //
    // Monadic optimization
    //

    // If you have MergeSources, then a Bind2 can be added for performance
    member builder.Bind2(x1: M<'T1>, x2: M<'T2>, f: 'T1 * 'T2 -> M<'T3>) : M<'T3> = 
        f (Unwrap x1, Unwrap x2)
