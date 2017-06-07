// This is a tiny lambda calculus interpreter.
//
// - You can define a function like this: `(lambda x. x)'
// - For multiple arguments, you can curry functions: `(lambda x. (lambda y. x y))'
// - Only lowercase letters. 
// - To apply functions just put two expression together: `(lambda x. x) y' or `a b`.

// MAYBE DO:
// - `suc' built-in function, returns the successive number
// - add numbers

// Parser for our tiny lambda calculus interpreter. Grammar:
// Expression := (Expression Expression)
//             | (lambda arg. Expression)
//             | Name
// Name       := [a-z]+
//
module Parser =
    open FParsec

    type Node = 
    | NameNode of string
    | FunctionNode of (Node * Node) // name * body
    | ApplicationNode of (Node * Node) // expr * expr

    // This allows for functions to be written `(lambda x y. x)' rather than `(lambda x. (lambda y. x))'
    let rec curryFunc args body =
        match args with
        | [single] -> FunctionNode(single, body)
        | head :: tail -> FunctionNode(head, curryFunc tail body)
        | [] -> body

    let pexpression, pexpressionImpl = createParserForwardedToRef()
    let pname            = many1 (satisfy (fun c -> c |> isLetter || c = '\'' || c |> isDigit )) |>> Seq.toArray |>> System.String |>> NameNode
    let popen            = pchar '('
    let pclose           = pchar ')'
    let pspace           = pchar ' '
    let optspace         = opt (many pspace)
    let plambda          = pstring "lambda" .>> optspace
    let pdot             = between optspace optspace (pchar '.')
    let pargs            = many1 (pname .>> optspace)
    let pfundef          = pipe4 plambda pargs pdot pexpression (fun _ args _ body -> curryFunc args body)
    let papplication     = pipe3 pexpression (many pspace) pexpression (fun e1 _ e2 -> ApplicationNode(e1, e2))
    do pexpressionImpl  := (attempt (between popen pclose pexpression)) <|> 
                           (attempt (between popen pclose papplication)) <|> 
                           (attempt (between popen pclose pfundef)) <|> 
                           pname

    //let test p str =
    //    match run p str with
    //    | Success(result, _, _) -> printfn "Success %A" result
    //    | Failure(errorMsg, _, _) -> printfn "Failure %s" errorMsg

    //test pexpression "((lambda x y. (y x))((lambda z .z) q))"
    //test pexpression "((lambda x. x) b)"
    //test pexpression "(lambda x. y)"
    //test pexpression "(lambda x. y)(lambda y.y)"
    //test pexpression "(lambda x. y z)"
    //test pexpression "(lambda x. y (lambda z. z))"

    let parse input = 
        match run pexpression input with
        | Success(result, _, _) -> Some(result)
        | Failure(msg, _, _)    -> None

module Interpreter =
    open Parser

    // Replaces all occurences of a with b in the given expression
    let rec replace a b expr =
        match expr with
        | NameNode(_) ->
            if expr = a then b
            else expr
        // ((lambda x. (lambda y. x)) z) => (lambda y. z)
        | FunctionNode(arg, body) ->
            FunctionNode(arg, replace a b body)
        // replace x for q in (x y)
        | ApplicationNode(lhs, rhs) ->
            let result = ApplicationNode(replace a b (apply lhs), replace a b (apply rhs))
            apply result
            
    and apply ast =
        // Run some expresion with a name node
        // Eg: `<something> x'
        let rec applyName expr name =
            match expr with
            // Eg: (y x)
            | Parser.NameNode(_) -> ApplicationNode(expr, name)
            // Eg: (lambda y. y) x
            | Parser.FunctionNode(arg, body) -> 
                replace arg name body
            // Eg: ((lambda z. z) (lambda y. y)) x
            | Parser.ApplicationNode(lhs, rhs) ->
                applyName (apply ast) name

        let rec applyFunction fn argument =
            match fn with
            //  fn   arg
            // (x <anything>)
            | NameNode(_) -> ApplicationNode(fn, argument)
            //         fn              arg
            // (lambda x. <anything>) <foo>
            | FunctionNode(arg, body) -> replace arg argument body
            //      fn       arg
            // (<foo><bar>) <baz>
            | ApplicationNode(afn, arg) -> applyFunction argument (applyApplication afn arg)

        and applyApplication fn argument =
            match apply fn with
            // x <whatever> cannot be applied so it returns itself
            | NameNode(_) -> ApplicationNode(fn, argument)
            // <fn> <argument>
            | _ -> applyFunction (apply fn) (apply argument)

        match ast with
        | Parser.ApplicationNode(lhs, rhs) -> applyApplication lhs rhs
        | Parser.FunctionNode(_, _)    -> ast
        | Parser.NameNode(name)        -> ast

    let rec asString node =
        match node with
        | NameNode(name) -> name
        | FunctionNode(arg, body) -> sprintf "(\\%s. %s)" (asString arg) (asString body)
        | ApplicationNode(lhs, rhs) -> sprintf "(%s %s)" (asString lhs) (asString rhs)

    let tryEval ast = 
        match ast with
        | Some(validAST) -> Some(asString (apply validAST))
        | None           -> None

open Parser
open Interpreter

[<EntryPoint>]
let main argv =
    //let input = "(((lambda x. x) (lambda y. y)) z)"
    //let input = "((lambda x. (lambda y. x)) q)"
    //let input = "((lambda x. (lambda y. (lambda z. x))) q)"
    let input = "(((lambda x y.(y x))(((((lambda x2 y2. (y2 x2))(((lambda x3.x3) q)))) (lambda x4.x4))))(lambda x5.x5))"
    //let input = "((lambda x y. (y x)) (((lambda z. z) q)))"
    match Interpreter.tryEval (Parser.parse input) with
    | Some(result) -> 
        printfn "Input: %s" input
        printfn "Output: %s" result
    | None         -> printfn "Could not evaluate:\n%s" input
    System.Console.ReadLine()
    0 // return an integer exit code