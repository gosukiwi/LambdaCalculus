Lambda Calculus Interpreter

This is a tiny lambda calculus interpreter written in FSharp. Example usage:

		Interpreter.tryEval (Parser.parse "(lambda x. x)")
