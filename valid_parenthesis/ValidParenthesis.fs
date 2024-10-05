
module ValidParenthesesTests
open Xunit
open FsUnit.Xunit

let isValid (s: string) : bool =
    let isOpening c =
        match c with
        | '{' -> true
        | '[' -> true
        | '(' -> true
        | _ -> false

    let canContinue current next =
        match current, next with
        | '{', ']'
        | '{', ')' 
        | '[', '}' 
        | '[', ')' 
        | '(', '}' 
        | '(', ']' -> false
        | _, _ -> true


    let rec reduceParentheses currentIndex stack =
        if(currentIndex = (s.Length - 1)) then stack = ""
        else    
                let stackPop = if stack = "" then s[currentIndex] else stack[stack.Length - 1]
                let nextChar = s[currentIndex + 1]
                if (not (canContinue stackPop nextChar)) then false
                else
                    if (isOpening nextChar) then reduceParentheses (currentIndex + 1) (stack + nextChar.ToString())
                    else
                        let newStack = if stack <> "" then stack.Remove(stack.Length - 1) else ""
                        reduceParentheses (currentIndex + 1) newStack
                        
    match s.Length with
    | 0 -> true
    | 1 -> false
    | n when n % 2 <> 0 -> false
    | _ -> reduceParentheses 0 ""
    


[<Fact>]
let ``Unmatched closing parenthesis returns false`` () =
    isValid "())" |> should equal false


[<Fact>]
let ``Empty string should return true`` () =
    isValid "" |> should equal true

[<Fact>]
let ``Single pair of parentheses returns true`` () =
    isValid "()" |> should equal true

[<Fact>]
let ``Single pair of different types returns true`` () =
    isValid "()[]{}" |> should equal true

[<Fact>]
let ``Mismatched parentheses returns false`` () =
    isValid "(]" |> should equal false

[<Fact>]
let ``Nested parentheses returns true`` () =
    isValid "([])" |> should equal true

[<Fact>]
let ``Incorrectly nested parentheses returns false`` () =
    isValid "([)]" |> should equal false

[<Fact>]
let ``Unmatched opening parenthesis returns false`` () =
    isValid "(()" |> should equal false



[<Fact>]
let ``Long sequence of valid parentheses returns true`` () =
    isValid "([]{})" |> should equal true

[<Fact>]
let ``Long sequence with a single mismatch returns false`` () =
    isValid "([]{}[)" |> should equal false
