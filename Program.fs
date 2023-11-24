(*
Arithmetic Calculator

Parsing and calculating an arithmetic expression usually involves tokenizing an expression and generating an abstract
syntax tree. This calculator works by following these 3 steps:
 - 1) Tokenization
 - 2) Eliminate (solve) parentheses
 - 3) Iterate and solve by order of operations until complete

 Step 1: Tokenization
 Tokenizing the string involves turning it into a series of tokens which identify each part of the expression.
 For example, '1 + 2 * 3' tokenizes to '(Literal(1),Add(),Literal(2),Multiply(),Literal(3))'
 The tokens are unaware of any syntax rules, just used to make the next steps easier (doable, really).
 
 Parentheses can be handled in many different ways. Parentheses will be their own token type, containing a token list.
 So '2*(4+3)' tokenizes to '(Literal(2),Multiply(),Parentheses(Literal(4),Add(),Literal(3))).

 Step 2: Remove parentheses
 This is done in a recursive function - the same used to solve the base expression.
 Parentheses evaluate to a decimal.

 Step 3: Iterate and solve by order of operations until complete
 This is probably the least efficient solution, but it's easy to implement for a beginner.
 Iterate over the expression and solve the first operation of the highest order until it is eventually all solved.
*)

module Calculator =

    (*********************
     Step 1: Tokenization
    *********************)

    // Defines the type of supported operations.
    type OperationType = 
        | Add
        | Subtract
        | Multiply
        | Divide
        | Power

    // Defines a type to represent the tokens in the input expression. 
    type Token =
        | Literal of decimal
        | Operation of OperationType
        | OpenParenthesis
        | CloseParenthesis
        | Parentheses of Token list

    // Tokenize the input string into a list of tokens.
    let tokenize (input: string) =
        let getToken(c: char) =
            match c with
            | '+' -> Operation(Add)
            | '-' -> Operation(Subtract)
            | '*' -> Operation(Multiply)
            | '/' -> Operation(Divide)
            | '^' -> Operation(Power)
            | '(' -> OpenParenthesis
            | ')' -> CloseParenthesis
            | _ -> failwithf "Unexpected character: %c" c

        // Helper function that splits a list of tokens into 2 lists at the last open parenthesis.
        let splitAtParenthesis (tokens: Token list) =
            let rec splitHelper leftAcc remaining =
                match remaining with
                | [] -> failwith "Open parenthesis '(' not found"
                | OpenParenthesis :: tl ->
                    let leftResult = List.rev tl
                    let rightResult = List.rev leftAcc
                    leftResult, rightResult
                | hd :: tl -> splitHelper (hd :: leftAcc) tl

            let left, right = splitHelper [] tokens
            List.rev left, List.rev right

        let mutable parenthesesCount = 0
        let mutable tokens : Token list = [] 
        let mutable literalStr = ""

        for c in input do

            // Check if token is a literal.
            if (match c with | _ when '0' <= c && c <= '9' -> true | '.' -> true | _ -> false) then

                // Append to token string.
                literalStr <- literalStr + string c

            else

                // Check if literal is being tracked.
                if(literalStr <> "") then

                    // Get literal token.
                    let literalToken = Literal(decimal literalStr)

                    // Append to list.
                    tokens <- literalToken :: tokens
                
                    // Untrack literal.
                    literalStr <- ""

                let tokenType = getToken(c)
                match tokenType with
                | OpenParenthesis ->
                    let operandToken = getToken(c)
                    tokens <- operandToken :: tokens
                    parenthesesCount <- parenthesesCount + 1
                | CloseParenthesis ->
                    let outerTokens, innerTokens = splitAtParenthesis tokens
                    let parenthesesToken = Parentheses(innerTokens)
                    tokens <- parenthesesToken :: outerTokens
                    parenthesesCount <- parenthesesCount - 1
                | _ ->
                    let operandToken = getToken(c)
                    tokens <- operandToken :: tokens

        // Add final literal.
        if literalStr <> "" then
            let literalToken = Literal(decimal literalStr)
            tokens <- literalToken :: tokens

        // Verify parentheses.
        if parenthesesCount <> 0 then
            failwith "Expected ')'"

        List.rev tokens

    (*********************
     Step 2 & 3: Solve
    *********************)

    // Define order of operations.
    let getOperationOrder(op: OperationType) =
        match op with
        | Power -> 3
        | Multiply -> 2
        | Divide -> 2
        | Add -> 1
        | Subtract -> 1

    // Perform operation.
    let operate(lh: decimal, op: OperationType, rh: decimal) =
        match op with
        | Add -> lh + rh
        | Subtract -> lh - rh
        | Multiply -> lh * rh
        | Divide -> lh * rh
        | Power -> System.Math.Pow(float lh, float rh) |> decimal

    // Recursive function to solve list of operations.
    let rec solveExpr(expr: Token list): decimal =
    
        // Solve all nested parentheses.
        let removeParentheses(token: Token) = 
            match token with
            | Parentheses inner -> Literal(solveExpr(inner))
            | _ -> token

        // First step: remove parentheses.
        let newExpr = expr |> List.map removeParentheses

        // Track the remaining operations.
        let mutable remaining: Token array = newExpr |> List.toArray

        // Gets a value from a Literal token.
        let extractDecimalValue token =
            match token with
            | Literal value -> value
            | _ -> failwith "Token is not a Literal"

        // Gets an operation from an Operation token.
        let extractOperation token =
            match token with
            | Operation value -> value
            | _ -> failwith "Token is not an Operation"

        // Start with the highest order of operation first.
        let mutable order = 3
        while order > 0 do
            // Track index of current operation.
            let mutable i = 1
            while i < remaining.Length do
                // Check if this operation matches the current order.
                if (getOperationOrder(extractOperation remaining[i]) = order) then
                    let lh = remaining[i-1] |> extractDecimalValue  // left hand
                    let op = remaining[i] |> extractOperation       // operation
                    let rh = remaining[i+1] |> extractDecimalValue  // right hand
                    let newLiteral = Literal(operate(lh, op, rh))   // operate and store literal

                    // Recreate remaining array.
                    let newArray = Array.append(Array.append [| for x in 0 .. i - 2 -> remaining.[x] |] [| newLiteral |]) [| for x in i + 2 .. Array.length remaining - 1 -> remaining.[x] |]
                    remaining <- newArray

                // Move to next operation. If the operation executed, then the next one comes to us and we don't need to increment i.
                else i <- i + 2

            // Move to lower order.
            order <- order - 1

        assert(remaining.Length = 1)
        extractDecimalValue remaining[0]

    let calculate(input: string) = 
        let tokens = tokenize input
        solveExpr tokens

let main() =
    printf "Enter a formula: "
    let input = System.Console.ReadLine()
    printfn "Evaluation: %f" (Calculator.calculate input)

main()
