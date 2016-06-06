module SCCore.Main

type Stack = StackContents of float list

let push x (StackContents contents) = 
    StackContents (x::contents)     

let pop (StackContents contents) =
    match contents with 
    | x::xs -> (x, StackContents xs)
    | [] -> failwith "Stack underflow"

let binaryOp f stack =
    let x, stack' = pop stack
    let y, stack'' = pop stack'
    push (f x y) stack''

let unaryOp f stack =
    let x, stack' = pop stack
    push (f x) stack'

let ADD = binaryOp (+)
let MUL = binaryOp (*) 
let DIV = binaryOp (/)
let SUB = binaryOp (-)

let NEG = unaryOp (fun x -> -x)
let SQUARE = unaryOp (fun x -> x*x)

let EMPTY = StackContents []
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0
let SIX = push 6.0

let SHOW stack = 
    let x,_ = pop stack
    printfn "The answer is %f" x
    stack  // keep going with same stack
