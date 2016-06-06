module SCCore.Tests.Main

open Xunit
open Swensen.Unquote

open SCCore.Main

module PopPushTests =

    [<Fact>]
    let ``Should wrap and unwrap contents`` () =
        let expectedContents = [1.0;2.0;3.0]
        let newStack = StackContents expectedContents
        
        let (StackContents contents) = newStack
        
        test <@ contents = expectedContents @>

    [<Fact>]
    let ``Should push to stack`` () =
        let emptyStack = StackContents []
        let stackWith1 = push 1.0 emptyStack 
        let stackWith2 = push 2.0 stackWith1
        
        test <@ stackWith1 = StackContents [1.0] @>
        test <@ stackWith2 = StackContents [2.0; 1.0] @>
    
    [<Fact>]
    let ``Should push with DSL syntax`` () =
        let stack123 = EMPTY |> ONE |> TWO |> THREE 
        
        test <@ stack123 = StackContents [3.0; 2.0; 1.0] @>
        
    [<Fact>]
    let ``Should pop from stack`` () =
        let expectedPopped = 3.0
        let expectedStack = EMPTY |> ONE |> TWO
        let stack123 = expectedStack |> THREE
        
        let popped, stack = pop stack123
        
        test <@ popped = expectedPopped @>
        test <@ stack = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when pop from empty stack`` () =
        raises<exn> <@ pop EMPTY @>
        
module AddTests =

    [<Fact>]
    let ``Should add two numbers`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> FIVE
        
        let result = ADD stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should add two numbers in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> FOUR
        
        let result = ADD stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when adding with one-elem stack`` () =
        let stack = EMPTY |> TWO
        
        raises<exn> <@ ADD stack @>
        
    [<Fact>]
    let ``Should raise ex when adding with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ ADD stack @>

module MulTests = 

    [<Fact>]
    let ``Should mul two numbers`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> SIX
        
        let result = MUL stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should mul two numbers in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> THREE
        
        let result = MUL stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when mul'ing with one-elem stack`` () =
        let stack = EMPTY |> TWO
        
        raises<exn> <@ MUL stack @>
        
    [<Fact>]
    let ``Should raise ex when mul'ing with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ MUL stack @>    

module DivTests = 

    [<Fact>]
    let ``Should div two numbers`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> push 1.5
        
        let result = DIV stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should div two numbers in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> push (1.0/3.0)
        
        let result = DIV stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when div'ing with one-elem stack`` () =
        let stack = EMPTY |> TWO
        
        raises<exn> <@ DIV stack @>
        
    [<Fact>]
    let ``Should raise ex when div'ing with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ DIV stack @>    
        
module SubTests = 

    [<Fact>]
    let ``Should sub two numbers`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> ONE
        
        let result = SUB stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should sub two numbers in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> push -2.0
        
        let result = SUB stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when sub'ing with one-elem stack`` () =
        let stack = EMPTY |> TWO
        
        raises<exn> <@ SUB stack @>
        
    [<Fact>]
    let ``Should raise ex when sub'ing with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ SUB stack @>    
                
module NegTests = 

    [<Fact>]
    let ``Should negate a number`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> TWO |> push -3.0
        
        let result = NEG stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should negate a number in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> THREE |> push -1.0
        
        let result = NEG stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should negate a number in one-elem stack`` () =
        let stack = EMPTY |> TWO
        let expectedStack = EMPTY |> push -2.0
        
        let result = NEG stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when negating with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ NEG stack @>    
                
module SquareTests = 

    [<Fact>]
    let ``Should square a number`` () =
        let stack = EMPTY |> TWO |> THREE
        let expectedStack = EMPTY |> TWO |> push 9.0
        
        let result = SQUARE stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should square a number in bigger stack`` () =
        let stack = EMPTY |> TWO |> THREE |> ONE
        let expectedStack = EMPTY |> TWO |> THREE |> ONE
        
        let result = SQUARE stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should square a number in one-elem stack`` () =
        let stack = EMPTY |> push -2.0
        let expectedStack = EMPTY |> FOUR
        
        let result = SQUARE stack
        
        test <@ result = expectedStack @>
        
    [<Fact>]
    let ``Should raise ex when squaring with zero-elem stack`` () =
        let stack = EMPTY
        
        raises<exn> <@ SQUARE stack @>    
                
            