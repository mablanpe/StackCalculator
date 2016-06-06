module SCCore.Tests.Main

open Xunit
open Swensen.Unquote

open SCCore.Main

[<Fact>]
let ``Should add 2 to 2 and return 4`` () =
    test <@ add 2 2 = 4 @>