module Review.CurryingTest exposing (all)

import Review.Currying exposing (forbid)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Currying"
        [ test "implicitly imported function, not applied" <|
            \() ->
                """module A exposing (..)
a = List.map
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied" <|
            \() ->
                """module A exposing (..)
a = List.map identity list
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported prefix operator, fully applied" <|
            \() ->
                """module A exposing (..)
a = (+) 1 2
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using one <|" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| list
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using two <|" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| List.map identity <| list
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using three <|" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| List.map identity <| List.map identity <| list
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using one |>" <|
            \() ->
                """module A exposing (..)
a = list |> List.map identity
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using two |>" <|
            \() ->
                """module A exposing (..)
a = list |> List.map identity |> List.map identity
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, fully applied using three |>" <|
            \() ->
                """module A exposing (..)
a = list |> List.map identity |> List.map identity |> List.map identity
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "explicitly imported dependency module function, fully applied" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map identity set
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "let-declared function, fully applied, let with same name and more arguments is declared in an unrelated branch" <|
            \() ->
                """module A exposing (..)
port messageReceiver : (String -> msg) -> Sub msg
a = ( let
        local : Int -> Int -> Int
        local x y = x
      in
      ()
    , let
        local : Int -> Int
        local x = x
      in
      local 0
    )
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "locally declared port, fully applied" <|
            \() ->
                """module A exposing (..)
port messageReceiver : (String -> msg) -> Sub msg
a = ( messageReceiver, messageReceiver identity )
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "locally declared record type alias with single field, fully applied" <|
            \() ->
                """module A exposing (..)
type alias Single = { it : Int }
a = ( Single, Single 0 )
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "locally declared variant with single value, fully applied" <|
            \() ->
                """module A exposing (..)
type SingleChoiceType = Single Int
a = ( Single, Single 0 )
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectNoErrors
        , test "implicitly imported function, curried" <|
            \() ->
                """module A exposing (..)
a = List.map identity
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors [ errorUnder "List.map" ]
        , test "explicitly imported dependency module function, curried" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map identity
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors [ errorUnder "Set.map" ]
        , test "implicitly imported prefix operator, curried" <|
            \() ->
                """module A exposing (..)
a = (+) 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors [ errorUnder "(+)" ]
        , test "locally declared function, curried" <|
            \() ->
                """module A exposing (..)
local : Int -> Int -> Int
local x y = x
a = local 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "local"
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 10 } }
                        ]
        , test "locally declared function, curried using <|" <|
            \() ->
                """module A exposing (..)
local : Int -> Int -> Int
local x y = x
a = local <| 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "local"
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 10 } }
                        ]
        , test "locally declared function, curried using |>" <|
            \() ->
                """module A exposing (..)
local : Int -> Int -> Int
local x y = x
a = 1 |> local
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "local"
                            |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 15 } }
                        ]
        , test "let-declared function, curried" <|
            \() ->
                """module A exposing (..)
a =
    let
        local : Int -> Int -> Int
        local x y = x
    in
    local 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "local"
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 10 } }
                        ]
        , test "locally declared variant, curried" <|
            \() ->
                """module A exposing (..)
type LocalChoiceType
    = Local Int Int
a = Local 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "Local"
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 10 } }
                        ]
        , test "locally declared record type alias, curried" <|
            \() ->
                """module A exposing (..)
type alias Local =
    { x : Int, y : Int }
a = Local 1
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors
                        [ errorUnder "Local"
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 10 } }
                        ]
        , test "prefix operator, curried, inside fully applied call" <|
            \() ->
                """module A exposing (..)
a = List.map ((+) 0) []
"""
                    |> Review.Test.run forbid
                    |> Review.Test.expectErrors [ errorUnder "(+)" ]
        ]


errorUnder : String -> Review.Test.ExpectedError
errorUnder under =
    Review.Test.error
        { message = "Curried function call"
        , details = [ "Not all expected arguments have been provided. Elm will interpret this as as 'partial application', which can be confusing and is also slow. To fix this error, add the missing arguments or use lambda if necessary." ]
        , under = under
        }
