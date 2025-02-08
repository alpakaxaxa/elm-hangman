module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (GameState, newGame)
import Test exposing (..)


testGameInitialization : Test
testGameInitialization =
    test "Game init with the correct word and attempts" <|
        \_ ->
            let
                game =
                    newGame "elm"
            in
            Expect.equal
                { word = "elm"
                , guessedLetters = []
                , remainingAttempts = 6
                }
                game
