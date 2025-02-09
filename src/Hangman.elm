module Hangman exposing (GameState, gameWord, guessedLetter, guessedLetters, initGameState, isGameOver, isGameWon, remainingAttempts, restartGameState)

import Set exposing (Set)



--  #Opaque Type


type GameState
    = GameState GameStateModel



--  #GameStateModel


type alias GameStateModel =
    { word : String
    , guessedLetters : List Char
    , remainingAttempts : Int
    }



--  #InitGameState


initGameState : String -> GameState
initGameState word =
    GameState
        { word = word
        , guessedLetters = []
        , remainingAttempts = determineInitialRemainingAttempts word
        }



--  ##HelperFunctions


determineInitialRemainingAttempts : String -> Int
determineInitialRemainingAttempts word =
    word
        |> uniqueLetters
        |> List.length


uniqueLetters : String -> List Char
uniqueLetters word =
    word
        |> String.toList
        |> Set.fromList
        |> Set.toList



--  #ReadGameState


guessedLetters : GameState -> List Char
guessedLetters (GameState model) =
    model.guessedLetters


gameWord : GameState -> String
gameWord (GameState model) =
    model.word


isGameOver : GameState -> Bool
isGameOver (GameState model) =
    model.remainingAttempts <= 0


isGameWon : GameState -> Bool
isGameWon (GameState model) =
    List.all (\c -> List.member c model.guessedLetters) (String.toList model.word)


remainingAttempts : GameState -> Int
remainingAttempts (GameState model) =
    model.remainingAttempts



--  #UpdateGameState


guessedLetter : Char -> GameState -> GameState
guessedLetter letter (GameState model) =
    GameState
        { model
            | guessedLetters = letter :: model.guessedLetters
            , remainingAttempts = model.remainingAttempts - 1
        }



--  ##HelperFunctions
--  #RestartGameState


restartGameState : GameState -> GameState
restartGameState (GameState model) =
    GameState
        { model
            | guessedLetters = []
            , remainingAttempts = determineInitialRemainingAttempts model.word
        }



--  #View
