module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Hangman
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { gameStage : GameStage
    , gameState : Hangman.GameState
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { gameStage = StartingGame
      , gameState = Hangman.initGameState
      }
    , Effect.none
    )



-- UPDATE


type GameStage
    = StartingGame
    | PlayingGame
    | GameOver


type Msg
    = NoOp
    | UserStartedGame
    | UserGuessed Char


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        UserStartedGame ->
            ( { model | gameStage = PlayingGame }, Effect.none )

        UserGuessed letter ->
            let
                updatedGameState =
                    Hangman.guessedLetter letter model.gameState
            in
            ( { gameStage =
                    if Hangman.isGameOver updatedGameState then
                        GameOver

                    else
                        PlayingGame
              , gameState = Hangman.guessedLetter letter model.gameState
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    case model.gameStage of
        StartingGame ->
            { title = "Hangman"
            , body =
                [ section
                    [ Attr.class "hero is-fullheight"
                    ]
                    [ div
                        [ Attr.class "hero-body"
                        ]
                        [ div
                            [ Attr.class ""
                            ]
                            [ p
                                [ Attr.class "title"
                                ]
                                [ text "Hangman for Angie P." ]
                            , p
                                [ Attr.class "subtitle"
                                ]
                                [ text "Guess the right letters and you will know the magic word" ]
                            , div [ Attr.class "buttons" ]
                                [ button
                                    [ Attr.class "button is-success"
                                    , onClick UserStartedGame
                                    ]
                                    [ text "Start the Game" ]
                                ]
                            ]
                        ]
                    ]
                ]
            }

        PlayingGame ->
            let
                gameOver =
                    Hangman.isGameOver model.gameState

                wordCompleted =
                    Hangman.isGameWon model.gameState
            in
            { title = "Hangman"
            , body =
                [ section
                    [ Attr.class "hero is-fullheight"
                    ]
                    [ div
                        [ Attr.class "hero-body"
                        ]
                        [ div
                            [ Attr.class ""
                            ]
                            [ p
                                [ Attr.class "title"
                                ]
                                [ text "Hangman for Angie P." ]
                            , p
                                [ Attr.class "subtitle"
                                ]
                                [ text "Guess the right letters and you will know the magic word" ]
                            , displayWord (Hangman.gameWord model.gameState) (Hangman.guessedLetters model.gameState)
                            , if gameOver then
                                if wordCompleted then
                                    p [ Attr.class "has-text-success is-size-3" ] [ text "🎉 Congratulations! You won! 🎉" ]

                                else
                                    p [ Attr.class "has-text-danger is-size-3" ] [ text "💀 Game Over! Try Again! 💀" ]

                              else
                                displayLetterButtons (Hangman.guessedLetters model.gameState)
                            , displayRemainingAttempts (Hangman.remainingAttempts model.gameState)
                            ]
                        ]
                    ]
                ]
            }

        GameOver ->
            { title = "Hangman"
            , body =
                [ section
                    [ Attr.class "hero is-fullheight"
                    ]
                    [ div
                        [ Attr.class "hero-body"
                        ]
                        [ div
                            [ Attr.class ""
                            ]
                            [ p
                                [ Attr.class "title"
                                ]
                                [ text "You lost the game!" ]
                            , p
                                [ Attr.class "subtitle"
                                ]
                                [ text "Press the button to start again" ]
                            ]
                        ]
                    ]
                ]
            }


alphabet : String
alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


letterButton : List Char -> Char -> Html.Html Msg
letterButton guessedLetters letter =
    let
        isGuessed =
            List.member letter guessedLetters

        btnClass =
            if isGuessed then
                "button is-static"

            else
                "button is-success"
    in
    button
        [ Attr.class btnClass
        , onClick (UserGuessed letter)
        ]
        [ text (String.fromChar letter) ]


displayLetterButtons : List Char -> Html.Html Msg
displayLetterButtons guessedLetters =
    div
        [ Attr.class "buttons" ]
        (List.map (letterButton guessedLetters) (String.toList alphabet))


displayWord : String -> List Char -> Html.Html msg
displayWord word guessedLetters =
    let
        currentWord =
            List.map (displayChar guessedLetters) (String.toList word)
                |> List.map characterView
    in
    p [ Attr.class "is-size-3" ] currentWord


isLetterGuessed : List Char -> Char -> Bool
isLetterGuessed guessedLetters letter =
    List.member letter guessedLetters


displayChar : List Char -> Char -> Char
displayChar alreadyGuessedLetters letter =
    if List.member letter alreadyGuessedLetters then
        letter

    else
        '_'


characterView : Char -> Html msg
characterView letter =
    span [ Attr.class "mr-4" ] [ text (String.fromChar letter) ]


displayRemainingAttempts : Int -> Html msg
displayRemainingAttempts remainingAttempts =
    p
        [ Attr.class "is-size-4" ]
        [ text ("Remaining attempts: " ++ String.fromInt remainingAttempts) ]
