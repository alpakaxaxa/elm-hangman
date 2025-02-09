module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Hangman
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import List.Extra as ListExtra
import Maybe exposing (withDefault)
import Page exposing (Page)
import Random
import Route exposing (Route)
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.perform (\_ -> msg) (Task.succeed ())



-- INIT


type alias Model =
    { gameStage : GameStage
    , gameState : Hangman.GameState
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { gameStage = Ready
      , gameState = Hangman.initGameState ""
      }
    , Effect.none
    )



-- UPDATE


type GameStage
    = Ready
    | PlayingGame
    | GameOver
    | GameWon


type Msg
    = GotRandomWord String
    | UserStartedGame
    | UserGuessed Char


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotRandomWord word ->
            ( { model | gameState = Hangman.initGameState word, gameStage = PlayingGame }, Effect.none )

        UserStartedGame ->
            ( { model | gameState = Hangman.initGameState (Hangman.gameWord model.gameState) }, Effect.sendCmd (Random.generate GotRandomWord randomWordGenerator) )

        UserGuessed letter ->
            let
                updatedGameState =
                    Hangman.guessedLetter letter model.gameState
            in
            ( { gameStage =
                    if Hangman.isGameWon updatedGameState then
                        GameWon

                    else if Hangman.isGameOver updatedGameState then
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
        Ready ->
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
                            , displayStartTheGameButton "Start the Game"
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
                            , displayLetterButtons (Hangman.guessedLetters model.gameState)
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
                                [ text "💀 Game Over! Try Again! 💀" ]
                            , displayWord (Hangman.gameWord model.gameState) (Hangman.guessedLetters model.gameState)
                            , p
                                [ Attr.class "subtitle"
                                ]
                                [ text "Press the button to start again" ]
                            , displayStartTheGameButton "Play Again"
                            ]
                        ]
                    ]
                ]
            }

        GameWon ->
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
                                [ text "🎉 Congratulations! You won! 🎉" ]
                            , displayWord (Hangman.gameWord model.gameState) (Hangman.guessedLetters model.gameState)
                            , p
                                [ Attr.class "subtitle"
                                ]
                                [ text "Press the button to start again" ]
                            , displayStartTheGameButton "Play Again"
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


displayStartTheGameButton : String -> Html.Html Msg
displayStartTheGameButton buttonCaption =
    div [ Attr.class "buttons" ]
        [ button
            [ Attr.class "button is-success"
            , onClick UserStartedGame
            ]
            [ text buttonCaption ]
        ]



-- RANDOM WORD GENERATOR


wordsList : List String
wordsList =
    [ "BABYGIRL", "SEESTERN", "NERD" ]


randomWordGenerator : Random.Generator String
randomWordGenerator =
    Random.int 0 (List.length wordsList - 1)
        |> Random.map
            (\index ->
                ListExtra.getAt index wordsList
                    |> withDefault "HELLO"
            )
