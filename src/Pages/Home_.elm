module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
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
    { word : String
    , guessedLetters : List Char
    , remainingAttempts : Int
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { word = "BABYGIRL"
      , guessedLetters = []
      , remainingAttempts = 7
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | UserGuessed Char


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        UserGuessed letter ->
            ( { model
                | guessedLetters = letter :: model.guessedLetters
                , remainingAttempts = model.remainingAttempts - 1
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
    let
        wordCompleted =
            List.all (\c -> List.member c model.guessedLetters) (String.toList model.word)

        gameOver =
            model.remainingAttempts == 0
    in
    { title = "Pages.Hangman"
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
                    , displayWord model.word model.guessedLetters
                    , if gameOver then
                        if wordCompleted then
                            p [ Attr.class "has-text-success is-size-3" ] [ text "🎉 Congratulations! You won! 🎉" ]

                        else
                            p [ Attr.class "has-text-danger is-size-3" ] [ text "💀 Game Over! Try Again! 💀" ]

                      else
                        displayLetterButtons model.guessedLetters
                    , displayRemainingAttempts model.remainingAttempts
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
