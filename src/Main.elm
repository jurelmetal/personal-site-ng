module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Browser exposing (Document)

main : Program () Model Msg
main = Browser.document
    {
        init = init,
        view = view,
        update = update,
        subscriptions = (\_ -> Sub.none)
    }

-- Base model for the page
type alias Model = { currentPage : Page }
type Page = Main | Contact
type Msg = NoOp | ChangePage Page


init : () -> ( Model, Cmd Msg )
init _ = ( Model Main, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        NoOp -> ( model, Cmd.none )
        ChangePage page -> ( { model | currentPage = page}, Cmd.none )

myButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
myButton attrs content =
    button ((class "button") :: attrs) content

view : Model -> Document Msg
view model = Document "Pagina" [
    div [ class "container" ] [
            case model.currentPage of
                Main -> drawMainPage
                Contact -> drawContactPage
        ]
    ]



drawMainPage : Html Msg
drawMainPage = div [ class "columns" ] [
    div [ class "column" ] [ text "First" ],
    div [ class "column" ] [ text "Second" ],
    div [ class "column" ] [ text "Third" ],
    div [ class "column" ] [
            myButton [ onClick (ChangePage Contact) ] [ text "To Contact" ]
        ]
    ]

drawContactPage : Html Msg
drawContactPage = div [ class "columns" ] [
    div [ class "column"] [
            myButton [ onClick (ChangePage Main)] [ text "Press Me" ]
        ]
    ]
