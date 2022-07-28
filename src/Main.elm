module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, button, section, p, nav, a, footer)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Browser exposing (Document)
import Html exposing (strong)
import Html exposing (h1)

main : Program () Model Msg
main = Browser.document
    {
        init = init,
        view = view,
        update = update,
        subscriptions = (\_ -> Sub.none)
    }

-- Base model for the page
type alias Model = { currentPage : Page, calcDisplay : String }
type alias PageInfo = { title : String }
type Page = Main | FormSubmission | Calculator | APIUsage | Graphics
pages : List Page
pages = [ Main, FormSubmission, Calculator, APIUsage, Graphics ]

pageTitle : Page -> String
pageTitle page = 
    case page of
        Main -> "Main page"
        FormSubmission -> "Form"
        Calculator -> "Calculator"
        APIUsage -> "API Usage"
        Graphics -> "Graphics"

type Operation = Mult | Sum | Minus | Div | Eq
getOpName : Operation -> String
getOpName op = 
    case op of
        Mult -> "*"
        Sum -> "+"
        Minus -> "-"
        Div -> "/"
        Eq -> "="
type Key = OperatorKey Operation | DigitKey Int
type Msg = NoOp | ChangePage Page | CalcInput Key


init : () -> ( Model, Cmd Msg )
init _ = ( Model Main "", Cmd.none )

printValue : a -> Cmd Msg
printValue value = Debug.log "printValue: " value
    |> \_ -> Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        NoOp -> ( model, Cmd.none )
        ChangePage page -> ( { model | currentPage = page}, Cmd.none )
        CalcInput (DigitKey digit) -> ( {model | calcDisplay = (model.calcDisplay ++ String.fromInt digit) }, printValue digit)
        CalcInput (OperatorKey op) -> ( {model | calcDisplay = (model.calcDisplay ++ getOpName op)}, printValue op)

view : Model -> Document Msg
view model = Document "Elm Test Page" [
        div [ class "container" ] [
            drawHeader model,
            drawMenu model,
            section [ class "section" ] [
                drawCurrentPage model
            ],
            drawFooter
        ]
    ]

-- HTML

drawCurrentPage : Model -> Html Msg
drawCurrentPage model = 
    case model.currentPage of
        Main -> drawMainPage
        Calculator -> drawCalculatorPage model
        other -> div [ class "hero"] [
            h1 [] [ text "Under Construction"],
            div [ class "subtitle" ] [
                text "The page ",
                text (pageTitle other),
                text " is not ready yet."
                ]
            ]

myButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
myButton attrs content =
    button ((class "button") :: attrs) content

drawHeader : Model -> Html Msg
drawHeader model = section [ class "hero" ] [
        div [ class "hero-body" ] [
            p [ class "title"] [
                text "Current page: ",
                text (pageTitle model.currentPage)
            ]
        ]
    ]

drawPageNav : Page -> Page -> Html Msg
drawPageNav currentPage page = if currentPage == page then
        div [ class "navbar-item" ] [
            strong [] [ text (pageTitle page) ]
        ]
    else
        a [ class "navbar-item", onClick (ChangePage page)] [
            text (pageTitle page)
        ]

drawMenu : Model -> Html Msg
drawMenu model = nav [ class "navbar" ] [
        div [ class "navbar-menu" ] [
            div [ class "navbar-start" ] (List.map (drawPageNav model.currentPage) pages)
        ]
    ]

drawMainPage : Html Msg 
drawMainPage = div [class "columns"] [
        div [ class "column" ] [ 
            text "Main page"
        ]
    ]

drawContactPage : Html Msg
drawContactPage = div [ class "columns" ] [
    div [ class "column"] [
            myButton [ onClick (ChangePage Main)] [ text "Press Me" ]
        ]
    ]

drawFooter : Html Msg
drawFooter = footer [ class "footer" ] [
        div [ class "content", class "has-text-centered"] [
            p [] [ text "Made with Elm"]
        ]
    ]

-- Calculator page

drawCalculatorPage : Model -> Html Msg
drawCalculatorPage model = div [ class "is-half", class "is-offset-one-quarter", class "column"] [
        div [ class "tile", class "is-ancestor"] [
            div [ class "tile", class "is-vertical"] (drawScreen model :: drawKeyboard),
            div [ class "tile" ] drawOperations
        ]
    ]
    
drawOperations : List (Html Msg)
drawOperations = [
        div [ class "tile", class "is-parent", class "is-vertical"] [
            createOpButton Div,
            createOpButton Mult,
            createOpButton Minus,
            createOpButton Sum,
            createOpButton Eq
        ]
    ]

drawScreen : Model -> Html Msg
drawScreen model = div [ class "tile", class "is-parent" ] [
        div [ class "tile", class "is-child"] [
            text model.calcDisplay
        ]
    ]

drawKeyboard : List (Html Msg)
drawKeyboard = [
        div [ class "tile", class "is-vertical"] [
            div [ class "tile", class "is-parent" ] (List.map createButton (List.range 1 3)),
            div [ class "tile", class "is-parent" ] (List.map createButton (List.range 4 6)),
            div [ class "tile", class "is-parent" ] (List.map createButton (List.range 7 9))
        ]
    ]

createButton : Int -> Html Msg
createButton digit = button [ onClick (CalcInput (DigitKey digit)), class "tile", class "is-child"] [
        text (String.fromInt digit)
    ]

createOpButton : Operation -> Html Msg
createOpButton op = button [ onClick (CalcInput (OperatorKey op)), class "tile", class "is-child"] [
        text (getOpName op)
    ]