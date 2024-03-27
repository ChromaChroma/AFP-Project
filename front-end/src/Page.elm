module Page exposing (Page(..),view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)

type Page 
    = Login
    | Register
    | CodeProblem

view : Page -> { title : String, content : Html msg } -> Document msg
view page {title, content} =
    { title = title ++ " - Coding Problems"
    , body  = viewHeader page :: content :: [viewFooter ]
    }

viewHeader : Page -> Html msg
viewHeader page =
    nav [ class "container" ]
        [ div [ class "left-content" ] [ a [ class "logo-font"] [text "Coding Problems"] ]
        , div [ class "right-content" ] (viewMenu page)
        ]


viewMenu : Page -> List (Html msg)
viewMenu page = [] -- TODO: in the future this should handle the menu bar (like only show sign out option when you are logged in)

viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font"] [ text "Coding Problems: " ]
            , span [ class "attribution" ]
                [ text "A web application where code can be commited to solve coding problems, source code "
                , a [ href "https://github.com/ChromaChroma/AFP-Project/tree/main" ] [ text "AFP Project" ]
                , text ". The project is connected to an Advanced Functional Programming course on Utrecht University."
                ]
            ]
        ]