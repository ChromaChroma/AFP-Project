module Page exposing (Page(..),view)

import Browser                         exposing (Document)
import Html                            exposing (..)
import Html.Attributes                 exposing (class, classList, href, style)
import Html.Events                     exposing (onClick)
import Page.Login       as Login
import Page.CodeProblem as CodeProblem
import Debug
import Session                         exposing (Session, isLoggedIn)
import Maybe exposing (Maybe(..), withDefault)

type Page 
    = NotFound
    | Login       Login.Model
    | CodeProblem CodeProblem.Model


-- VIEW

view : Page -> { title : String, content : Html msg } -> Document msg
view page {title, content} =
    { title = title ++ " - Coding Problems"
    , body  = viewHeader page :: content :: [viewFooter ]
    }

-- viewHeader : Page -> Html msg
-- viewHeader page =
--     nav [ class "container-header" ]
--         [ div [ class "left-content" ] [ a [ class "logo-font"] [text "Coding Problems"] ]
--         , div [ class "right-content" ] (viewMenu page)
--         ]

viewHeader : Page -> Html msg
viewHeader page =
    header [ class "header" ]
        [ div [ class "logo" ] [ text "Coding Problems" ]
        , case page of
            CodeProblem model ->            
                viewNavMenu (Just model.session) [ "/login" ]

            _ -> viewNavMenu Nothing []
        ]

viewNavMenu : Maybe Session -> List String -> Html msg
viewNavMenu session links =
    let
        loggedIn = case session of
            Just ses -> 
                isLoggedIn ses
            Nothing ->
                False
    in
    nav [ class "navbar" ]
        [ ul [ class "nav-menu" ]
            (List.map (\link -> li [] [ a [ href link ] [ text (pageNameFromLink loggedIn link) ] ]) links)
        ]

pageNameFromLink : Bool -> String -> String
pageNameFromLink loggedIn link =
    case link of
        "/login" -> 
            if loggedIn then
                "Logout"
            else 
                "Login"
        
        _ -> "None"


viewMenu : Page -> List (Html msg)
viewMenu page = [] -- TODO: in the future this should handle the menu bar (like only show sign out option when you are logged in)

viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "footer" ]
            [ a [ class "logo-font"] [ text "Coding Problems: " ]
            , span [ class "attribution" ]
                [ text "A web application where code can be commited to solve coding problems, source code "
                , a [ href "https://github.com/ChromaChroma/AFP-Project/tree/main" ] [ text "AFP Project" ]
                , text ". The project is connected to an Advanced Functional Programming course on Utrecht University."
                ]
            ]
        ]

-- viewFooter : Html msg
-- viewFooter =
--     footer [ class "footer" ]
--         [ p [] [ text "© 2024 Your Company. All rights reserved." ]
--         , p [] [ text "Terms of Service | Privacy Policy" ]
--         ]