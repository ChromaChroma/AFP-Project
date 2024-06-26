module Page exposing (view)

import Browser                          exposing (Document)
import Html                             exposing (..)
import Html.Attributes                  exposing (class, classList, href, style)
import Html.Events                      exposing (onClick)
import Page.Login       as Login
import Page.CodeProblem as CodeProblem
import Debug
import Utils.Session    as Session      exposing (isLoggedIn)
import Maybe                            exposing (Maybe(..), withDefault)
import Utils.Types                      exposing (..)


-- VIEW

{-| This function handles the view of a page,
    its body is based on the content of the given input page view.
-}
view : Page -> { title : String, content : Html msg } -> Document msg
view page {title, content} =
    { title = title ++ " - Coding Problems"
    , body  = viewHeader page :: content :: [viewFooter ]
    }

{-| This function defines the header view of the page.
-}
viewHeader : Page -> Html msg
viewHeader page =
    header [ class "header" ]
        [ div [ class "logo" ] [ text "Coding Problems" ]
        , case page of
            CodingProblemPage model ->            
                viewNavMenu (Just model.session) [ "/login" ]

            _                       -> 
                viewNavMenu Nothing []
        ]

{-| This function handles the navigation view of the header.
-}
viewNavMenu : Maybe Session -> List String -> Html msg
viewNavMenu session links =
    let
        loggedIn = case session of
            Just ses -> 
                isLoggedIn ses

            Nothing  ->
                False
    in
    nav [ class "navbar" ]
        [ ul [ class "nav-menu" ]
            (List.map (\link -> li [] [ a [ href link ] [ text (pageNameFromLink loggedIn link) ] ]) links)
        ]

{-| This function configures the link text based on the page link.
-}
pageNameFromLink : Bool -> String -> String
pageNameFromLink loggedIn link =
    case link of
        "/login" -> 
            if loggedIn then
                "Logout" -- TODO: logout should remove stored creds
            else 
                "Login"
        
        _ -> "None"

{-| This function defines the footer view of the page.
-}
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