module Utils.Route exposing ( parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url                       exposing (Url)
import Url.Parser                exposing (..)
import Utils.Types               exposing (..)


parseUrl : Url -> Route
parseUrl url =
    case parse match url of
        Just route -> 
            route

        Nothing    ->
            PageNotFoundRoute


match : Parser (Route -> a) a
match = 
    oneOf
        [ map LoginRoute         top
        , map LoginRoute         (s "login")
        , map CodingProblemRoute (s "codeproblem")
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToStr route
        |> Nav.pushUrl navKey


routeToStr : Route -> String
routeToStr route =
    case route of
        PageNotFoundRoute ->
            "/page-not-found"

        LoginRoute        ->
            "/login"

        CodingProblemRoute  ->
            "/codeproblem"