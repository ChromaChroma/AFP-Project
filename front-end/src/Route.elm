module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url                       exposing (Url)
import Url.Parser                exposing (..)


type Route 
    = PageNotFound
    | Login
    | CodeProblem

parseUrl : Url -> Route
parseUrl url =
    case parse match url of
        Just route -> 
            route

        Nothing    ->
            PageNotFound

match : Parser (Route -> a) a
match = 
    oneOf
        [ map Login top
        , map Login (s "login")
        , map CodeProblem (s "codeproblem")
        ]

pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToStr route
        |> Nav.pushUrl navKey

routeToStr : Route -> String
routeToStr route =
    case route of
        PageNotFound ->
            "/page-not-found"

        Login        ->
            "/login"

        CodeProblem  ->
            "/codeproblem"