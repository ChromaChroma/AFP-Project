module Utils.Route exposing ( parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url                       exposing (Url)
import Url.Parser                exposing (..)
import Utils.Types               exposing (..)

{- Routing
    These function handle routing updates.
-}

{-| This function parses the given url into a Route,
    it routes to the no found page when the route is invalid.
-}
parseUrl : Url -> Route
parseUrl url =
    case parse match url of
        Just route -> 
            route

        Nothing    ->
            PageNotFoundRoute

{-| This function parses the route and returns it when the route is valid,
    otherwise it returns Nothing.
-}
match : Parser (Route -> a) a
match = 
    oneOf
        [ map LoginRoute         top
        , map LoginRoute         (s "login")
        , map CodingProblemRoute (s "codeproblem")
        ]

{-| This function updates the current Url with the new route.
-}
pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToStr route
        |> Nav.pushUrl navKey

{-| This function converts a route into a tring.
-}
routeToStr : Route -> String
routeToStr route =
    case route of
        PageNotFoundRoute ->
            "/page-not-found"

        LoginRoute        ->
            "/login"

        CodingProblemRoute  ->
            "/codeproblem"