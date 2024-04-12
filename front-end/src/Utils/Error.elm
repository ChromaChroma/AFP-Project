module Utils.Error exposing (errorToStr)

import Http
import String exposing(fromInt)


-- HTTP ERROR MESSAGES

{-| This function converts a Http error messages to a string.
-}
errorToStr : Http.Error -> String
errorToStr error =
    case error of
        Http.BadUrl msg       ->
            msg

        Http.Timeout          -> 
            "Server timeout error!"

        Http.NetworkError     ->
            "Network error, cannot reach server!"

        Http.BadStatus status ->
            "BadStatus error, request failed with status: " ++ fromInt status

        Http.BadBody msg      ->
            "Bad body error, message: " ++ msg