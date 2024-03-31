module Error exposing (errorToStr)

import Http


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
            "BadStatus error, request failed with status: " ++ String.fromInt status

        Http.BadBody msg      ->
            "Bad body error, message: " ++ msg