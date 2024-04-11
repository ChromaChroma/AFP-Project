module Utils.Transcoder exposing (..)

import Json.Decode as Decode exposing (Decoder, map7, field, string, list)
import Json.Encode as Encode 
import Utils.Types exposing (..)

-- import Utils.Transcoder exposing (..)

credEncoder : Cred -> Encode.Value
credEncoder cred = 
    Encode.object 
        [("access", Encode.string cred.access)
        ,("refesh", Encode.string cred.refresh)
        ]

userEncoder : User -> Encode.Value
userEncoder user = 
    Encode.object 
        [("username", Encode.string user.username)
        ,("password", Encode.string user.password)
        ]


credDecoder : Decoder Cred
credDecoder =
  Decode.map2 Cred
    (Decode.field "access" Decode.string)
    (Decode.field "refresh" Decode.string)


difficultyDecoder : Decoder ProblemDifficulty
difficultyDecoder =
    Decode.string |> Decode.andThen -- Decode.customDecoder
        (\str ->
            case str of
                "Easy" -> Decode.succeed Easy
                "Intermediate" -> Decode.succeed Intermediate
                "Difficult" -> Decode.succeed Difficult
                "Extreme" -> Decode.succeed Extreme
                _ -> Decode.fail ("Invalid ProblemDifficulty: " ++ str)
        )

codeProblemDecoder : Decoder CodingProblem
codeProblemDecoder =
  Decode.map7 CodingProblem
    (field "_id" string)
    (field "deadline" string)
    (field "problemTags" (list string))
    (field "difficulty" difficultyDecoder)
    (field "title" string)
    (field "description" string)
    (field "templateCode" string)
    -- (field "testCases" (list string))


submissionEncoder : String -> Encode.Value
submissionEncoder file =
    Encode.object 
        [("code", Encode.string file)]