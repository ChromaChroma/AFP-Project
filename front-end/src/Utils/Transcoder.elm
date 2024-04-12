module Utils.Transcoder exposing (..)

import Json.Decode as Decode exposing (Decoder, map7, field, string, list)
import Json.Encode as Encode 
import File exposing (File)
import Utils.Types           exposing (..)


{- ENCODERS
    These functions are used to parse Elm values into JSON data.

    Encode.Value represents a JS Value that can be serialized into JSON.
-}

{-| This function encodes the credential data type into a JS Value.
-}
credEncoder : Cred -> Encode.Value
credEncoder cred = 
    Encode.object 
        [("access", Encode.string cred.access )
        ,("refesh", Encode.string cred.refresh)
        ]

{-| This function encodes a user into a JS Value.
-}
userEncoder : User -> Encode.Value
userEncoder user = 
    Encode.object 
        [("username", Encode.string user.username)
        ,("password", Encode.string user.password)
        ]

{-| This function encodes submission data into a JSON object.
-}
submissionEncoder : String -> Encode.Value
submissionEncoder file =
    Encode.object 
        [("code", Encode.string file)]


{- DECODERS
    These functions are used to parse JSON data into Elm values.
-}

{-| This function decodes JSON data into the credentials data type.
-}
credDecoder : Decoder Cred
credDecoder =
  Decode.map2 Cred
    (Decode.field "access"  Decode.string)
    (Decode.field "refresh" Decode.string)

{-| This function decodes JSON data into the coding problem difficulty data type.
-}
difficultyDecoder : Decoder ProblemDifficulty
difficultyDecoder =
    Decode.string |> Decode.andThen
        (\str ->
            case str of
                "Easy"         -> 
                    Decode.succeed Easy

                "Intermediate" -> 
                    Decode.succeed Intermediate

                "Difficult"    -> 
                    Decode.succeed Difficult

                "Extreme"      -> 
                    Decode.succeed Extreme

                _              -> 
                    Decode.fail ("Invalid ProblemDifficulty: " ++ str)
        )

{-| This function decodes JSON data into the coding problem data type.
-}
codeProblemDecoder : Decoder CodingProblem
codeProblemDecoder =
  Decode.map7 CodingProblem
    (field "_id"          string           )
    (field "deadline"     string           )
    (field "problemTags"  (list string)    )
    (field "difficulty"   difficultyDecoder)
    (field "title"        string           )
    (field "description"  string           )
    (field "templateCode" string           )
    -- (field "testCases" (list string))

{-| This function decodes JSON data into a List of files.
-}
filesDecoder : Decoder (List File)
filesDecoder =
  Decode.at ["target","files"] (Decode.list File.decoder)