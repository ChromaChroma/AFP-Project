module Page.CodeProblem exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, map8, field, string, list)

-- MODEL


type Model 
  = Failure
  | Loading
  | Success CodeProblem

type ProblemDifficulty 
  = Easy 
  | Intermediate 
  | Difficult 
  | Extreme

type alias CodeProblem =
  { id           : String
  , deadline     : String
  , problemTags  : List String
  , difficulty   : ProblemDifficulty
  , title        : String
  , description  : String
  , testCases    : List String
  , templateCode : String
  }

init : (Model, Cmd Msg)
init = (Loading, getCodeProblem)


-- UPDATE

type Msg
  = GotCodeProblem (Result Http.Error CodeProblem)

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotCodeProblem (Ok codeProblem) -> (Success codeProblem, Cmd.none)
    GotCodeProblem (Err _) -> (Failure, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "container"]
            [ h2 [] [ text "Coding Problem:" ]
            , viewCodeProblem model
            ]
    }


viewCodeProblem : Model -> Html Msg
viewCodeProblem model =
  case model of
    Failure         -> div [] [text "Failed to load problem case! "]
    Loading         -> text "Loading..."
    Success problem ->
      div []
        [ h1 [] [ text problem.title ]
        , p [] [ text problem.description ]
        ]


-- HTTP

getCodeProblem : Cmd Msg
getCodeProblem =
    Http.get
    { url = "http://local-host:8000/problems/p1"
    , expect = Http.expectJson GotCodeProblem codeProblemDecoder
    }

codeProblemDecoder : Decoder CodeProblem
codeProblemDecoder =
  Decode.map8 CodeProblem -- map8 CodeProblem
    (field "id" string)
    (field "deadline" string)
    (field "problemTags" (list string))
    (field "difficulty" difficultyDecoder)
    (field "title" string)
    (field "description" string)
    (field "testCases" (list string))
    (field "templateCode" string)

-- Decoder for ProblemDifficulty
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
