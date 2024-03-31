port module Page.CodeProblem exposing (Model, Msg, init, subscriptions, update, view, openPlainTextTabPort)

import Browser
import File                  exposing (File)
import Html                  exposing (..)
import Html.Attributes       exposing (attribute, class, disabled, href, id, placeholder, value, type_)
import Html.Events           exposing (onClick, onInput, onSubmit, on)
import Http
import Json.Decode as Decode exposing (Decoder, map7, field, string, list)
import Json.Encode as Encode 


-- MODEL

type alias Model =
    { state              : Status
    , uploadedSubmission : Maybe String -- File
    }

type Status 
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
--   , testCases    : List String
  , templateCode : String
  }

init : (Model, Cmd Msg)
-- init = (Loading, getCodeProblem)
init = ( { state              = Loading
         , uploadedSubmission = Nothing
         }
       , getCodeProblem
       )


-- UPDATE

type Msg
  = GotCodeProblem      (Result Http.Error CodeProblem)
  | Reload
  | DownloadTemplate    String
  | UploadSubmission
  | CompletedSubmission (Result Http.Error String)
  | GotFile             String -- File


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload                           ->
        ( { model | state = Loading }
        , getCodeProblem 
        )

    GotCodeProblem (Ok codeProblem)  -> 
        updateState (\_ -> Success codeProblem ) model 
            |> Debug.log "coding problem received"

    GotCodeProblem (Err tt)          -> 
        updateState (\_ -> Failure ) model 
        |> Debug.log ("error: " ++ Debug.toString tt)

    DownloadTemplate content         -> 
        ( model
        , openPlainTextTabPort content
        )
    UploadSubmission                 -> 
        case model.uploadedSubmission of
            Just file ->
                ( model, submitFile file )   -- TODO:handle this case
            
            Nothing   ->
                ( model, Cmd.none )

    CompletedSubmission (Ok response) ->
        ( model, Cmd.none ) 
            |> Debug.log "yes" -- TODO:handle this case

    CompletedSubmission (Err error)   ->
        ( model, Cmd.none ) |> Debug.log "err"-- TODO:handle this case

    GotFile file                      -> 
        ( { model | uploadedSubmission = Just file }
        , Cmd.none
        )
    -- GotCodeProblem (Ok codeProblem) -> (Success codeProblem, Cmd.none)
    -- GotCodeProblem (Err _) -> (Failure, Cmd.none)

updateState : (Status -> Status) -> Model -> ( Model, Cmd Msg )
updateState transform model =
    ( { model | state = transform model.state }
    , Cmd.none 
    )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title   = "CodeProblem"
    , content =
        div [ class "container.page"]
            [ viewCodeProblem model ]
    }


viewCodeProblem : Model -> Html Msg
viewCodeProblem model =
  case model.state of
    Failure         -> 
        div [] [ text "Failed to load problem case! "
               , button [ onClick Reload ] 
                        [ text " Reload" ]
               ]

    Loading         -> 
        text "Loading..."

    Success problem ->
        div [ class "problem-container" ]
            [ h2 [ class "title" ] [ text problem.title ]
            , ul [ class "problem-tags" ] (List.map tagToHtml problem.problemTags)
            , p  [ class "deadline" ] [ text "Deadline:", text problem.deadline ]
            , p  [ class "difficulty" ] [ text "Difficulty:", text (diffToStr problem.difficulty) ]
            , hr [ class "separator" ] []
            , p  [ class "description" ] [ text problem.description ]
            , text "Download the template file and use it to implement the specified functions: "
            , button [ class "download-button", onClick (DownloadTemplate problem.templateCode) ] [ text "Download" ]
            , text "Upload your answer here: "
            , input
                [ type_ "file"
                , onInput GotFile
                ]
                []
            , button [ class "upload-button", onClick UploadSubmission ] [ text "Submit" ]
            ]

-- Helper functions
tagToHtml : String -> Html Msg
tagToHtml tag =
    li [] [ text tag ]

diffToStr : ProblemDifficulty -> String
diffToStr difficulty =
    case difficulty of
        Easy         ->
            "Easy"

        Intermediate ->
            "Intermediate"

        Difficult    ->
            "Difficult"

        Extreme      ->
            "Extreme"

-- fileDecoder : Decode.Decoder File
-- fileDecoder =
--   Decode.at ["target","file"] File.decoder


-- PORT
port openPlainTextTabPort : String -> Cmd msg

-- HTTP

getCodeProblem : Cmd Msg
getCodeProblem =
    Http.get
    { url = "http://localhost:8080/coding-problems/123"
    , expect = Http.expectJson GotCodeProblem codeProblemDecoder
    }

codeProblemDecoder : Decoder CodeProblem
codeProblemDecoder =
  Decode.map7 CodeProblem
    (field "_id" string)
    (field "deadline" string)
    (field "problemTags" (list string))
    (field "difficulty" difficultyDecoder)
    (field "title" string)
    (field "description" string)
    (field "templateCode" string)
    -- (field "testCases" (list string))

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

submissionEncoder : String -> Encode.Value
submissionEncoder file =
    Encode.object 
        [("code", Encode.string file)]

submitFile : String -> Cmd Msg
submitFile file =
    Http.post
        { body = Http.jsonBody (submissionEncoder file)
        , expect = Http.expectString CompletedSubmission
        , url = "http://localhost:8080/coding-problems/123/attempts"
        }