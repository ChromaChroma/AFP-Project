module Page.Problem exposing ()

-- ! OLD TEST



-- module Page.Problem exposing (Model,Msg,init,view,update)

-- import Browser
-- import Html exposing (..)
-- import Html.Attributes exposing (class)
-- import Html.Events exposing (onClick)
-- import Http
-- import Json.Decode as D exposing (Decoder, map8, field, string, list)

-- -- MAIN

-- -- main : Html msg
-- -- main =
-- --     div [ class ".myDiv" ] [ text "This is a styled div." ]

-- -- main : Program () Model Msg
-- -- main =
-- --   Browser.element
-- --     { init          = init
-- --     , update        = update
-- --     , subscriptions = subscriptions
-- --     , view          = view
-- --     }

-- -- MODEL

-- type Model
--   = Failure
--   | Loading
--   | Success CodeProblem

-- type ProblemDifficulty 
--   = Easy 
--   | Intermediate 
--   | Difficult 
--   | Extreme

-- type alias CodeProblem =
--   { id           : String
--   , deadline     : String
--   , problemTags  : List String
--   , difficulty   : ProblemDifficulty
--   , title        : String
--   , description  : String
--   , testCases    : List String
--   , templateCode : String
--   }

-- init : () -> (Model, Cmd Msg)
-- init _ = (Loading, getCodeProblem)

-- -- UPDATE

-- type Msg
--   = NoMsg
--   | GotCodeProblem (Result Http.Error CodeProblem)

-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg _ =
--   case msg of
--     NoMsg                 -> (Loading, Cmd.none)
--     GotCodeProblem result ->
--       case result of
--         Ok  problem -> (Success problem, Cmd.none)
--         Err _       -> (Failure, Cmd.none)

-- -- SUBSCRIPTIONS

-- subscriptions : Model -> Sub Msg
-- subscriptions _ = Sub.none

-- -- VIEW

-- view : Model -> Html Msg
-- view model =
--   div [ class "container"]
--     [ h2 [] [ text "Coding Problem:" ]
--     , viewProblem model
--     ]


-- viewProblem : Model -> Html Msg
-- viewProblem model =
--   case model of
--     Failure         -> div [] [text "Failed to load problem case! "]
--     Loading         -> text "Loading..."
--     Success problem ->
--       div []
--         [ h1 [] [ text problem.title ]
--         , p [] [ text problem.description ]
--         ]

-- -- HTTP

-- getCodeProblem : Cmd Msg
-- getCodeProblem =
--     Http.get
--     { url = "http://local-host:8000/problems/p1"
--     , expect = Http.expectJson GotCodeProblem codeProblemDecoder
--     }


-- difficultyDecoder : Decoder ProblemDifficulty
-- difficultyDecoder =
--     D.string |> D.andThen (\str ->
--         case str of
--             "Easy" -> D.succeed Easy
--             "Intermediate" -> D.succeed Intermediate
--             "Difficult" -> D.succeed Difficult
--             "Extreme" -> D.succeed Extreme
--             _ -> D.fail "Invalid difficulty"
--     )

-- codeProblemDecoder : Decoder CodeProblem
-- codeProblemDecoder =
--   map8 CodeProblem
--     (field "id" string)
--     (field "deadline" string)
--     (field "problemTags" (list string))
--     (field "difficulty" difficultyDecoder)
--     (field "title" string)
--     (field "description" string)
--     (field "testCases" (list string))
--     (field "templateCode" string)




