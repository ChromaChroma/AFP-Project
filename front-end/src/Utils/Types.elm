module Utils.Types exposing (..)

import Browser.Navigation as Nav
import Http


-- CODING PROBLEM TYPES


type ProblemDifficulty 
  = Easy 
  | Intermediate 
  | Difficult 
  | Extreme


type alias CodingProblem =
  { id           : String
  , deadline     : String
  , problemTags  : List String
  , difficulty   : ProblemDifficulty
  , title        : String
  , description  : String
--   , testCases    : List String
  , templateCode : String
  }


-- USER TYPES


type alias User =
  { username : String
  , password : String
  }


type Field
  = Username
  | Password


-- PAGES / MODEL TYPES


type Page 
    = NotFoundPage
    | LoginPage         LoginModel
    | CodingProblemPage CodeProblemModel 


type alias LoginModel =
  { session : Session 
  , user    : User
  }


type Status 
  = Failure
  | Loading
  | Success CodingProblem


type alias CodeProblemModel =
    { session            : Session
    , state              : Status
    , uploadedSubmission : Maybe String
    }


type alias MainModel =
    { route   : Route
    , page    : Page
    , session : Session
    }


-- ROUTING TYPES


type Route 
    = PageNotFoundRoute
    | LoginRoute
    | CodingProblemRoute


-- SESSION TYPES 


type alias Cred =
    { access  : String
    , refresh : String   
    }


type Session
    = Authenticated   Nav.Key Cred
    | Unauthenticated Nav.Key


-- AUXILIARY FUNCTIONS


problemDifficultyToStr : ProblemDifficulty -> String
problemDifficultyToStr difficulty =
    case difficulty of
        Easy         ->
            "Easy"

        Intermediate ->
            "Intermediate"

        Difficult    ->
            "Difficult"

        Extreme      ->
            "Extreme"