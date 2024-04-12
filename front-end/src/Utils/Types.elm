module Utils.Types exposing (..)

import Browser.Navigation as Nav
import Http


-- CODING PROBLEM TYPES

{-| Represents the different levels of difficulty a coding problem can have.
-}
type ProblemDifficulty 
  = Easy 
  | Intermediate 
  | Difficult 
  | Extreme

{-| Represents a coding problem, it contains all information needed to describe the problem.
-}
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

{-| Represents the user login information in the system.
-}
type alias User =
  { username : String
  , password : String
  }

{-| Represents the different types of input fields in the system.
-}
type Field
  = Username
  | Password


-- PAGES / MODEL TYPES

{-| Represents the different pages in the system that you can navigate to.
-}
type Page 
    = NotFoundPage
    | LoginPage         LoginModel
    | CodingProblemPage CodeProblemModel 

{-| Represents the model of the login page.
-}
type alias LoginModel =
  { session : Session 
  , user    : User
  }

{-| Represents the model of the coding problem page.
-}
type alias CodeProblemModel =
    { session            : Session
    , state              : Status
    , uploadedSubmission : Maybe String
    , submissionState    : String
    }

{-| Represents the state of retrieving the coding problem data,
  which can either be loading, failed to load data, or success.
-}
type Status 
  = Failure
  | Loading
  | Success CodingProblem

{-| Represents the main model that handles the top level actions.
-}
type alias MainModel =
    { route   : Route
    , page    : Page
    , session : Session
    }


-- ROUTING TYPES

{-| Represents a route, that the system can navigate to.
-}
type Route 
    = PageNotFoundRoute
    | LoginRoute
    | CodingProblemRoute


-- SESSION TYPES 

{-| Represents credentials, which are used for authenticated actions. 
-}
type alias Cred =
    { access  : String
    , refresh : String   
    }

{-| Represents whether a session is authenticated or not.
  It contains the current navigation key and credentials.
-}
type Session
    = Authenticated   Nav.Key Cred
    | Unauthenticated Nav.Key


-- AUXILIARY FUNCTIONS

{-| This function converts a coding problem difficulty constructor to a string.
-}
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