module Page.NotFound exposing (view)

import Html            exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)


-- VIEW

{-| This function defines the pages view when a page is not found.
-}
view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content =
        main_ [ class "container" ]
              [ h1 [] [ text "Page Not Found" ] ]
    } 