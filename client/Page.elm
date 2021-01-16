module Page exposing (view)

import Browser exposing (Document)
import Html exposing (Html)

--type Page
--  = Bill 

view : --Page ->
        { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title ++ " - Conduit"
    , body
        = --viewHeader page maybeViewer ::
          [ content ]-- :: [ viewFooter ]
    }
