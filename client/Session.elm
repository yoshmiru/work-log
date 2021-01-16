module Session exposing (Session(..), navKey)


import Browser.Navigation as Nav

import Route exposing (Route)


type Session =
    Guest Nav.Key

--viewer : Session -> Maybe Viewer
--viewer session =
--    case session of
--        LoggedIn _ val ->
--            Just val
--
--        Guest _ ->
--            Nothing

navKey : Session -> Nav.Key
navKey (Guest key) = key
