module Button exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type LinkAction
    = Href String
    | Click Msg


makeLink : String -> String -> String -> LinkAction -> Html Msg
makeLink type_ icon_ label_ action_ =
    let
        icon =
            span [ class <| "glyphicon glyphicon-" ++ icon_ ] []

        className =
            "btn btn-" ++ type_

        label =
            text <| " " ++ label_
    in
        case action_ of
            Href h ->
                a [ class className, href h ]
                    [ icon
                    , label
                    ]

            Click act ->
                button [ class className, onClick act ]
                    [ icon
                    , label
                    ]


makeLinkDefault =
    makeLink "default"


makeLinkWarning =
    makeLink "warning"


makeLinkInfo =
    makeLink "info"


makeLinkDanger =
    makeLink "danger"


linkRemove h =
    let
        link =
            Href h
    in
        makeLinkDanger "remove" "Remove" link


linkEdit h =
    let
        link =
            Href h
    in
        makeLinkInfo "edit" "Edit" link


linkConnect h =
    let
        link =
            Href h
    in
        makeLinkWarning "link" "Connect" link


linkCancelRemove h =
    let
        link =
            Href h
    in
        makeLinkDefault "thumbs-down" "No, go back" link


btnConfirmRemove h =
    let
        link =
            Click h
    in
        makeLinkDanger "remove" "Yes, Delete this item please" link
