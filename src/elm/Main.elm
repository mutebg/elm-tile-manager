module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type TileType
    = Link
    | News


type TileGroupsPosition
    = Main
    | Context


type alias Tile =
    { id : Int
    , created : String
    , modified : String
    , name : String
    , image_url : String
    , type_ : TileType
    , action : String
    , active : Bool
    }


type alias TileGroup =
    { id : Int
    , created : String
    , modified : String
    , name : String
    , slug : String
    , position : TileGroupsPosition
    }


type alias TypeConnection =
    { id : Int
    , created : String
    , modified : String
    , target : Int
    , sort_order : Int
    , nav_group_id : Int
    , nav_tile_id : Int
    , store_id : Int
    }



-- MODEL


type alias Model =
    { tiles : List Tile
    , groups : List TileGroup
    , connections : List TypeConnection
    }


model : Model
model =
    { tiles = []
    , groups = []
    , connections = []
    }



-- UPDATE


type Msg
    = NoOp
    | OpenNewTile
    | SaveNewTile Tile
    | OpenExistingTile Tile
    | SaveExistingTile Tile


update : Msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ tileForm emptyTile
        ]


emptyTile : Tile
emptyTile =
    { id = 0
    , created = ""
    , modified = ""
    , name = ""
    , image_url = ""
    , type_ = Link
    , action = ""
    , active = False
    }


tileForm : Tile -> Html Msg
tileForm tile =
    div []
        [ div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , input [ class "form-control", type_ "text", value tile.name ] []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Image URL" ]
            , input [ class "form-control", type_ "text", value tile.image_url ] []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Action" ]
            , input [ class "form-control", type_ "text", value tile.action ] []
            ]
        , div [ class "form-check" ]
            [ label [ class "form-check-label" ]
                [ input [ class "form-check-input", type_ "checkbox", checked tile.active ]
                    []
                , text "Active"
                ]
            ]
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }
