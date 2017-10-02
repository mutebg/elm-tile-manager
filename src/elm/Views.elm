module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onSubmit)
import Dict
import Select
import Types exposing (..)
import Button exposing (..)


homePage : Model -> Html Msg
homePage model =
    div [ class "row" ]
        [ div [ class "col-md-8" ]
            [ h2 [] [ text "Tiles" ]
            , listTiles model.tiles
            ]
        , div [ class "col-md-4" ]
            [ h2 [] [ text "Pages" ]
            , listGroups model.groups
            ]
        ]


extractIf : Maybe a -> (a -> Html msg) -> Html msg
extractIf data fn =
    data
        |> Maybe.map fn
        |> Maybe.withDefault (text "")


addTilePage : Model -> Html Msg
addTilePage { currentTile } =
    extractIf currentTile (\g -> tileForm g)


addGroupPage : Model -> Html Msg
addGroupPage { currentGroup } =
    extractIf currentGroup (\g -> groupForm g)


connectTilePage : Model -> String -> String -> Html Msg
connectTilePage model tile_id group_id =
    let
        tileConns =
            getTileConns tile_id model.connections

        groupConns =
            getGroupConns group_id model.connections

        groupsDict =
            groupsToDict model.groups

        tilesDict =
            tilesToDict model.tiles

        tilesList =
            tileConns
                |> List.filter (\c -> Dict.get c.nav_group_id groupsDict /= Nothing)
                |> List.sortBy .sort_order
                |> List.map
                    (\c ->
                        case Dict.get c.nav_group_id groupsDict of
                            Just d ->
                                tr []
                                    [ td [] [ text <| showTarget c.target ]
                                    , td [] [ text <| toString c.sort_order ]
                                    , td [] [ text <| toString d.position ]
                                    , td [] [ text <| toString d.name ]
                                    , td [] [ linkRemove <| "#delete/connections/" ++ c.id ]
                                    ]

                            _ ->
                                text ""
                    )

        grpupList =
            groupConns
                |> List.filter (\c -> Dict.get c.nav_tile_id tilesDict /= Nothing)
                |> List.sortBy .sort_order
                |> List.map
                    (\c ->
                        case Dict.get c.nav_tile_id tilesDict of
                            Just d ->
                                tr []
                                    [ td [] [ text <| showTarget c.target ]
                                    , td [] [ text <| toString c.sort_order ]
                                    , td [] [ text <| toString d.name ]
                                    , td [] [ linkRemove <| "#delete/connections/" ++ c.id ]
                                    ]

                            _ ->
                                text ""
                    )
    in
        div [ class "row" ]
            [ div [ class "col-md-8" ]
                [ h2 [] [ text "Connections" ]
                , table [ class "table table-striped table-bordered table-hover" ]
                    (if group_id == "-" then
                        [ thead []
                            [ tr []
                                [ th [] [ text "Target" ]
                                , th [] [ text "Sort" ]
                                , th [] [ text "Position" ]
                                , th [] [ text "Name" ]
                                , th [ style [ ( "width", "100px" ) ] ] [ text "Action" ]
                                ]
                            ]
                        , tbody []
                            tilesList
                        ]
                     else
                        [ thead []
                            [ tr []
                                [ th [] [ text "Target" ]
                                , th [] [ text "Sort" ]
                                , th [] [ text "Name" ]
                                , th [ style [ ( "width", "100px" ) ] ] [ text "Action" ]
                                ]
                            ]
                        , tbody []
                            grpupList
                        ]
                    )
                ]
            , div
                [ class "col-md-4" ]
                [ extractIf model.currentConnection
                    (\conn -> connectionForm model.tiles model.groups conn)
                ]
            ]


header : Html Msg
header =
    nav [ class "navbar navbar-default" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "TileManager" ]
            , ul [ class "nav navbar-nav" ]
                [ li [] [ a [ href "#" ] [ text "Home" ] ]
                , li [] [ a [ href "#tile-add" ] [ text "Add tile" ] ]
                , li [] [ a [ href "#group-add" ] [ text "Add group" ] ]
                ]
            ]
        ]


deleteModal : String -> String -> Html Msg
deleteModal itemType id =
    div [ class "remove-dialog" ]
        [ h1 [] [ text "Are you sure?" ]
        , div [ class "btn-group" ]
            [ linkCancelRemove "#"
            , btnConfirmRemove (DeleteItem itemType id)
            ]
        ]


tileForm : Tile -> Html Msg
tileForm tile =
    let
        selectOptions =
            [ Link, News ]
    in
        div []
            [ div [ class "form-group" ]
                [ label [] [ text "Name" ]
                , input [ class "form-control", type_ "text", value tile.name, onInput (\val -> UpdateTileField (TextInput "name" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Image URL" ]
                , input [ class "form-control", type_ "url", value tile.image_url, onInput (\val -> UpdateTileField (TextInput "image_url" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Action" ]
                , input [ class "form-control", type_ "text", value tile.action, onInput (\val -> UpdateTileField (TextInput "action" val)) ] []
                ]
            , div [ class "form-check" ]
                [ label [ class "form-check-label" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked tile.active, onCheck (\val -> UpdateTileField (Checkbox "active" val)) ]
                        []
                    , text "Active"
                    ]
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Type" ]
                , Select.fromSelected selectOptions (\val -> UpdateTileField (SelectTileType val)) tile.type_
                ]
            , div []
                [ button [ onClick (SaveTile tile), class "btn btn-primary" ] [ text "Save" ]
                ]
            ]


groupForm : TileGroup -> Html Msg
groupForm g =
    let
        selectOptions =
            [ Main, Context ]
    in
        div []
            [ div [ class "form-group" ]
                [ label [] [ text "Name" ]
                , input [ class "form-control", type_ "text", value g.name, onInput (\val -> UpdateGroupField (TextInput "name" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Slug" ]
                , input [ class "form-control", type_ "text", value g.slug, onInput (\val -> UpdateGroupField (TextInput "slug" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Position" ]
                , Select.fromSelected selectOptions (\val -> UpdateGroupField (SelectPosition val)) g.position
                ]
            , div []
                [ button [ onClick (SaveGroup g), class "btn btn-primary" ] [ text "Save" ]
                ]
            ]


connectionForm : List Tile -> List TileGroup -> TileConnection -> Html Msg
connectionForm tiles groups conn =
    let
        targets =
            [ ( "1", "Desktop" ), ( "2", "Mobile" ) ]

        tilesTuple =
            List.map (\t -> ( t.id, t.name )) tiles

        tileValues =
            if conn.nav_tile_id == "" || conn.nav_tile_id == "-" then
                ( "", "---" ) :: tilesTuple
            else
                tilesTuple

        groupTuple =
            List.map (\v -> ( v.id, v.name )) groups

        groupValues =
            if conn.nav_group_id == "" || conn.nav_group_id == "-" then
                ( "", "---" ) :: groupTuple
            else
                groupTuple
    in
        div []
            [ h2 [] [ text "Add connection" ]
            , Html.form [ onSubmit (SaveConnection conn), class "br" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Tiles" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "nav_tile_id" val))
                        , required True
                        ]
                        (tupleToSelectOptions tileValues conn.nav_tile_id)
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Groups" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "nav_group_id" val))
                        , required True
                        ]
                        (tupleToSelectOptions groupValues conn.nav_group_id)
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Sort order" ]
                    , input
                        [ class "form-control"
                        , type_ "number"
                        , value <| toString conn.sort_order
                        , onInput (\val -> UpdateConnectionField (TextInput "sort_order" val))
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Target" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "target" val))
                        ]
                        (tupleToSelectOptions targets (toString conn.target))
                    ]
                , div []
                    [ button [ class "btn btn-primary" ] [ text "Save" ]
                    ]
                ]
            ]


listTiles : List Tile -> Html Msg
listTiles tiles =
    ul [ class "list-group" ] (List.map listTileItem tiles)


listTileItem : Tile -> Html Msg
listTileItem tile =
    li [ class "list-group-item" ]
        [ img [ class "img-rounded", src tile.image_url ] []
        , h4 [] [ text tile.name, span [ class "small" ] [ text (" (" ++ tile.action ++ ")") ] ]
        , (if tile.active then
            span [ class "badge  badge-success" ]
                [ text "active" ]
           else
            span [ class "badge badge-inverse" ]
                [ text "no active" ]
          )
        , div [ class "btn-group" ]
            [ linkEdit <| "#tile-edit/" ++ tile.id
            , linkConnect <| "#connect/" ++ tile.id ++ "/-"
            , linkRemove <| "#delete/tiles/" ++ tile.id
            ]
        ]


listGroups : List TileGroup -> Html Msg
listGroups groups =
    ul [ class "list-group" ] (List.map listGroupItem groups)


listGroupItem : TileGroup -> Html Msg
listGroupItem group =
    li [ class "list-group-item" ]
        [ h4 [] [ text group.name, span [ class "small" ] [ text <| " (" ++ (toString group.position) ++ ")" ] ]
        , div [ class "btn-group" ]
            [ linkEdit <| "#group-edit/" ++ group.id
            , linkConnect <| "#connect/-/" ++ group.id
            , linkRemove <| "#delete/groups/" ++ group.id
            ]
        ]


tupleToSelectOptions : List ( String, String ) -> String -> List (Html Msg)
tupleToSelectOptions values selectedValue =
    values
        |> List.map (\z -> option [ value <| Tuple.first z, selected (selectedValue == (Tuple.first z)) ] [ text <| Tuple.second z ])



-- HELPERS


getTileConns : String -> List TileConnection -> List TileConnection
getTileConns tileId conns =
    conns
        |> List.filter (\c -> c.nav_tile_id == tileId)


getGroupConns : String -> List TileConnection -> List TileConnection
getGroupConns group_id conns =
    conns
        |> List.filter (\c -> c.nav_group_id == group_id)


groupsToDict : List TileGroup -> Dict.Dict String TileGroup
groupsToDict groups =
    groups
        |> List.map (\k -> ( k.id, k ))
        |> Dict.fromList


tilesToDict : List Tile -> Dict.Dict String Tile
tilesToDict tiles =
    tiles
        |> List.map (\k -> ( k.id, k ))
        |> Dict.fromList
