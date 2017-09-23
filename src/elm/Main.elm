module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onSubmit)
import Http
import Select
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipe
import Json.Encode as Encode
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)


-- APP
-- main : Program Never Model Msg
-- main =
--     Html.beginnerProgram { model = model, view = view, update = update }


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( model
    , Cmd.batch [ loadTiles, loadGroups, loadConnections ]
    )


apiBase : String
apiBase =
    "http://localhost:5000/tilesmanager-70e8d/us-central1/api/"


type Page
    = Login
    | Home
    | AddTile
    | EditTile String
    | ConnectTile String String
    | AddGroup
    | EditGroup String
    | ConnectGroup String
    | Delete String String


routeParse : Url.Parser (Page -> a) a
routeParse =
    Url.oneOf
        [ Url.map Home top
        , Url.map AddTile (Url.s "tile-add")
        , Url.map EditTile (Url.s "tile-edit" </> Url.string)
        , Url.map ConnectTile (Url.s "connect" </> Url.string </> Url.string)
        , Url.map AddGroup (Url.s "group-add")
        , Url.map EditGroup (Url.s "group-edit" </> Url.string)
        , Url.map ConnectGroup (Url.s "group-connect" </> Url.string)
        , Url.map Delete (Url.s "delete" </> Url.string </> Url.string)
        ]


type TileType
    = Link
    | News


type TileGroupsPosition
    = Main
    | Context


type alias Tile =
    { id : String
    , created : String
    , modified : String
    , name : String
    , image_url : String
    , type_ : TileType
    , action : String
    , active : Bool
    }


type alias TileGroup =
    { id : String
    , created : String
    , modified : String
    , name : String
    , slug : String
    , position : TileGroupsPosition
    }


type alias TileConnection =
    { id : String
    , created : String
    , modified : String
    , target : Int
    , sort_order : Int
    , nav_group_id : String
    , nav_tile_id : String
    , store_id : Int
    }


type FormMessage
    = TextInput String String
    | Checkbox String Bool
    | SelectPosition TileGroupsPosition
    | SelectTileType TileType


type alias MessageResponse =
    { code : Int
    , message : String
    }


targetDesktop : number
targetDesktop =
    1


targetMobile : number
targetMobile =
    2



-- MODEL


type alias Model =
    { page : Page
    , tiles : List Tile
    , currentTile : Maybe Tile
    , groups : List TileGroup
    , currentGroup : Maybe TileGroup
    , connections : List TileConnection
    , currentConnection : Maybe TileConnection
    }


model : Model
model =
    { page = Home
    , tiles = []
    , currentTile = Nothing
    , groups = []
    , currentGroup = Nothing
    , connections = []
    , currentConnection = Nothing
    }



-- UPDATE


type Msg
    = NoOp
    | UrlChange Navigation.Location
    | NewUrl String
    | SaveTile Tile
    | SaveGroup TileGroup
    | SaveConnection TileConnection
    | DeleteItem String String
    | UpdateTileField FormMessage
    | UpdateGroupField FormMessage
    | UpdateConnectionField FormMessage
    | LoadTiles (Result Http.Error (List Tile))
    | LoadGroups (Result Http.Error (List TileGroup))
    | LoadConnections (Result Http.Error (List TileConnection))
    | ReqSaveTile (Result Http.Error Tile)
    | ReqSaveGroup (Result Http.Error TileGroup)
    | ReqSaveConnection (Result Http.Error TileConnection)
    | ReqDelete String (Result Http.Error MessageResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newPage =
                    case Url.parseHash routeParse location of
                        Just page ->
                            page

                        _ ->
                            Home

                newModel =
                    case newPage of
                        AddTile ->
                            { model | currentTile = Just emptyTile }

                        EditTile id ->
                            { model
                                | currentTile =
                                    model.tiles
                                        |> List.filter (\t -> t.id == id)
                                        |> List.head
                            }

                        ConnectTile tile_id group_id ->
                            let
                                conn =
                                    { emptyConnection | nav_tile_id = tile_id, nav_group_id = group_id }
                            in
                                { model | currentConnection = Just conn }

                        AddGroup ->
                            { model | currentGroup = Just emptyGroup }

                        EditGroup id ->
                            { model
                                | currentGroup =
                                    model.groups
                                        |> List.filter (\t -> t.id == id)
                                        |> List.head
                            }

                        _ ->
                            model

                msgs =
                    case newPage of
                        Home ->
                            [ loadTiles, loadGroups, loadConnections ]

                        _ ->
                            [ Cmd.none ]
            in
                ( { newModel | page = newPage }
                , Cmd.batch msgs
                )

        UpdateTileField message ->
            let
                newTile =
                    case model.currentTile of
                        Just tile ->
                            Just (updateTile tile message)

                        _ ->
                            model.currentTile
            in
                ( { model | currentTile = newTile }, Cmd.none )

        UpdateGroupField message ->
            let
                newGroup =
                    case model.currentGroup of
                        Just g ->
                            Just (updateGroup g message)

                        _ ->
                            model.currentGroup
            in
                ( { model | currentGroup = newGroup }
                , Cmd.none
                )

        UpdateConnectionField message ->
            let
                newConn =
                    case model.currentConnection of
                        Just g ->
                            Just (updateConnection g message)

                        _ ->
                            model.currentConnection
            in
                ( { model | currentConnection = newConn }
                , Cmd.none
                )

        LoadTiles (Ok tiles) ->
            ( { model | tiles = tiles }
            , Cmd.none
            )

        LoadGroups (Ok groups) ->
            ( { model | groups = groups }
            , Cmd.none
            )

        LoadConnections (Ok conns) ->
            ( { model | connections = conns }
            , Cmd.none
            )

        SaveTile tile ->
            ( model, saveTile tile )

        SaveGroup g ->
            ( model, saveGroup g )

        SaveConnection c ->
            ( model, saveConnection c )

        DeleteItem item id ->
            ( model, deleteItem item id )

        ReqSaveTile (Ok tile) ->
            ( model, Navigation.newUrl "#" )

        ReqSaveTile (Err error) ->
            ( model, Debug.log (toString error) Cmd.none )

        ReqSaveGroup (Ok group) ->
            ( model, Navigation.newUrl "#" )

        ReqSaveConnection (Ok conn) ->
            let
                newConn =
                    case model.page of
                        ConnectTile tile_id group_id ->
                            { emptyConnection | nav_tile_id = tile_id, nav_group_id = group_id }

                        _ ->
                            emptyConnection
            in
                ( { model | currentConnection = Just newConn, connections = conn :: model.connections }, Cmd.none )

        ReqSaveConnection (Err error) ->
            ( model, Debug.log (toString error) Cmd.none )

        ReqDelete itemType (Ok msg) ->
            ( model, Navigation.newUrl "#" )

        ReqDelete itemType (Err error) ->
            ( model, Cmd.none )

        _ ->
            ( model
            , Cmd.none
            )


updateTile : Tile -> FormMessage -> Tile
updateTile tile message =
    case message of
        TextInput "name" inputValue ->
            { tile | name = inputValue }

        TextInput "image_url" inputValue ->
            { tile | image_url = inputValue }

        TextInput "action" inputValue ->
            { tile | action = inputValue }

        SelectTileType type_ ->
            { tile | type_ = type_ }

        Checkbox "active" inputValue ->
            { tile | active = inputValue }

        _ ->
            tile


updateGroup : TileGroup -> FormMessage -> TileGroup
updateGroup g message =
    case message of
        TextInput "name" inputValue ->
            { g | name = inputValue }

        TextInput "slug" inputValue ->
            { g | slug = inputValue }

        SelectPosition position ->
            { g | position = position }

        _ ->
            g


updateConnection : TileConnection -> FormMessage -> TileConnection
updateConnection g message =
    case message of
        TextInput "nav_group_id" inputValue ->
            { g | nav_group_id = inputValue }

        TextInput "nav_tile_id" inputValue ->
            { g | nav_tile_id = inputValue }

        TextInput "target" inputValue ->
            { g | target = Result.withDefault 0 (String.toInt inputValue) }

        TextInput "sort_order" inputValue ->
            { g | sort_order = Result.withDefault 0 (String.toInt inputValue) }

        _ ->
            g



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div []
            [ a [ href "#" ] [ text "Home" ]
            , a [ href "#tile-add" ] [ text "Add tile" ]
            , a [ href "#group-add" ] [ text "Add group" ]
            ]
        , case model.page of
            Home ->
                div []
                    [ listTiles model.tiles
                    , hr [] []
                    , listGroups model.groups
                    ]

            AddTile ->
                case model.currentTile of
                    Just g ->
                        tileForm g

                    _ ->
                        text "None"

            EditTile id ->
                case model.currentTile of
                    Just g ->
                        tileForm g

                    _ ->
                        text "None"

            ConnectTile tile_id group_id ->
                let
                    tileConns =
                        getTileConns tile_id model.connections

                    groupsDict =
                        groupsToDict model.groups
                in
                    div []
                        [ ul []
                            (tileConns
                                |> List.filter (\c -> Dict.get c.nav_group_id groupsDict /= Nothing)
                                |> List.map
                                    (\c ->
                                        case Dict.get c.nav_group_id groupsDict of
                                            Just d ->
                                                li []
                                                    [ text
                                                        ("Target: "
                                                            ++ (toString c.target)
                                                            ++ " Sort order:"
                                                            ++ (toString c.sort_order)
                                                            ++ " Position:"
                                                            ++ (toString d.position)
                                                            ++ " Name:"
                                                            ++ (toString d.name)
                                                        )
                                                    , a [ href ("#delete/connections/" ++ c.id) ] [ text "delete" ]
                                                    ]

                                            _ ->
                                                text ""
                                    )
                            )
                        , div []
                            [ case model.currentConnection of
                                Just conn ->
                                    connectionForm model.tiles model.groups conn

                                _ ->
                                    text ""
                            ]
                        ]

            AddGroup ->
                case model.currentGroup of
                    Just g ->
                        groupForm g

                    _ ->
                        text "None"

            EditGroup id ->
                case model.currentGroup of
                    Just g ->
                        groupForm g

                    _ ->
                        text "None"

            Delete itemType id ->
                deleteModal itemType id

            _ ->
                text "No selected page"
        ]


emptyTile : Tile
emptyTile =
    { id = ""
    , created = ""
    , modified = ""
    , name = ""
    , image_url = ""
    , type_ = Link
    , action = ""
    , active = False
    }


emptyGroup : TileGroup
emptyGroup =
    { id = ""
    , created = ""
    , modified = ""
    , name = ""
    , slug = ""
    , position = Main
    }


emptyConnection : TileConnection
emptyConnection =
    { id = ""
    , created = ""
    , modified = ""
    , target = 1
    , sort_order = 0
    , nav_group_id = ""
    , nav_tile_id = ""
    , store_id = 0
    }


deleteModal : String -> String -> Html Msg
deleteModal itemType id =
    div []
        [ h1 [] [ text "Are you sure?" ]
        , a [ href "#", class "btn btn-secondary" ] [ text "No, go back" ]
        , button [ class "btn btn-danger", onClick (DeleteItem itemType id) ] [ text "Yes, Delete this item please" ]
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
        Html.form [ onSubmit (SaveConnection conn) ]
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


listTiles : List Tile -> Html Msg
listTiles tiles =
    ul [] (List.map listTileItem tiles)


listTileItem : Tile -> Html Msg
listTileItem tile =
    li []
        [ h3 [] [ text tile.name ]
        , a [ href <| "#tile-edit/" ++ tile.id ] [ text "Edit" ]
        , a [ href <| "#connect/" ++ tile.id ++ "/-" ] [ text "Connect" ]
        , a [ href <| "#delete/tiles/" ++ tile.id ] [ text "Delete" ]
        ]


listGroups : List TileGroup -> Html Msg
listGroups groups =
    ul [] (List.map listGroupItem groups)


listGroupItem : TileGroup -> Html Msg
listGroupItem group =
    li []
        [ h3 [] [ text group.name ]
        , a [ href <| "#group-edit/" ++ group.id ] [ text "Edit" ]
        , a [ href <| "#connect/-/" ++ group.id ] [ text "Connect" ]
        , a [ href <| "#delete/groups/" ++ group.id ] [ text "Delete" ]
        ]


getTileConns : String -> List TileConnection -> List TileConnection
getTileConns tileId conns =
    conns
        |> List.filter (\c -> c.nav_tile_id == tileId)


groupsToDict : List TileGroup -> Dict.Dict String TileGroup
groupsToDict groups =
    groups
        |> List.map (\k -> ( k.id, k ))
        |> Dict.fromList


tilesDecoder : Decode.Decoder (List Tile)
tilesDecoder =
    Decode.list tileDecoder


tileDecoder : Decode.Decoder Tile
tileDecoder =
    DecodePipe.decode Tile
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "name" Decode.string
        |> DecodePipe.required "image_url" Decode.string
        |> DecodePipe.required "type_" tileTypeDecoder
        |> DecodePipe.required "action" Decode.string
        |> DecodePipe.required "active" Decode.bool


tileEncoder : Tile -> Encode.Value
tileEncoder tile =
    let
        attributes =
            [ ( "id", Encode.string tile.id )
            , ( "name", Encode.string tile.name )
            , ( "image_url", Encode.string tile.image_url )
            , ( "type_", Encode.string <| toString tile.type_ )
            , ( "action", Encode.string tile.action )
            , ( "active", Encode.bool tile.active )
            ]
    in
        Encode.object attributes


tileTypeDecoder : Decode.Decoder TileType
tileTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Link" ->
                        Decode.succeed Link

                    "News" ->
                        Decode.succeed News

                    _ ->
                        Decode.fail <| "Unknown type"
            )


groupsDecoder : Decode.Decoder (List TileGroup)
groupsDecoder =
    Decode.list groupDecoder


groupDecoder : Decode.Decoder TileGroup
groupDecoder =
    DecodePipe.decode TileGroup
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "name" Decode.string
        |> DecodePipe.required "slug" Decode.string
        |> DecodePipe.required "position" groupPositionDecoder


groupPositionDecoder : Decode.Decoder TileGroupsPosition
groupPositionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Main" ->
                        Decode.succeed Main

                    "Context" ->
                        Decode.succeed Context

                    _ ->
                        Decode.fail <| "Unknown type"
            )


groupEncoder : TileGroup -> Encode.Value
groupEncoder g =
    let
        attributes =
            [ ( "id", Encode.string g.id )
            , ( "name", Encode.string g.name )
            , ( "slug", Encode.string g.slug )
            , ( "position", Encode.string <| toString g.position )
            ]
    in
        Encode.object attributes


connectionsDecoder : Decode.Decoder (List TileConnection)
connectionsDecoder =
    Decode.list connectionDecoder


connectionDecoder : Decode.Decoder TileConnection
connectionDecoder =
    DecodePipe.decode TileConnection
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "target" Decode.int
        |> DecodePipe.required "sort_order" Decode.int
        |> DecodePipe.required "nav_group_id" Decode.string
        |> DecodePipe.required "nav_tile_id" Decode.string
        |> DecodePipe.required "store_id" Decode.int


connectionEncoder : TileConnection -> Encode.Value
connectionEncoder g =
    let
        attributes =
            [ ( "id", Encode.string g.id )
            , ( "target", Encode.int g.target )
            , ( "sort_order", Encode.int g.sort_order )
            , ( "nav_group_id", Encode.string g.nav_group_id )
            , ( "nav_tile_id", Encode.string g.nav_tile_id )
            , ( "store_id", Encode.int g.store_id )
            ]
    in
        Encode.object attributes


messageDecoder : Decode.Decoder MessageResponse
messageDecoder =
    DecodePipe.decode MessageResponse
        |> DecodePipe.required "code" Decode.int
        |> DecodePipe.required "message" Decode.string


loadTiles : Cmd Msg
loadTiles =
    let
        url =
            apiBase ++ "tiles"

        request =
            Http.get url tilesDecoder
    in
        Http.send LoadTiles request


loadGroups : Cmd Msg
loadGroups =
    let
        url =
            apiBase ++ "groups"

        request =
            Http.get url groupsDecoder
    in
        Http.send LoadGroups request


loadConnections : Cmd Msg
loadConnections =
    let
        url =
            apiBase ++ "connections"

        request =
            Http.get url connectionsDecoder
    in
        Http.send LoadConnections request


saveTile : Tile -> Cmd Msg
saveTile tile =
    let
        url =
            apiBase ++ "tiles"

        body =
            tileEncoder tile |> Http.jsonBody

        request =
            Http.post url body tileDecoder
    in
        Http.send ReqSaveTile request


saveGroup : TileGroup -> Cmd Msg
saveGroup g =
    let
        url =
            apiBase ++ "groups"

        body =
            groupEncoder g |> Http.jsonBody

        request =
            Http.post url body groupDecoder
    in
        Http.send ReqSaveGroup request


saveConnection : TileConnection -> Cmd Msg
saveConnection g =
    let
        url =
            apiBase ++ "connections"

        body =
            connectionEncoder g |> Http.jsonBody

        request =
            Http.post url body connectionDecoder
    in
        Http.send ReqSaveConnection request


deleteItem : String -> String -> Cmd Msg
deleteItem itemType id =
    let
        url =
            apiBase ++ itemType ++ "/" ++ id

        req =
            Http.request
                { method = "DELETE"
                , body = Http.emptyBody
                , url = url
                , expect = Http.expectJson messageDecoder
                , headers = []
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (\res -> ReqDelete itemType res) req


tupleToSelectOptions : List ( String, String ) -> String -> List (Html Msg)
tupleToSelectOptions values selectedValue =
    values
        |> List.map (\z -> option [ value <| Tuple.first z, selected (selectedValue == (Tuple.first z)) ] [ text <| Tuple.second z ])
