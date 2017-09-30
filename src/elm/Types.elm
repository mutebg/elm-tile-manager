module Types exposing (..)

import Http
import Navigation


targetDesktop : number
targetDesktop =
    1


targetMobile : number
targetMobile =
    2


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


type alias MessageResponse =
    { code : Int
    , message : String
    }


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


type TileType
    = Link
    | News


type TileGroupsPosition
    = Main
    | Context


type FormMessage
    = TextInput String String
    | Checkbox String Bool
    | SelectPosition TileGroupsPosition
    | SelectTileType TileType


showTarget : Int -> String
showTarget target =
    if target == targetDesktop then
        "Desktop"
    else if target == targetMobile then
        "Mobile"
    else
        "None"
