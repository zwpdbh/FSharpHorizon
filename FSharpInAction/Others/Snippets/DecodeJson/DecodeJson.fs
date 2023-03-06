namespace Others.Snippets 
module DecodeJson = 
    open Thoth.Json.Net

    module DecodeMapResponse = 
        type Cooridates = {
            coordinates: float list
        }
    
    
        let coordinateDecoder: Decoder<Cooridates> = 
            Decode.object (
                fun get -> 
                    {
                        Cooridates.coordinates = get.Required.Field "coordinates" (Decode.list Decode.float)
                    }
            )

        type MapPlace = {
            name: string 
            coordinates: Cooridates
        }

        let mapPlaceDecoder: Decoder<MapPlace> = 
            Decode.object (
                fun get -> 
                    {
                        MapPlace.name =  get.Required.Field "name" Decode.string
                        MapPlace.coordinates = get.Required.Field "point" coordinateDecoder
                    }
            )

        let demo () = 
            //m
            0
