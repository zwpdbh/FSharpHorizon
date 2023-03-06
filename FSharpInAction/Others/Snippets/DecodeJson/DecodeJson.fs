namespace Others.Snippets 
module DecodeJson = 
    open Thoth.Json.Net

    module DecodeMapResponse = 

        type Point = {
            cooridates: float list
        }

        type Place = {
            name: string
            point: Point
        }

        type ResourceSet = {
            resources: Place list
        }

        type MapPlace = {
            resourceSets: ResourceSet list  
        }

        let pointDecoder: Decoder<Point> = 
            Decode.object (
                fun get ->
                    {
                        Point.cooridates = get.Required.Field "coordinates" (Decode.list Decode.float)
                    }
            )

        let placeDecoder: Decoder<Place> = 
            Decode.object (
                fun get -> 
                    {
                        Place.name = get.Required.Field "name" Decode.string
                        Place.point = get.Required.Field "point" pointDecoder
                    }
            )

        let resourceSetDecoder: Decoder<ResourceSet> = 
            Decode.object (
                fun get -> 
                    {
                        ResourceSet.resources = get.Required.Field "resources" (Decode.list placeDecoder)
                    }
            )

        let mapPlaceDecoder: Decoder<MapPlace> = 
            Decode.object (
                fun get -> 
                    {
                        MapPlace.resourceSets =  get.Required.Field "resourceSets" (Decode.list resourceSetDecoder)
                    }
            )

        let demo () = 
            match ResponseData.mapResponseStr |> Decode.fromString mapPlaceDecoder with 
            | Result.Ok x -> 
                x.resourceSets.Head.resources.Head.point.cooridates
            | _ -> 
                failwith "error"
           
            
