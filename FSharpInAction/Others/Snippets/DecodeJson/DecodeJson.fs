namespace Others.Snippets 
module DecodeJson = 
    open Thoth.Json.Net

    module DecodeMapResponse = 
        // In this example, the response is JSON object and each of its attribute's value is either simple value or an JSON object.
        // Here, we want to extract the cooridate from the JSON.
        // 1) Define hierachcal object from the deepest level.
        // 2) Define on each level, how to extract (decode) the attributes.
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
            | Result.Error err -> 
                failwith $"{err}"


    module DecodeWorkflowsResponseV1 = 
        // The problem is that some attribute's value is another serilized JSON object. For example, executionPointers and data
        // Start from simple, just treat values as string first. 
        // Then, modify the our module to use more complex type and change cooresponding decoder.
        
        type Workflow = {
            id: string 
            definitionName: string
            executionPointers: string 
            description: string option 
            data: string 
        }

        let workflowsDecoder: Decoder<Workflow> = 
            Decode.object (
                fun get -> 
                    {
                        Workflow.id = get.Required.Field "id" (Decode.string)
                        Workflow.definitionName = get.Required.Field "definitionName" (Decode.string)
                        Workflow.executionPointers = get.Required.Field "executionPointers" (Decode.string)
                        Workflow.description = get.Optional.Field "description" (Decode.string)
                        Workflow.data = get.Required.Field "data" (Decode.string)
                    }
            )


        let demo () = 
            match ResponseData.workflowsStr |> Decode.fromString (Decode.list workflowsDecoder) with 
            | Result.Ok x -> 
                x
            | Result.Error err -> 
                failwith $"{err}"


    module DecodeWorkflowsResponseV2 = 
        // The problem is that some attribute's value is another serilized JSON object. 
        // For example, executionPointers and data. Those attribute's value is an list of JSON object be serilized as string.
        // Start from simple, just treat values as string first. 
        // Then, modify the our module to use more complex type and change cooresponding decoder.
    
        type ExecutionPointer = {
            Id: string 
            StepId: int 
            StepName: string 
            StartTime: System.DateTime
            EndTime: System.DateTime option
            Status: string 
        }

        type Workflow = {
            id: string 
            definitionName: string
            executionPointers: ExecutionPointer list
            description: string option 
            data: string 
        }


        let executionPointerDecoder: Decoder<ExecutionPointer> = 
            Decode.object (
                fun get -> 
                    {
                        ExecutionPointer.Id = get.Required.Field "Id" (Decode.string)
                        ExecutionPointer.StepId = get.Required.Field "StepId" (Decode.int)
                        ExecutionPointer.StepName = get.Required.Field "StepName" (Decode.string)
                        ExecutionPointer.StartTime = get.Required.Field "StartTime" (Decode.datetimeUtc)
                        ExecutionPointer.EndTime = get.Optional.Field "EndTime" (Decode.datetimeUtc)
                        ExecutionPointer.Status = get.Required.Field "Status" (Decode.string)
                    }
            )


        let executionPointersDecoder: Decoder<ExecutionPointer list> = 
            Decode.string 
            |> Decode.andThen (fun str ->
                match str |> Decode.fromString (Decode.list executionPointerDecoder) with 
                | Ok pointers -> Decode.succeed pointers 
                | Error err -> Decode.fail $"{err}" 
            )

        let workflowsDecoder: Decoder<Workflow> = 
            Decode.object (
                fun get -> 
                    {
                        Workflow.id = get.Required.Field "id" (Decode.string)
                        Workflow.definitionName = get.Required.Field "definitionName" (Decode.string)
                        Workflow.executionPointers = get.Required.Field "executionPointers" executionPointersDecoder
                        Workflow.description = get.Optional.Field "description" (Decode.string)
                        Workflow.data = get.Required.Field "data" (Decode.string)
                    }
            )


        let demo () = 
            match ResponseData.workflowsStr |> Decode.fromString (Decode.list workflowsDecoder) with 
            | Result.Ok x -> 
                x
            | Result.Error err -> 
                failwith $"{err}"            


    // Different from V2, we want to decode data from string to be complex DU object
    module DecodeWorkflowsResponseV3 = 
        // The problem is that some attribute's value is another serilized JSON object. 
        // For example, executionPointers and data. Those attribute's value is an list of JSON object be serilized as string.
        // Start from simple, just treat values as string first. 
        // Then, modify the our module to use more complex type and change cooresponding decoder.
    
        type ExecutionPointer = {
            Id: string 
            StepId: int 
            StepName: string 
            StartTime: System.DateTime
            EndTime: System.DateTime option
            Status: string 
        }

        type AksData = {
            DeploymentName: string 
            DeploymentLocation: string 
            K8sConfig: string 
            SubscriptionId: string 
        }

        type NspData = {
            DeploymentName: string
            NspResourceId: string 
            StorageResourceId: string 
            StorageKey: string 
            StorageDstResourceId: string 
            StorageDstKey: string 
            SrcAccountName: string 
            StorageDstName: string 
            SubscriptionId: string 
        }

        type WorkflowData = 
            | Aks of AksData
            | Nsp of NspData


        type Workflow = {
            id: string 
            definitionName: string
            executionPointers: ExecutionPointer list
            description: string option 
            data: WorkflowData 
        }


        let executionPointerDecoder: Decoder<ExecutionPointer> = 
            Decode.object (
                fun get -> 
                    {
                        ExecutionPointer.Id = get.Required.Field "Id" (Decode.string)
                        ExecutionPointer.StepId = get.Required.Field "StepId" (Decode.int)
                        ExecutionPointer.StepName = get.Required.Field "StepName" (Decode.string)
                        ExecutionPointer.StartTime = get.Required.Field "StartTime" (Decode.datetimeUtc)
                        ExecutionPointer.EndTime = get.Optional.Field "EndTime" (Decode.datetimeUtc)
                        ExecutionPointer.Status = get.Required.Field "Status" (Decode.string)
                    }
            )


        let executionPointersDecoder: Decoder<ExecutionPointer list> = 
            Decode.string 
            |> Decode.andThen (fun str ->
                match str |> Decode.fromString (Decode.list executionPointerDecoder) with 
                | Ok pointers -> Decode.succeed pointers 
                | Error err -> Decode.fail $"{err}" 
            )





        let workflowsDecoder: Decoder<Workflow> = 
            Decode.object (
                fun get -> 
                    {
                        Workflow.id = get.Required.Field "id" (Decode.string)
                        Workflow.definitionName = get.Required.Field "definitionName" (Decode.string)
                        Workflow.executionPointers = get.Required.Field "executionPointers" executionPointersDecoder
                        Workflow.description = get.Optional.Field "description" (Decode.string)
                        Workflow.data = get.Required.Field "data" (Decode.string)
                    }
            )


        let demo () = 
            match ResponseData.workflowsStr |> Decode.fromString (Decode.list workflowsDecoder) with 
            | Result.Ok x -> 
                x
            | Result.Error err -> 
                failwith $"{err}"         

    // We want to decode the value based on condition specified by the type
    module ConditionParsing = 
        
        open System
        // From: https://stackoverflow.com/questions/70025092/thoth-json-net-conditional-parsing
        type DecodeBuilder() =
            member _.Bind(decoder, f) : Decoder<_> =
                Decode.andThen f decoder
            member _.Return(value) =
                Decode.succeed value
            member _.ReturnFrom(decoder : Decoder<_>) =
                decoder
        
        let decode = DecodeBuilder()


        let decodeReverse =
            decode {
                let! str = Decode.string
                return str
                    |> Seq.rev
                    |> Seq.toArray
                    |> String
            }
        
        let decodeRot13 =
        
            let rot13 c =
                if 'a' <= c && c <= 'm' || 'A' <= c && c <= 'M' then
                    char (int c + 13)
                elif 'n' <= c && c <= 'z' || 'N' <= c && c <= 'Z' then
                    char (int c - 13)
                else c
                
            decode {
                let! str = Decode.string
                return str
                    |> Seq.map rot13
                    |> Seq.toArray
                    |> String
            }
        
        let customDecoders =
            Map [
                "Reverse", decodeReverse
                "Rot13", decodeRot13
            ]

        let decodeByType =
            decode {
                let! typ = Decode.field "type" Decode.string
                return! Decode.field "value" customDecoders.[typ]
            }

        let demo () = 
            Decode.fromString (Decode.array decodeByType) ResponseData.conditionResponseStr
    

