namespace Others.WorkingWithData 



module DomainModels = 
    // From https://www.compositional-it.com/news-blog/sql-server-and-f-series/
    // It will be used across diffent modules
    open System

    type PokemonTrainerId = PokemonTrainerId of Guid
    type PokeIndex = PokeIndex of int

    type PokemonType =
    | Rock
    | Grass
    | Poison
    | Fire
    | Psychic
    | Ghost
    | Ice

    type Pokemon =
        { PokeIndex : PokeIndex
          Name : string
          Level: int
          PokemonTypes: PokemonType list
          EvolutionName: string option }

    type Record = 
        { Wins: uint; Losses: uint }

    type PokemonTrainer =
        { Id : PokemonTrainerId
          Name : string
          Pokemon : Pokemon list
          Record: Record }


    // TBD:: https://www.compositional-it.com/news-blog/from-domain-types-to-sql-tables/
