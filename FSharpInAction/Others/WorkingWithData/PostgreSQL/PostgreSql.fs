namespace Others.WorkingWithData 

/// Testing https://github.com/Zaid-Ajaj/Npgsql.FSharp
/// Thin F#-friendly layer for the Npgsql data provider for PostgreSQL.
module PostgreSql =
    
    module NpgsqlFSharp = 
        open Npgsql.FSharp

        let connectionString = 
            //// (1) from environment variables
            //let connectionString = System.Environment.GetEnvironmentVariable "DATABASE_CONNECTION_STRING"

            //// (2) hardcoded
            //let connectionString = "Host=localhost; Database=fsharp; Username=postgres; Password=postgres;"

            //// the library also accepts URI postgres connection format (NOTE: not all query string parameters are converted)
            //let connectionString = "postgres://username:password@localhost/dvdrental";

            // (3) using the connection string builder API
            let connectionString : string =
                Sql.host "localhost"
                |> Sql.database "fsharp"
                |> Sql.username "postgres"
                |> Sql.password "postgres"
                |> Sql.port 5432
                |> Sql.formatConnectionString

            connectionString

        let demoInsertOneRow () =
            connectionString
            |> Sql.connect
            |> Sql.query "insert into Student(name, age) values (@Name, @Age)"
            |> Sql.parameters
                [
                    "@Name", Sql.string "zw"; "@age", Sql.int 19
                ]
            |> Sql.executeNonQuery
            |> printfn "%A"

        let demoInfinitSeq () =
            let rnd = new System.Random()
            let rec infiniteRands () = 
                seq {
                    yield rnd.Next(20, 30)
                    yield! infiniteRands()
                }
            infiniteRands () |> Seq.take 100 |> printfn "%A"

        let dummyStudents = 
            seq {
                for i in 1..30 do 
                    yield ["@Name", Sql.string $"student{i}"; "@age", (Sql.int (i + 10))]
            } |> List.ofSeq


        let insertStudents () = 
            let students = 
                [
                    ("Abercrombie, Kim", Some 10)
                    ("Abolrous, Hazen", Some 14)
                    ("Hance, Jim", Some 12)
                    ("Adams, Terry", Some 12)
                    ("Hansen, Claus", Some 11)
                    ("Penor, Lori", Some 13)
                    ("Perham, Tom", Some 12)
                    ("Peng, Yun-Feng", None)
                ]
                |> List.map (fun (name, age) -> 
                    ["@Name", Sql.string name; "@age", Sql.intOrNone age]
                )

            // See example: https://zaid-ajaj.github.io/Npgsql.FSharp/#/usecases/batch-updates-or-inserts
            connectionString
            |> Sql.connect
            |> Sql.executeTransaction [
                "insert into Student(name, age) values (@Name, @Age)", students        
            ]
            |> printfn "%A"


        let insertCourses () = 
            let courses = 
                ["Algebra I"; "Trigonometry"; "Algebra II"; "History"; "English"; "French"; "Chinese"]
                |> List.map (fun name -> 
                    ["@Name", Sql.string name]
                )


            connectionString
            |> Sql.connect
            |> Sql.executeTransaction [
                "insert into Course(name) values (@Name)", courses        
            ]
            |> printfn "%A"


        let insertCourseSelections () = 
            let courseSelections = 
                [
                    (1, 2)
                    (1, 3)
                    (1, 5)
                    (2, 2)
                    (2, 5)
                    (2, 6)
                    (2, 3)
                    (3, 2)
                    (3, 1)
                    (4, 2)
                    (4, 5)
                    //(4, 2), somehow this cause pk dulicated problem...
                    (5, 3)
                    (5, 2)
                    (7, 3)
                ]
                |> List.map (fun (studentId, courseId) ->
                    [
                        @"StudentId", Sql.int studentId 
                        @"CourseId", Sql.int courseId]
                )

            connectionString
            |> Sql.connect
            |> Sql.executeTransaction [
                "insert into CourseSelection(StudentId, CourseId) values (@StudentId, @CourseId)", courseSelections
            ]
            |> printfn "%A"


        let initDB () =
            // Need to create those tables from SQL script.
            insertStudents ()
            insertCourses ()
            insertCourseSelections ()


    