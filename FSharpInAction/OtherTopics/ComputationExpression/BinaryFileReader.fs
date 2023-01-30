namespace OtherTopics

open Expecto

module BinaryFileReader =
    // Using F# computation expressions to read binary files
    // See: http://langexplr.blogspot.com/2008/10/using-f-computation-expressions-to-read.html
    open System.IO

    type ParseResult<'a> =
        | Success of 'a * BinaryReader
        | Failure of int64 * string

    type BinParser<'a> =
        | BinParser of (BinaryReader -> ParseResult<'a>)

        member this.Function =
            match this with
            | BinParser pFunc -> pFunc


    // basic binary parsers for bytes, int16 and int32 values.
    let IOExceptionHandlingWrapper (f: BinaryReader -> ParseResult<'a>) =
        fun i ->
            try
                f (i)
            with
            | (e & :? IOException) -> Failure(i.BaseStream.Position, e.Message)

    // Read 1 byte
    let RByte =
        BinParser(IOExceptionHandlingWrapper(fun (i: BinaryReader) -> Success(i.ReadByte(), i)))

    // Read 2 byte
    let RShort =
        BinParser(IOExceptionHandlingWrapper(fun (i: BinaryReader) -> Success(i.ReadInt16(), i)))

    // Read 4 byte
    let RInt =
        BinParser(IOExceptionHandlingWrapper(fun (i: BinaryReader) -> Success(i.ReadInt32(), i)))

    // For reading an expected byte
    let AByte (b: byte) =
        BinParser(
            IOExceptionHandlingWrapper (fun (i: BinaryReader) ->
                let rB = i.ReadByte()

                if (rB = b) then
                    Success(byte (rB), i)
                else
                    Failure(i.BaseStream.Position, sprintf ($"Expecting {b}, got {rB}")))
        )

    // Let your read a fixed sequence of elements recognized by another parser
    let ParsingStep (func: 'a -> BinParser<'b>) (accumulatedResult: ParseResult<'b list>) currentSeqItem =
        match accumulatedResult with
        | Success (result, inp) ->
            match ((func currentSeqItem).Function inp) with
            | Success (result2, inp2) -> Success(result2 :: result, inp2)
            | Failure (offset, description) -> Failure(offset, description)
        | Failure (offset, description) -> Failure(offset, description)

    // A fixed sequence of elements is recognized. The number of elements is given by the seq instance.
    let FixedSequence (s: seq<'b>, parser: BinParser<'a>) =
        BinParser (fun i ->
            match (Seq.fold (ParsingStep(fun _ -> parser)) (Success([], i)) s) with
            | Success (result, input) -> Success(List.rev (result), input)
            | Failure (offset, description) -> Failure(offset, description))


    // Definition of builder
    // We're only defining the behavior for the let! and return elements.
    type BinParserBuilder() =
        member this.Bind(p: BinParser<'a>, rest: 'a -> BinParser<'b>) : BinParser<'b> =
            BinParser (fun i ->
                match p.Function(i) with
                | Success (r: 'a, i2) -> ((rest r).Function i2)
                | Failure (offset, description) -> Failure(offset, description))

        member this.Return(x) = BinParser(fun i -> Success(x, i))

    let pBuilder = BinParserBuilder()

    let bytesToString (bytes: byte list) =
        bytes
        |> Array.ofList
        |> System.Text.Encoding.UTF8.GetString

    let thePNGFileParser =
        //  0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        pBuilder {
            let! _ = AByte(byte (0x89))
            let! _ = AByte(byte (0x50))
            let! _ = AByte(byte (0x4E))
            let! _ = AByte(byte (0x47))
            let! _ = AByte(byte (0x0D))
            let! _ = AByte(byte (0x0A))
            let! _ = AByte(byte (0x1A))
            let! _ = AByte(byte (0x0A))

            let! length = RInt
            let! ihdr = 
                    FixedSequence(("IHDR" |> Array.ofSeq |> Seq.ofArray), RByte)
            let! width = RInt
            let! height = RInt
            let! depth = RByte
            let! colorType = RByte
            let! compressionMethod = RByte
            let! filterMethod = RByte
            let! interlaceMethod = RByte
            let! crc = RInt
            let! chunks = FixedSequence({1..crc}, RByte)

            return (length, ihdr, width, height, depth, colorType, compressionMethod, filterMethod, interlaceMethod, crc, chunks)
        }

    let demo01 () = 
            // Check http://www.fssnip.net/nv/title/Reading-binary-values-from-a-file to understand basic reading binary from file
            // Need to solve,  Error: Unable to read beyond the end of the stream.,in 226933
            //let pngFilePath = Common.filePathInProject @"ComputationExpression/PNG_01.png"
            let pngFilePath = @"D:\code\fsharp-programming\FSharpHorizen\FSharpInAction\OtherTopics\ComputationExpression\PNG_01.png"
            let pngFileStream = File.Open(pngFilePath, FileMode.Open, FileAccess.Read)
            pngFileStream.Seek(0, SeekOrigin.Begin) |> ignore 

            let pngReader = new BinaryReader(pngFileStream)
            let parsed = thePNGFileParser.Function pngReader

            match parsed with 
            | Success 
                ((length, ihdr, width, height, depth, colorType, compressionMethod, filterMethod, interlaceMethod, crc, chunks), _) when (bytesToString ihdr) = "IHDR" -> 
                printfn $"length = {length}, width = {width}, height = {height}"
                Result.Ok (width, height, ihdr)
            | Success _ -> 
                printfn "ihdr is not correct!"
                Result.Error "ihdr is not correct!"
            | Failure(x,y) -> 
                System.Console.WriteLine("Error: {0},in {1}",y,x)
                Result.Error $"Error: {y},in {x}"

    let test01 = 
        testCase "test 01"
        <| fun _ -> 
            match demo01() with 
            | Result.Ok (width, height, ihdr) -> 
                Expect.equal width 800 ""
                Expect.equal height 600 ""
                Expect.equal (bytesToString ihdr) "IHDR" ""
            | Result.Error err -> 
                Expect.equal err "" ""




    [<Tests>]
    let testList = testList "Test Binary Reader" [test01]

module BitSyntax = 
    // Using lib from https://github.com/theburningmonk/BitSyntax
    let demo () = 
        0


