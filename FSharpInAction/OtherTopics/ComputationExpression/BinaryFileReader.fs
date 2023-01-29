namespace ComputationExpression



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
    let IOExceptionHandlingWrapper(f: BinaryReader -> ParseResult<'a>) = 
        fun i -> 
            try
                f(i)
            with 
                (e & :? IOException) -> Failure(i.BaseStream.Position, e.Message)

    let RByte = 
        BinParser(IOExceptionHandlingWrapper(
            fun (i: BinaryReader) -> Success(i.ReadByte(), i)
        ))

    let RShort = 
        BinParser(IOExceptionHandlingWrapper (fun (i: BinaryReader) -> Success(i.ReadInt16(), i)))

    let RInt = 
        BinParser(IOExceptionHandlingWrapper (fun (i: BinaryReader) -> Success(i.ReadInt32(), i)))

    let AByte(b: byte) = 
        BinParser(IOExceptionHandlingWrapper(
            fun (i: BinaryReader) -> 
                let rB = i.ReadByte()
                if (rB = b) then 
                    Success(byte(rB), i)
                else 
                    Failure(i.BaseStream.Position, sprintf($"Expecting {b}, got {rB}"))
        ))