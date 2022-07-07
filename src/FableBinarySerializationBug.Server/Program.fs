module p

open System.IO
open TypeShape

module Write =
    module Format =
        [<Literal>]
        let Nil = 0xc0uy
        [<Literal>]
        let False = 0xc2uy
        [<Literal>]
        let True = 0xc3uy

        let inline fixposnum value = byte value
        let inline fixnegnum value = byte value ||| 0b11100000uy
        [<Literal>]
        let Uint8 = 0xccuy
        [<Literal>]
        let Uint16 = 0xcduy
        [<Literal>]
        let Uint32 = 0xceuy
        [<Literal>]
        let Uint64 = 0xcfuy

        [<Literal>]
        let Int8 = 0xd0uy
        [<Literal>]
        let Int16 = 0xd1uy
        [<Literal>]
        let Int32 = 0xd2uy
        [<Literal>]
        let Int64 = 0xd3uy

        let inline fixstr len = 160uy + byte len
        [<Literal>]
        let Str8 = 0xd9uy
        [<Literal>]
        let Str16 = 0xdauy
        [<Literal>]
        let Str32 = 0xdbuy

        let inline fixarr len = 144uy + byte len
        [<Literal>]
        let Array16 = 0xdcuy
        [<Literal>]
        let Array32 = 0xdduy

    open System
    open System.Text
    open FSharp.NativeInterop
    open TypeShape
    open TypeShape_Utils

    let inline writeNil (out: Stream) = out.WriteByte Format.Nil

    let writeString (str: string) (out: Stream) =
        let maxLength = Encoding.UTF8.GetMaxByteCount str.Length

        let buffer = Span<byte> (NativePtr.stackalloc<byte> maxLength |> NativePtr.toVoidPtr, maxLength)
        let bytesWritten = Encoding.UTF8.GetBytes (String.op_Implicit str, buffer)

        printfn "Byte - %d, in total %d" buffer.[0] bytesWritten
        out.Write (Span.op_Implicit (buffer.Slice (0, bytesWritten)))

    let inline writeUnion union (out: Stream) (caseSerializers: Action<'a, Stream>[][]) tagReader =
        let tag = tagReader union
        let fieldSerializers = caseSerializers.[tag]

        if fieldSerializers.Length = 0 then
            out.WriteByte (Format.fixarr 1uy)
            out.WriteByte (Format.fixposnum tag)
        else
            out.WriteByte (Format.fixarr 2uy)
            out.WriteByte (Format.fixposnum tag)

            // write the field directly instead of using an array if the union case has a single field
            // saves 1 byte
            if fieldSerializers.Length = 1 then
                let serializer = fieldSerializers.[0]
                serializer.Invoke (union, out)
            else
                for serializer in fieldSerializers do
                    serializer.Invoke (union, out)

    let rec makeSerializer<'T> (): Action<'T, Stream> =
        if typeof<'T> = typeof<obj> then
            failwithf "Cannot serialize System.Object. If you are unable specify the generic parameter for 'makeSerializer', use 'makeSerializerObj' instead."

        let ctx = new TypeGenerationContext ()
        serializerCached<'T> ctx

    and private serializerCached<'T> (ctx: TypeGenerationContext): Action<'T, Stream> =
        let delay (c: Cell<Action<'T, Stream>>): Action<'T, Stream> =
            Action<'T, Stream>(fun x out -> c.Value.Invoke (x, out))

        match ctx.InitOrGetCachedValue<Action<'T, Stream>> delay with
        | Cached (value, _) -> value
        | NotCached x ->
            let serializer = makeSerializerAux<'T> ctx
            ctx.Commit x serializer

    and private makeSerializerAux<'T> (ctx: TypeGenerationContext): Action<'T, Stream> =
        let w (p: Action<'a, Stream>) = unbox<Action<'T, Stream>> p

        let makeMemberVisitor (m: IShapeReadOnlyMember<'T>) =
            m.Accept {
                new IReadOnlyMemberVisitor<'T, Action<'T, Stream>> with
                    member _.Visit (field: ReadOnlyMember<'T, 'a>) =
                        let s = serializerCached<'a> ctx
                        Action<_, _> (fun (x: 'T) out -> s.Invoke (field.Get x, out)) |> w
            }

        match shapeof<'T> with
        | Shape.Unit -> Action<_, _> (fun () out -> writeNil out) |> w
        | Shape.String -> Action<_, _> writeString |> w
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
            let caseSerializers = shape.UnionCases |> Array.map (fun c -> Array.map makeMemberVisitor c.Fields)
            Action<_, _> (fun (union: 'T) out -> writeUnion union out caseSerializers shape.GetTag) |> w
        | _ ->
            failwithf "Cannot serialize %s." typeof<'T>.Name

[<Struct>]
type MyResult<'T,'TError> = 
  | Ok of a: 'T
  | Error of b: 'TError

let msgPackSerialize =
    let s = Write.makeSerializer<MyResult<unit, string>> ()
    fun o stream -> s.Invoke (o, stream)

for _ in 1 .. 10 do
    let s = new MemoryStream ()
    msgPackSerialize (MyResult<unit, string>.Error "5") s
    printfn "%A\n" (s.ToArray ())
