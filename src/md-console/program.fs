module Aornota.Bridge.MdConsole.Program

open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.MdConsole.Console
open Aornota.Bridge.MdConsole.Process

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.IO

let [<Literal>] private SOURCE = "MdConsole.Program"

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
#if DEBUG
        .AddJsonFile("appsettings.development.json", false)
#else
        .AddJsonFile("appsettings.production.json", false)
#endif
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = Log.Logger |> sourcedLogger SOURCE

let private debugOrRelease =
#if DEBUG
    "Debug"
#else
    "Release"
#endif

let private mainAsync argv = async {
    writeNewLine "Running " ConsoleColor.Green
    write debugOrRelease ConsoleColor.DarkYellow
    write (sprintf " %s.mainAsync" SOURCE) ConsoleColor.Green
    write (sprintf " %A" argv) ConsoleColor.DarkGreen
    write "...\n\n" ConsoleColor.Green

    // TODO-NMB: Recursively look for .\src (rather than assuming that this is immediate parent of current)? e.g. in case run from ...\bin\[Debug|Release]\...
    try processMd Log.Logger (Path.Combine ((DirectoryInfo Environment.CurrentDirectory).Parent.FullName, "md"))
    with | exn -> sourcedLogger.Error ("Unexpected error:\n\t{errorMessage}", exn.Message)

    writeNewLine "Press any key to exit..." ConsoleColor.Green
    Console.ReadKey () |> ignore
    writeBlankLine ()
    return 0 }

[<EntryPoint>]
let main argv =
    async {
        do! Async.SwitchToThreadPool ()
        return! mainAsync argv
    } |> Async.RunSynchronously
