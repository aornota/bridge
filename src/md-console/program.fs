module Aornota.Bridge.MdConsole.Program

open Aornota.Bridge.Common.Console
open Aornota.Bridge.Common.IfDebug
open Aornota.Bridge.Common.SourcedLogger

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "MdConsole.Program"

// TODO-NMB: Investigate why Release not writing to ...\logs?...

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

let private mainAsync () = async {
    try
        writeNewLine "Running " ConsoleColor.Yellow
        write (ifDebug "Debug" "Release") ConsoleColor.DarkYellow
        write $" {SOURCE}.mainAsync\n\n" ConsoleColor.Yellow

        Process.processMd Log.Logger

    with | exn -> sourcedLogger.Error("Unexpected error:\n\t{errorMessage}", exn.Message)

    writeNewLine "\nPress any key to exit..." ConsoleColor.Yellow
    Console.ReadKey () |> ignore
    writeBlankLine ()
    return 0 }

[<EntryPoint>]
let main _ =
    async {
        do! Async.SwitchToThreadPool ()
        return! mainAsync ()
    } |> Async.RunSynchronously
