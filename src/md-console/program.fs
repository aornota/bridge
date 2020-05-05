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
        .AddJsonFile("appsettings.development.json", true)
#else
        .AddJsonFile("appsettings.production.json", true)
#endif
        .Build()

do Log.Logger <-
    LoggerConfiguration()
        .ReadFrom.Configuration(configuration)
        .Destructure.FSharpTypes()
        .CreateLogger()

let private logger = Log.Logger
let private sourcedLogger = logger |> sourcedLogger SOURCE

let private mainAsync argv = async {
    writeNewLine (sprintf "Running %s.mainAsync" SOURCE) ConsoleColor.Green
    write (sprintf " %A" argv) ConsoleColor.DarkGreen
    write "...\n\n" ConsoleColor.Green

    try processMd logger (Path.Combine ((DirectoryInfo Environment.CurrentDirectory).Parent.FullName, "md"))
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
