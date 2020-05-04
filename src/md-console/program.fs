module Aornota.Bridge.MdConsole.Program

open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.MdConsole.Console

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

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
    write "..." ConsoleColor.Green

    try
        (* TEMP-NMB... *)
        writeNewLine "\nLogging examples:\n" ConsoleColor.Magenta
        let test = Some 3.14
        sourcedLogger.Debug "This is a debug message"
        sourcedLogger.Information ("This is an information message: {test}", test)
        sourcedLogger.Warning "This is a warning message"
        failwith "Fake error. Sad!"
    with | exn -> sourcedLogger.Error ("Unexpected error: {errorMessage}\n{stackTrace}", exn.Message, exn.StackTrace)

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
