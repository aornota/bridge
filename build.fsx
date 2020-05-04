#r "paket: groupref build //"
#if !FAKE
// See https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095.
#r "netstandard"
#r "Facades/netstandard"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open System

open Fake.Core
open Fake.DotNet
open Fake.IO

let private mdConsoleDir = Path.getFullName "./src/md-console"

let private runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let private createMissingAppSettingsForDevelopment dir =
    let requiredSettings, productionSettings = Path.combine dir "appsettings.development.json", "appsettings.production.json"
    if not (File.exists requiredSettings) then
        Shell.copyFile requiredSettings (Path.combine dir productionSettings)
        Trace.traceImportant (sprintf "WARNING -> %s did not exist and has been copied from %s; it will most likely need to be modified" requiredSettings productionSettings)

Target.create "run-md-console" (fun _ ->
    createMissingAppSettingsForDevelopment mdConsoleDir
    runDotNet "run" mdConsoleDir)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'fake build -t {target}':"
    printfn "\n\trun-md-console -> builds and runs [Debug] md-console"
    printfn "\n\thelp -> shows this list of build targets\n")

Target.runOrDefaultWithArguments "run-md-console"
