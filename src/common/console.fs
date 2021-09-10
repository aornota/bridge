module Aornota.Bridge.Common.Console

open System

let private lockObj = obj()

let write (text:string) consoleColor =
    let action = (fun _ ->
        let current = Console.ForegroundColor
        Console.ForegroundColor <- consoleColor
        Console.Write text
        Console.ForegroundColor <- current)
    lock lockObj action

let writeNewLine text consoleColor = write $"\n{text}" consoleColor

let writeBlankLine() = writeNewLine String.Empty ConsoleColor.White
