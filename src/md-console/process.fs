module Aornota.Bridge.MdConsole.Process

open Aornota.Bridge.Common.Domain
open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.MdConsole.Domain

open Serilog
open System.IO
open System.Text.RegularExpressions

let [<Literal>] private SOURCE = "MdConsole.Process"

let matchContents (match':Match) = match'.Value.Substring (1, match'.Value.Length - 2) // i.e. match' should be "{contents}"

let private fileTag = Regex "{file:.+}"
let private processFileTag (fileInfo:FileInfo) (processFile:FileInfo -> string) (match':Match) =
    processFile (FileInfo (Path.Combine (fileInfo.Directory.FullName, (matchContents match').Substring 5)))

let private cardTag = Regex "{[AKQJT98765432][cdhs]}"
let private processCardTag (fileInfo:FileInfo) (match':Match) =
    let tag = matchContents match'
    match Card.ofString tag with
    | Ok card -> card.MdString
    | Error error -> failwithf "%s -> Card tag '%s' is invalid: %s" fileInfo.FullName tag error

let private bidTag = Regex "{[1234567]([CDHS]|NT)}|{-}|{pass}|{dbl}|{rdbl}"
let private processBidTag (fileInfo:FileInfo) (match':Match) =
    let tag = matchContents match'
    match Bid.ofString tag with
    | Ok bid -> bid.MdString
    | Error error -> failwithf "%s -> Bid tag '%s' is invalid: %s" fileInfo.FullName tag error

let rec private processFile (logger:ILogger) (fileInfo:FileInfo) =
    let partialPath = sprintf @"..\%s\%s" fileInfo.Directory.Name fileInfo.Name
    logger.Debug ("Processing {partialPath}...", partialPath)
    let contents = File.ReadAllText fileInfo.FullName
    let contents = fileTag.Replace (contents, MatchEvaluator (processFileTag fileInfo (processFile logger)))
    let contents = cardTag.Replace (contents, MatchEvaluator (processCardTag fileInfo))
    let contents = bidTag.Replace (contents, MatchEvaluator (processBidTag fileInfo))
    // TODO-NMB: More tags (e.g. hands | auctions | deals)...
    logger.Debug ("...processed {partialPath}", partialPath)
    contents

let processMd logger mdDir =
    let logger = logger |> sourcedLogger SOURCE
    let rootFileInfo = FileInfo (Path.Combine (mdDir, "root.md"))
    logger.Information "Starting processing..."
    let contents = processFile logger rootFileInfo
    (* TEMP-NMB...
    logger.Debug ("Processed contents:\n{contents}", contents) *)
    logger.Information "...finished processing"
    let readmeFile = Path.Combine (rootFileInfo.Directory.Parent.Parent.FullName, "README.md")
    logger.Information ("Writing {readme}...", readmeFile)
    File.WriteAllText (readmeFile, contents)
    logger.Information("...{readme} written", readmeFile)
