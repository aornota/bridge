module Aornota.Bridge.MdConsole.Process

open Aornota.Bridge.Common.Domain
open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.MdConsole.Domain

open Serilog
open System
open System.IO
open System.Text.RegularExpressions

let [<Literal>] private SOURCE = "MdConsole.Process"

let [<Literal>] private SINGLE_LINE_COMMENT = "//"
let [<Literal>] private MULTI_LINE_COMMENT__STARTS = "(*"
let [<Literal>] private MULTI_LINE_COMMENT__ENDS = "*)"

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

let private tocTag = Regex "{toc}"

let private anyTag = Regex "{.+}"

let rec private processFile (logger:ILogger) (fileInfo:FileInfo) =
    let partialPath = sprintf @"..\%s\%s" fileInfo.Directory.Name fileInfo.Name
    logger.Debug ("Processing {partialPath}...", partialPath)
    let lines =
        File.ReadAllLines fileInfo.FullName
        |> List.ofArray
        |> List.filter (fun line -> not ((line.Trim ()).StartsWith SINGLE_LINE_COMMENT))
        |> List.map (fun line -> match line.IndexOf SINGLE_LINE_COMMENT with | index when index > 0 -> line.Substring (0, index) | _ -> line)
    let folder (lines:string list, inMultiLineComment:bool) (line:string) =
        if inMultiLineComment then lines, not ((line.Trim ()).EndsWith MULTI_LINE_COMMENT__ENDS)
        else
            let inMultiLineComment = (line.Trim ()).StartsWith MULTI_LINE_COMMENT__STARTS
            (if not inMultiLineComment then line :: lines else lines), inMultiLineComment
    let lines, _ = lines |> List.fold folder ([], false)
    let contents = lines |> List.rev |> String.concat Environment.NewLine
    let contents = fileTag.Replace (contents, MatchEvaluator (processFileTag fileInfo (processFile logger)))
    let contents = cardTag.Replace (contents, MatchEvaluator (processCardTag fileInfo))
    let contents = bidTag.Replace (contents, MatchEvaluator (processBidTag fileInfo))

    // TODO-NMB: More tags (e.g. hands | auctions? | deals?)...

    anyTag.Matches contents
    |> List.ofSeq
    |> List.filter (fun match' -> not (tocTag.IsMatch match'.Value))
    |> List.iter (fun match' -> logger.Warning ("{file} -> Unprocessed tag {tag}", fileInfo.FullName, match'.Value))
    logger.Debug ("...processed {partialPath}", partialPath)
    contents

let processMd logger srcDir =
    let logger = logger |> sourcedLogger SOURCE
    let rootFileInfo = FileInfo (Path.Combine (srcDir, @"md\root.md"))
    logger.Information "Starting processing..."
    let contents = processFile logger rootFileInfo

    let contents =
        match tocTag.Matches contents |> List.ofSeq with
        | [] -> contents
        | [ match' ] ->
            // TODO-NMB: Generate table-of-contents...
            (* logger.Information "Generating table-of-contents..."
            let contents = ...
            logger.Information "...table-of-contents generated" *)
            contents
        | match' :: _ ->
            logger.Warning ("Multiple {tag} tags found", match'.Value)
            contents

    (* TEMP-NMB...
    logger.Debug ("Processed contents:\n{contents}", contents) *)
    logger.Information "...finished processing"
    let readmeFile = Path.Combine (rootFileInfo.Directory.Parent.Parent.FullName, "README.md")
    logger.Information ("Writing {readme}...", readmeFile)
    File.WriteAllText (readmeFile, contents)
    logger.Information("...{readme} written", readmeFile)
