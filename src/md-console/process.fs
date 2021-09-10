module Aornota.Bridge.MdConsole.Process

open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.Domain.Auction
open Aornota.Bridge.Domain.Core
open Aornota.Bridge.Domain.Evaluation.Core
open Aornota.Bridge.Domain.Formatting.Core
open Aornota.Bridge.MdConsole.Extensions.Auction
open Aornota.Bridge.MdConsole.Extensions.Core

open FsToolkit.ErrorHandling
open Serilog
open System
open System.IO
open System.Text.RegularExpressions

let [<Literal>] private SOURCE = "MdConsole.Process"

let [<Literal>] private SINGLE_LINE_COMMENT = "//"
let [<Literal>] private MULTI_LINE_COMMENT__STARTS = "(*"
let [<Literal>] private MULTI_LINE_COMMENT__ENDS = "*)"

let [<Literal>] private TOC = "toc"

let [<Literal>] private UNPROCESSED_TAG_WARNING = "**_WARNING_ -> Unprocessed tag:**"

let private fileTag = Regex("{file:(.+)}") // e.g. {file:1000 Introduction\Introduction.md}
let private processFileTag (processFile:FileInfo -> string) (fileInfo:FileInfo) (match':Match) = processFile (FileInfo(Path.Combine(fileInfo.Directory.FullName, match'.Groups.[1].Value)))

let private cardTag = Regex("{([AKQJT98765432][cdhs])}") // e.g. {As} | {7h} | {3d} | {Jc}
let private processCardTag (fileInfo:FileInfo) (match':Match) =
    match Card.OfString(match'.Groups.[1].Value) with
    | Ok card -> card.MdString
    | Error error -> failwith $"{fileInfo.FullName} -> {nameof Card} tag {match'.Value} is invalid: {error}"

let private bidTag = Regex("{([1234567]([CDHS]|NT)|-|pass|dbl|rdbl)}") // e.g. {1H} | {3NT} | {-} | {pass} | {dbl} | {rdbl}
let private processBidTag (fileInfo:FileInfo) (match':Match) =
    match Bid.OfString(match'.Groups.[1].Value) with
    | Ok bid -> bid.MdString
    | Error error -> failwith $"{fileInfo.FullName} -> {nameof Bid} tag {match'.Value} is invalid: {error}"

let private handTag = Regex("{\|(.+)\|}") // e.g. {| s:AK63 h:T42 d:- c:AQJT62 --shape --hcp |} | {| s:3 h:T d:- c:JT6 --partial |}
let private processHandTag (fileInfo:FileInfo) (match':Match) =
    let suitWithRanks (suit:Suit) (splits:string list) =
        let suitChar = match suit with | Spade -> 's' | Heart -> 'h' | Diamond -> 'd' | Club -> 'c'
        match splits |> List.filter (fun split -> split.StartsWith($"{suitChar}:")) with
        | [] -> Error $"No split for {suit}"
        | [ split ] ->
            match split.Length with
            | n when n > 2 ->
                let ranks = split.Substring 2
                if ranks = "-" then Ok (suit, [])
                else
                    let ranks = ranks |> List.ofSeq |> List.map Rank.OfChar
                    match ranks |> List.choose (fun rank -> match rank with | Ok _ -> None | Error error -> Some error) with
                    | error :: _ -> Error error
                    | _ ->
                        let ranks = ranks |> List.choose (fun rank -> match rank with | Ok rank -> Some rank | Error _ -> None) |> List.sort
                        let uniqueRanks = ranks |> List.groupBy id
                        if uniqueRanks.Length <> ranks.Length then Error $"Repeated cards for {suit}"
                        else Ok (suit, ranks)
            | _ -> Error $"No cards for {suit}"
        | _ :: _ -> Error $"Multiple splits for {suit}"
    let suitWithRanksMd (suit:Suit, ranks:Rank list) = $"""{suit.MdString}{if ranks.Length > 0 then ranks |> List.map (fun rank -> rank.MdString) |> String.concat "" else "-"}"""
    let cardsMd spadeRanks heartRanks diamondRanks clubRanks = $"{suitWithRanksMd spadeRanks}{suitWithRanksMd heartRanks}{suitWithRanksMd diamondRanks}{suitWithRanksMd clubRanks}"
    let splits = match'.Groups.[1].Value.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let result = result {
        let! spadeRanks = suitWithRanks Spade splits
        let! heartRanks = suitWithRanks Heart splits
        let! diamondRanks = suitWithRanks Diamond splits
        let! clubRanks = suitWithRanks Club splits
        let cardCount = (snd spadeRanks).Length + (snd heartRanks).Length + (snd diamondRanks).Length + (snd clubRanks).Length
        let isPartial = splits |> List.exists (fun split -> split = "--partial")
        let showShape = splits |> List.exists (fun split -> split = "--shape")
        let showHcp = splits |> List.exists (fun split -> split = "--hcp")
        return!
            match cardCount, isPartial with
            | n, _ when n > 13 -> Error $"{nameof Hand} contains more than 13 cards"
            | 13, true -> Error $"--partial flag should only be used when {nameof Hand} contains fewer than 13 cards"
            | 13, false ->
                let cardsMd = cardsMd spadeRanks heartRanks diamondRanks clubRanks
                let shapeMd, hcpMd =
                    if showShape || showHcp then
                        let spades = snd spadeRanks |> List.map (fun rank -> Card.Make(rank, Spade))
                        let hearts = snd heartRanks |> List.map (fun rank -> Card.Make(rank, Heart))
                        let diamonds = snd diamondRanks |> List.map (fun rank -> Card.Make(rank, Diamond))
                        let clubs = snd clubRanks |> List.map (fun rank -> Card.Make(rank, Club))
                        let hand = Hand.Make(spades @ hearts @ diamonds @ clubs)
                        let shapeMd =
                            if showShape then
                                let suitCountsMd (spadeCount, heartCount, diamondCount, clubCount) =
                                    let suitCountMd count = if count > 4 then $"**{count}**" else if count = 4 then $"_**{count}**_" else if count <= 2 then $"_{count}_" else $"{count}"
                                    $"{suitCountMd spadeCount}={suitCountMd heartCount}={suitCountMd diamondCount}={suitCountMd clubCount}"
                                let suitCountsMd = suitCountsMd hand.SuitCounts
                                Some $"{hand.ShapeCategory.TextLower} ({suitCountsMd})"
                            else None
                        let hcpMd = if showHcp then Some $"{hand.Hcp} HCP" else None
                        shapeMd, hcpMd
                    else None, None
                let additionalInfoMd =
                    match shapeMd, hcpMd with
                    | Some shapeMd, Some hcpMd -> $" -- {shapeMd} | {hcpMd}"
                    | Some shapeMd, None -> $" -- {shapeMd}"
                    | None, Some hcpMd -> $" -- {hcpMd}"
                    | None, None -> ""
                Ok $"{cardsMd}{additionalInfoMd}"
            | _, true ->
                if showShape then Error $"--shape flag should not be used when {nameof Hand} contains fewer than 13 cards"
                else if showHcp then Error $"--hcp flag should not be used when {nameof Hand} contains fewer than 13 cards"
                else Ok (cardsMd spadeRanks heartRanks diamondRanks clubRanks)
            | _, false -> Error $"--partial flag must be used when {nameof Hand} contains fewer than 13 cards" }
    match result with
    | Ok text -> text
    | Error error -> failwith $"{fileInfo.FullName} -> {nameof Hand} tag {match'.Value} is invalid: {error}"

let private anyTag = Regex("{(.*)}")
let private processUnprocessedTag (logger:ILogger) (fileInfo:FileInfo) (match':Match) =
    let tag = match'.Groups.[1].Value
    if tag = TOC || tag.StartsWith(UNPROCESSED_TAG_WARNING) then match'.Value
    else
        logger.Warning("{file} -> Unprocessed tag: {tag}", fileInfo.FullName, match'.Value)
        $"{UNPROCESSED_TAG_WARNING}{tag}"

let rec private processFile (logger:ILogger) (fileInfo:FileInfo) =
    let partialPath = @$"..\{fileInfo.Directory.Name}\{fileInfo.Name}"
    logger.Debug ("Processing {partialPath}...", partialPath)
    let lines =
        File.ReadAllLines fileInfo.FullName
        |> List.ofArray
        |> List.filter (fun line -> not ((line.Trim()).StartsWith(SINGLE_LINE_COMMENT)))
        // Note: When removing end-of-line comments, look for " //" rather than just "//" - else will inadvertently mangle "https://...".
        |> List.map (fun line -> match line.IndexOf($" {SINGLE_LINE_COMMENT}") with | index when index > 0 -> line.Substring(0, index) | _ -> line)
    let folder (lines:string list, inMultiLineComment:bool) (line:string) =
        if inMultiLineComment then lines, not ((line.Trim()).EndsWith(MULTI_LINE_COMMENT__ENDS))
        else if (line.Trim()).StartsWith(MULTI_LINE_COMMENT__STARTS) && (line.Trim()).EndsWith(MULTI_LINE_COMMENT__ENDS) then lines, false
        else
            let inMultiLineComment = (line.Trim()).StartsWith(MULTI_LINE_COMMENT__STARTS)
            (if not inMultiLineComment then line :: lines else lines), inMultiLineComment
    let lines, _ = lines |> List.fold folder ([], false)
    let contents = lines |> List.rev |> String.concat "\n"
    let contents = fileTag.Replace(contents, MatchEvaluator (processFileTag (processFile logger) fileInfo))
    let contents = cardTag.Replace(contents, MatchEvaluator (processCardTag fileInfo))
    let contents = bidTag.Replace(contents, MatchEvaluator (processBidTag fileInfo))
    let contents = handTag.Replace(contents, MatchEvaluator (processHandTag fileInfo))

    (* TODO-NMB:
         - Auction tags?...
         - Deal tags?... *)

    let contents = anyTag.Replace(contents, MatchEvaluator (processUnprocessedTag logger fileInfo))
    logger.Debug("...processed {partialPath}", partialPath)
    contents

let private tocTag = Regex($"{{{TOC}}}")
let private processTocTag (logger:ILogger) (contents:string) (match':Match) =
    let namedAnchor = Regex("<a name=\"(.+)\">")
    let linkAndText line =
        let match' = namedAnchor.Match(line)
        if match'.Success then
            let anchor = match'.Groups.[1].Value
            Some (anchor, anchor.Replace("_", " "))
        else None
    let (|H1|_|) (line:string) = if (line.Trim()).StartsWith("# ") then linkAndText line else None
    let (|H2|_|) (line:string) = if (line.Trim()).StartsWith("## ") then linkAndText line else None
    let (|H3|_|) (line:string) = if (line.Trim()).StartsWith("### ") then linkAndText line else None
    let (|H4|_|) (line:string) = if (line.Trim()).StartsWith("#### ") then linkAndText line else None
    logger.Information("Generating table-of-contents...")
    let toc =
        contents.Split '\n'
        |> List.ofArray
        |> List.choose (fun line ->
            match line with
            | H1 (link, text) -> Some $"* [**{text}**](#{link})"
            | H2 (link, text) -> Some $"  * [_**{text}**_](#{link})"
            | H3 (link, text) -> Some $"    * [{text}](#{link})"
            | H4 (link, text) -> Some $"      * [_{text}_](#{link})"
            | _ -> None)
        |> String.concat "\n"
    logger.Information("...table-of-contents generated")
    toc

let processMd logger =
    let rec findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent
    let logger = logger |> sourcedLogger SOURCE
    let srcDir = findSrcDir (DirectoryInfo(Environment.CurrentDirectory))
    let rootFileInfo = FileInfo(Path.Combine(srcDir, @"md\- root -.md"))
    logger.Information("Starting processing...")
    let contents = processFile logger rootFileInfo
    let contents =
        match tocTag.Matches contents |> List.ofSeq with
        | [] -> contents
        | [ _ ] -> tocTag.Replace(contents, MatchEvaluator(processTocTag logger contents))
        | match' :: _ ->
            logger.Warning("Multiple {tag} tags found", match'.Value)
            contents
    logger.Information("...finished processing")
    let readmeFile = Path.Combine(DirectoryInfo(srcDir).Parent.FullName, "README.md")
    logger.Information("Writing {readme}...", readmeFile)
    File.WriteAllText(readmeFile, contents)
    logger.Information("...{readme} written", readmeFile)
