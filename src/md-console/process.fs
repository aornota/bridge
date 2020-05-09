module Aornota.Bridge.MdConsole.Process

open Aornota.Bridge.Common.Domain
open Aornota.Bridge.Common.SourcedLogger
open Aornota.Bridge.MdConsole.Domain

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

let private fileTag = Regex "{file:(.+)}" // e.g. {file:1000 Introduction\Introduction.md}
let private processFileTag (processFile:FileInfo -> string) (fileInfo:FileInfo) (match':Match) =
    processFile (FileInfo (Path.Combine (fileInfo.Directory.FullName, match'.Groups.[1].Value)))

let private cardTag = Regex "{([AKQJT98765432][cdhs])}" // e.g. {As} | {7h} | {3d} | {Jc}
let private processCardTag (fileInfo:FileInfo) (match':Match) =
    let tag = match'.Groups.[1].Value
    match Card.ofString tag with
    | Ok card -> card.MdString
    | Error error -> failwithf "%s -> Card tag %s is invalid: %s" fileInfo.FullName match'.Value error

let private bidTag = Regex "{([1234567]([CDHS]|NT)|-|pass|dbl|rdbl)}" // e.g. {1H} | {3NT} | {-} | {pass} | {dbl} | {rdbl}
let private processBidTag (fileInfo:FileInfo) (match':Match) =
    let tag = match'.Groups.[1].Value
    match Bid.ofString tag with
    | Ok bid -> bid.MdString
    | Error error -> failwithf "%s -> Bid tag %s is invalid: %s" fileInfo.FullName match'.Value error

let private handTag = Regex "{\|(.+)\|}" // e.g. {| s:AK63 h:T42 d:- c:AQJT62 --shape --hcp |} | {| s:3 h:T d:- c:JT6 --partial |}
let private processHandTag (fileInfo:FileInfo) (match':Match) =
    let suitWithRanks (suit:Suit) (splits:string list) =
        let suitChar = match suit with | Spade -> 's' | Heart -> 'h' | Diamond -> 'd' | Club -> 'c'
        match splits |> List.filter (fun split -> split.StartsWith (sprintf "%c:" suitChar)) with
        | [] -> Error (sprintf "No split for %As" suit)
        | [ split ] ->
            match split.Length with
            | n when n > 2 ->
                let ranks = split.Substring 2
                if ranks = "-" then Ok (suit, [])
                else
                    let ranks = ranks |> List.ofSeq |> List.map Rank.ofChar
                    match ranks |> List.choose (fun rank -> match rank with | Ok _ -> None | Error error -> Some error) with
                    | error :: _ -> Error error
                    | _ ->
                        let ranks = ranks |> List.choose (fun rank -> match rank with | Ok rank -> Some rank | Error _ -> None) |> List.sort
                        // TODO-NMB: Check for repeated Ranks...
                        let uniqueRanks = ranks |> List.groupBy id
                        if uniqueRanks.Length <> ranks.Length then Error (sprintf "Repeated cards for %As" suit)
                        else Ok (suit, ranks)
            | _ -> Error (sprintf "No cards for %As" suit)
        | _ :: _ -> Error (sprintf "Multiple splits for %As" suit)
    let suitWithRanksMd (suit:Suit, ranks:Rank list) = sprintf "%s%s" suit.MdString (if ranks.Length > 0 then ranks |> List.map (fun rank -> rank.MdString) |> String.concat "" else "-")
    let cardsMd spadeRanks heartRanks diamondRanks clubRanks =
        sprintf "%s %s %s %s" (suitWithRanksMd spadeRanks) (suitWithRanksMd heartRanks) (suitWithRanksMd diamondRanks) (suitWithRanksMd clubRanks)
    let tag = match'.Groups.[1].Value
    let splits = tag.Split (' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
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
            | n, _ when n > 13 -> Error "Hand contains more than 13 cards"
            | 13, true -> Error "--partial flag should only be used when hand contains fewer than 13 cards"
            | 13, false ->
                let cardsMd = cardsMd spadeRanks heartRanks diamondRanks clubRanks
                let shapeMd, hcpMd =
                    if showShape || showHcp then
                        let spades = snd spadeRanks |> List.map (fun rank -> Card (rank, Spade))
                        let hearts = snd heartRanks |> List.map (fun rank -> Card (rank, Heart))
                        let diamonds = snd diamondRanks |> List.map (fun rank -> Card (rank, Diamond))
                        let clubs = snd clubRanks |> List.map (fun rank -> Card (rank, Club))
                        let hand = spades @ hearts @ diamonds @ clubs
                        let shapeMd =
                            if showShape then
                                let suitCountsMd (spadeCount, heartCount, diamondCount, clubCount) =
                                    let suitCounts = [ spadeCount ; heartCount ; diamondCount ; clubCount ]
                                    let min, max = suitCounts |> List.min, suitCounts |> List.max
                                    let suitCountMd count =
                                        if count = min then sprintf "_%i_" count
                                        else if count = max then sprintf "**%i**" count
                                        else if count >= 4 then sprintf "_**%i**_" count
                                        else sprintf "%i" count
                                    sprintf "%s-%s-%s-%s" (suitCountMd spadeCount) (suitCountMd heartCount) (suitCountMd diamondCount) (suitCountMd clubCount)
                                match hand with
                                | Balanced suitCounts -> sprintf "Balanced (%s)" (suitCountsMd suitCounts)
                                | SemiBalanced suitCounts -> sprintf "Semi-balanced (%s)" (suitCountsMd suitCounts)
                                | Unbalanced suitCounts -> sprintf "Unbalanced (%s)" (suitCountsMd suitCounts)
                                | _ -> failwithf "%s -> Hand tag %s is invalid: Unable to categorize as Balanced | Semi-Balanced | Unbalanced" fileInfo.FullName match'.Value
                            else ""
                        let hcpMd =
                            if showHcp then
                                match hcp hand with
                                | Ok hcp -> sprintf "%i HCP" hcp
                                | Error error -> failwithf "%s -> Hand tag %s is invalid: %s" fileInfo.FullName match'.Value error
                            else ""
                        shapeMd, hcpMd
                    else "", ""
                let additionalInfoMd =
                    match shapeMd, hcpMd with
                    | "", "" -> ""
                    | shapeMd, "" -> sprintf " -- %s" shapeMd
                    | "", hcpMd -> sprintf " -- %s" hcpMd
                    | shapeMd, hcpMd -> sprintf " -- %s | %s" shapeMd hcpMd
                Ok (sprintf "%s%s" cardsMd additionalInfoMd)
            | _, true ->
                if showShape then Error "--shape flag should not be used when hand contains fewer than 13 cards"
                else if showHcp then Error "--hcp flag should not be used when hand contains fewer than 13 cards"
                else Ok (cardsMd spadeRanks heartRanks diamondRanks clubRanks)
            | _, false -> Error "--partial flag must be used when hand contains fewer than 13 cards" }
    match result with
    | Ok text -> text
    | Error error -> failwithf "%s -> Hand tag %s is invalid: %s" fileInfo.FullName match'.Value error

let private anyTag = Regex "{(.*)}"
let private processUnprocessedTag (logger:ILogger) (fileInfo:FileInfo) (match':Match) =
    let tag = match'.Groups.[1].Value
    if tag = TOC || tag.StartsWith UNPROCESSED_TAG_WARNING then match'.Value
    else
        logger.Warning ("{file} -> Unprocessed tag: {tag}", fileInfo.FullName, match'.Value)
        sprintf "{%s%s}" UNPROCESSED_TAG_WARNING tag

let rec private processFile (logger:ILogger) (fileInfo:FileInfo) =
    let partialPath = sprintf @"..\%s\%s" fileInfo.Directory.Name fileInfo.Name
    logger.Debug ("Processing {partialPath}...", partialPath)
    let lines =
        File.ReadAllLines fileInfo.FullName
        |> List.ofArray
        |> List.filter (fun line -> not ((line.Trim ()).StartsWith SINGLE_LINE_COMMENT))
        // Note: When removing end-of-line comments, look for " //" rather than just "//" - else will inadvertently mangle "https://...".
        |> List.map (fun line -> match line.IndexOf (sprintf " %s" SINGLE_LINE_COMMENT) with | index when index > 0 -> line.Substring (0, index) | _ -> line)
    let folder (lines:string list, inMultiLineComment:bool) (line:string) =
        if inMultiLineComment then lines, not ((line.Trim ()).EndsWith MULTI_LINE_COMMENT__ENDS)
        else
            let inMultiLineComment = (line.Trim ()).StartsWith MULTI_LINE_COMMENT__STARTS
            (if not inMultiLineComment then line :: lines else lines), inMultiLineComment
    let lines, _ = lines |> List.fold folder ([], false)
    let contents = lines |> List.rev |> String.concat "\n"
    let contents = fileTag.Replace (contents, MatchEvaluator (processFileTag (processFile logger) fileInfo))
    let contents = cardTag.Replace (contents, MatchEvaluator (processCardTag fileInfo))
    let contents = bidTag.Replace (contents, MatchEvaluator (processBidTag fileInfo))
    let contents = handTag.Replace (contents, MatchEvaluator (processHandTag fileInfo))

    // TODO-NMB: More tags, e.g. auctions? deals?...

    let contents = anyTag.Replace (contents, MatchEvaluator (processUnprocessedTag logger fileInfo))
    logger.Debug ("...processed {partialPath}", partialPath)
    contents

let private tocTag = Regex (sprintf "{%s}" TOC)

let private processTocTag (logger:ILogger) (contents:string) (match':Match) =
    let namedAnchor = Regex "<a name=\"(.+)\">"
    let linkAndText line =
        let match' = namedAnchor.Match line
        if match'.Success then
            let anchor = match'.Groups.[1].Value
            Some (anchor, anchor.Replace ("_", " "))
        else None
    let (|H2|_|) (line:string) = if (line.Trim ()).StartsWith "## " then linkAndText line else None
    let (|H3|_|) (line:string) = if (line.Trim ()).StartsWith "### " then linkAndText line else None
    let (|H4|_|) (line:string) = if (line.Trim ()).StartsWith "#### " then linkAndText line else None
    logger.Information "Generating table-of-contents..."
    let toc =
        contents.Split '\n'
        |> List.ofArray
        |> List.choose (fun line ->
            match line with
            | H2 (link, text) -> Some (sprintf "* [**%s**](#%s)" text link)
            | H3 (link, text) -> Some (sprintf "  * [%s](#%s)" text link)
            | H4 (link, text) -> Some (sprintf "    * [_%s_](#%s)" text link)
            | _ -> None)
        |> String.concat "\n"
    logger.Information "...table-of-contents generated"
    toc

let processMd logger srcDir =
    let logger = logger |> sourcedLogger SOURCE
    let rootFileInfo = FileInfo (Path.Combine (srcDir, @"md\root.md"))
    logger.Information "Starting processing..."
    let contents = processFile logger rootFileInfo
    let contents =
        match tocTag.Matches contents |> List.ofSeq with
        | [] -> contents
        | [ _ ] -> tocTag.Replace (contents, MatchEvaluator (processTocTag logger contents))
        | match' :: _ ->
            logger.Warning ("Multiple {tag} tags found", match'.Value)
            contents
    logger.Information "...finished processing"
    let readmeFile = Path.Combine (rootFileInfo.Directory.Parent.Parent.FullName, "README.md")
    logger.Information ("Writing {readme}...", readmeFile)
    File.WriteAllText (readmeFile, contents)
    logger.Information("...{readme} written", readmeFile)
