module Aornota.Bridge.Domain.Formatting.Auction

open Aornota.Bridge.Domain.Auction
open Aornota.Bridge.Domain.Core
open Aornota.Bridge.Domain.Formatting.Core

type Level with
    member this.Text = match this with | OneLevel -> "One" | TwoLevel -> "Two" | ThreeLevel -> "Three" | FourLevel -> "Four" | FiveLevel -> "Five" | SixLevel -> "Six" | SevenLevel -> "Seven"
    member this.ShortText = $"{this.Rank}"

type Strain with
    member this.Text = match this with | Suit suit -> suit.Text | NoTrump -> "No Trump"
    member this.TextPlural = $"{this.Text}s"
    member this.ShortText = match this with | Suit suit -> suit.ShortText | NoTrump -> "NT"

type Bid with
    member this.Text = match this with | Pass -> "Pass" | Bid (level, strain) -> $"{level.Text} {if level = OneLevel then strain.Text else strain.TextPlural}" | Double -> "Double" | Redouble -> "Redouble"
    member this.ShortText = match this with | Pass -> "pass" | Bid (level, strain) -> $"{level.ShortText}{strain.ShortText}" | Double -> "dbl" | Redouble -> "rdbl"

type Stakes with
    member this.Text = match this with | Undoubled -> "" | Doubled -> " (doubled)" | Redoubled -> " (redoubled)"
    member this.ShortText = match this with | Undoubled -> "" | Doubled -> "x" | Redoubled -> "xx"

type Contract with
    member this.ShortText = match this with | Contract (level, strain, stakes, declarer) -> $"{level.ShortText}{strain.ShortText}{stakes.ShortText} by {declarer.Text}" | PassedOut -> "passed out"

type private DiagramBid = | Bid of Bid | NotApplicable | Awaiting

let auctionDiagram (dealer, state, bids) =
    let padRight width (text:string) = if text.Length >= width then text.Substring(0, width) else $"""{text}{String.replicate (width - text.Length) " "}"""
    let positionHeader (position:Position) = if position = dealer then $"|{position.ShortText}|" else $" {position.ShortText}"
    let rec lines (bids:(Position * DiagramBid) list) acc =
        let line bids =
            let column (_, bid) = $""" {match bid with | Bid bid -> bid.ShortText | NotApplicable -> "" | Awaiting -> "..."}"""
            bids |> List.map (column >> padRight 6) |> String.concat ""
        match bids.Length with | n when n > 4 -> lines (bids |> List.skip 4) (line (bids |> List.take 4) :: acc) | _ -> (line bids :: acc) |> List.rev
    let rec addNotApplicable position acc = if position = North then acc else addNotApplicable position.RHO ((position, NotApplicable) :: acc)
    let bids = bids |> List.map (fun (position, bid) -> position, Bid bid)
    let bids = match state with | Completed _ -> addNotApplicable dealer bids | AwaitingBid (position, _) -> ((position, Awaiting) :: (addNotApplicable dealer bids |> List.rev)) |> List.rev
    [
        [ North ; East ; South; West ] |> List.map (positionHeader >> padRight 6) |> String.concat ""
        String.replicate 24 "-"
        yield! lines bids []
        ""
        match state with
        | Completed contract -> match contract with | Contract _ -> $"Contract is {contract.ShortText}" | PassedOut -> $"Deal is {contract.ShortText}"
        | AwaitingBid (position, _) -> $"Awaiting bid from {position.Text}"
    ]

type Auction with
    member this.Diagram = auctionDiagram (this.Dealer, this.State, this.Bids)
