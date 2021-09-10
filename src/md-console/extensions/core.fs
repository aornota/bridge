module Aornota.Bridge.MdConsole.Extensions.Core

open Aornota.Bridge.Domain.Core
open Aornota.Bridge.Domain.Formatting.Core

open FsToolkit.ErrorHandling

type Rank with
    static member OfChar(char) =
        match char with
        | 'A' -> Ok Ace | 'K' -> Ok King | 'Q' -> Ok Queen | 'J' -> Ok Jack | 'T' -> Ok Ten | '9' -> Ok Nine | '8' -> Ok Eight
        | '7' -> Ok Seven | '6' -> Ok Six | '5' -> Ok Five | '4' -> Ok Four | '3' -> Ok Three | '2' -> Ok Two
        | _ -> Error $"'{char}' is not a {nameof Rank}"
    member this.MdString = this.ShortText

type Suit with
    static member OfChar(char) =
        match char with
        | 's' -> Ok Spade | 'h' -> Ok Heart | 'd' -> Ok Diamond | 'c' -> Ok Club
        | _ -> Error $"'{char}' is not a {nameof Suit}"
    member this.MdString =
        let text = match this with | Spade -> "spade" | Heart -> "heart" | Diamond -> "diamond" | Club -> "club"
        "![{text}](https://raw.githubusercontent.com/aornota/bridge/master/src/resources/{text}.png)".Replace("{text}", text)

type Card with
    static member OfString(string:string) = result {
        match string.Length with
        | 2 ->
            let! rank = Rank.OfChar(string.[0])
            let! suit = Suit.OfChar(string.[1])
            return! Ok (Card.Make(rank, suit))
        | _ -> return! Error $"'{string}' is not a {nameof Card}" }
    member this.MdString = $"{this.Suit.MdString}{this.Rank.MdString}"
