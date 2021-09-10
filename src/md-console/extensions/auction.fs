module Aornota.Bridge.MdConsole.Extensions.Auction

open Aornota.Bridge.Domain.Auction
open Aornota.Bridge.Domain.Core
open Aornota.Bridge.Domain.Formatting.Auction

open FsToolkit.ErrorHandling

type Level with
    static member OfChar(char) =
        match char with
        | '1' -> Ok OneLevel | '2' -> Ok TwoLevel | '3' -> Ok ThreeLevel | '4' -> Ok FourLevel | '5' -> Ok FiveLevel | '6' -> Ok SixLevel | '7' -> Ok SevenLevel
        | _ -> Error $"'char' is not a {nameof Level}"
    member this.MdString = this.ShortText

type Strain with
    static member OfString(string) =
        match string with
        | "NT" -> Ok NoTrump | "S" -> Ok (Suit Spade) | "H" -> Ok (Suit Heart) | "D" -> Ok (Suit Diamond) | "C" -> Ok (Suit Club)
        | _ -> Error $"'{string}' is not a {nameof Strain}"
    member this.MdString =
        let text = match this with | NoTrump -> "NT" | Suit Spade -> "S" | Suit Heart -> "H" | Suit Diamond -> "D" | Suit Club -> "C"
        "![{text}](https://raw.githubusercontent.com/aornota/bridge/master/src/resources/{text}.png)".Replace("{text}", text)

type Bid with
    static member OfString(string:string) =  result {
        match string with
        | "-" | "pass" -> return! Ok Pass
        | "dbl" -> return! Ok Double
        | "rdbl" -> return! Ok Redouble
        | _ ->
            match string.Length with
            | 2 | 3 ->
                let! level = Level.OfChar(string.[0])
                let! strain = Strain.OfString(string.Substring 1)
                return! Ok (Bid (level, strain))
            | _ -> return! Error $"'{string}' is not a {nameof Bid}" }
    member this.MdString = match this with | Pass -> "pass" | Bid (level, strain) -> $"{level.MdString}{strain.MdString}" | Double -> "**dbl**" | Redouble -> "_**rdbl**_"
