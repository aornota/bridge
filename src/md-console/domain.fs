module Aornota.Bridge.MdConsole.Domain

open Aornota.Bridge.Common.Domain

open FsToolkit.ErrorHandling

type Rank with
    static member ofChar char =
        match char with
        | 'A' -> Ok Ace | 'K' -> Ok King | 'Q' -> Ok Queen | 'J' -> Ok Jack | 'T' -> Ok Ten | '9' -> Ok Nine | '8' -> Ok Eight
        | '7' -> Ok Seven | '6' -> Ok Six | '5' -> Ok Five | '4' -> Ok Four | '3' -> Ok Three | '2' -> Ok Two
        | _ -> Error (sprintf "'%c' is not a %s" char (nameof Rank))
    member this.MdString =
        match this with
        | Ace -> "A" | King -> "K" | Queen -> "Q" | Jack -> "J" | Ten -> "T" | Nine -> "9" | Eight -> "8"
        | Seven -> "7" | Six -> "6" | Five -> "5" | Four -> "4" | Three -> "3" | Two -> "2"

type Suit with
    static member ofChar char =
        match char with
        | 's' -> Ok Spade | 'h' -> Ok Heart | 'd' -> Ok Diamond | 'c' -> Ok Club
        | _ -> Error (sprintf "'%c' is not a %s" char (nameof Suit))
    member this.MdString =
        let text = match this with | Spade -> "spade" | Heart -> "heart" | Diamond -> "diamond" | Club -> "club"
        "![{text}](https://raw.githubusercontent.com/aornota/bridge/master/src/resources/{text}.png)".Replace("{text}", text)

type Card with
    static member ofString (string:string) = result {
        match string.Length with
        | 2 ->
            let! rank = Rank.ofChar (string.[0])
            let! suit = Suit.ofChar (string.[1])
            return! Ok (Card (rank, suit))
        | _ -> return! Error (sprintf "'%s' is not a %s" string (nameof Card)) }
    member this.MdString =
        let (Card (rank, suit)) = this
        sprintf "%s%s" suit.MdString rank.MdString

type BidSuit with
    static member ofString string =
        match string with
        | "NT" -> Ok NoTrump | "S" -> Ok (Suit Spade) | "H" -> Ok (Suit Heart) | "D" -> Ok (Suit Diamond) | "C" -> Ok (Suit Club)
        | _ -> Error (sprintf "'%s' is not a %s" string (nameof BidSuit))
    member this.MdString =
        let text = match this with | NoTrump -> "NT" | Suit Spade -> "S" | Suit Heart -> "H" | Suit Diamond -> "D" | Suit Club -> "C"
        "![{text}](https://raw.githubusercontent.com/aornota/bridge/master/src/resources/{text}.png)".Replace("{text}", text)

type BidLevel with
    static member ofChar char =
        match char with
        | '1' -> Ok OneLevel | '2' -> Ok TwoLevel | '3' -> Ok ThreeLevel | '4' -> Ok FourLevel | '5' -> Ok FiveLevel | '6' -> Ok SixLevel | '7' -> Ok SevenLevel
        | _ -> Error (sprintf "'%c' is not a %s" char (nameof BidLevel))
    member this.MdString = match this with | OneLevel -> "1" | TwoLevel -> "2" | ThreeLevel -> "3" | FourLevel -> "4" | FiveLevel -> "5" | SixLevel -> "6" | SevenLevel -> "7"

type Bid with
    static member ofString (string:string) =  result {
        match string with
        | "-" | "pass" -> return! Ok Pass
        | "dbl" -> return! Ok Double
        | "rdbl" -> return! Ok Redouble
        | _ ->
            match string.Length with
            | 2 | 3 ->
                let! bidLevel = BidLevel.ofChar (string.[0])
                let! bidSuit = BidSuit.ofString (string.Substring 1)
                return! Ok (Bid (bidLevel, bidSuit))
            | _ -> return! Error (sprintf "'%s' is not a %s" string (nameof Bid)) }
    member this.MdString = match this with | Pass -> "-" | Bid (bidLevel, bidSuit) -> sprintf "%s%s" bidLevel.MdString bidSuit.MdString | Double -> "**dbl**" | Redouble -> "_**rdbl**_"
