module Aornota.Bridge.Common.Domain

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two with
    member this.Hcp = match this with | Ace -> 4 | King -> 3 | Queen -> 2 | Jack -> 1 | _ -> 0

type Suit = | Spade | Heart | Diamond | Club

type Card = | Card of Rank * Suit

type Hand = Card list

type BidSuit = | Suit of Suit | NoTrump

type BidLevel = | OneLevel | TwoLevel | ThreeLevel | FourLevel | FiveLevel | SixLevel | SevenLevel

type Bid = | Pass | Bid of BidLevel * BidSuit | Double | Redouble

let private suitCounts (hand:Hand) =
    let spadeCount = hand |> List.filter (fun (Card (_, suit)) -> suit = Spade) |> List.length
    let heartCount = hand |> List.filter (fun (Card (_, suit)) -> suit = Heart) |> List.length
    let diamondCount = hand |> List.filter (fun (Card (_, suit)) -> suit = Diamond) |> List.length
    let clubCount = hand |> List.filter (fun (Card (_, suit)) -> suit = Club) |> List.length
    spadeCount, heartCount, diamondCount, clubCount

let private voidCount (spadeCount, heartCount, diamondCount, clubCount) =
    (if spadeCount = 0 then 1 else 0) + (if heartCount = 0 then 1 else 0) + (if diamondCount = 0 then 1 else 0) + (if clubCount = 0 then 1 else 0)

let private singletonCount (spadeCount, heartCount, diamondCount, clubCount) =
    (if spadeCount = 1 then 1 else 0) + (if heartCount = 1 then 1 else 0) + (if diamondCount = 1 then 1 else 0) + (if clubCount = 1 then 1 else 0)

let private doubletonCount (spadeCount, heartCount, diamondCount, clubCount) =
    (if spadeCount = 2 then 1 else 0) + (if heartCount = 2 then 1 else 0) + (if diamondCount = 2 then 1 else 0) + (if clubCount = 2 then 1 else 0)

let (|Balanced|_|) (hand:Hand) =
    if hand.Length = 13 then
        let suitCounts = suitCounts hand
        if voidCount suitCounts = 0 && singletonCount suitCounts = 0 && doubletonCount suitCounts <= 1 then Some suitCounts else None
    else None

let (|SemiBalanced|_|) (hand:Hand) =
    if hand.Length = 13 then
        let suitCounts = suitCounts hand
        if voidCount suitCounts = 0 && singletonCount suitCounts = 0 && doubletonCount suitCounts = 2 then Some suitCounts else None
    else None

let (|Unbalanced|_|) (hand:Hand) =
    if hand.Length = 13 then
        let suitCounts = suitCounts hand
        if voidCount suitCounts > 0 || singletonCount suitCounts > 0 || doubletonCount suitCounts > 2 then Some suitCounts else None
    else None

let hcp (hand:Hand) =
    if hand.Length = 13 then Ok (hand |> List.sumBy (fun (Card (rank, _)) -> rank.Hcp))
    else Error (sprintf "HCP should only be used when %s contains 13 %ss" (nameof Hand) (nameof Card))
