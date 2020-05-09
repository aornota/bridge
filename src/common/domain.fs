module Aornota.Bridge.Common.Domain

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two with
    member this.Hcp = match this with | Ace -> 4 | King -> 3 | Queen -> 2 | Jack -> 1 | _ -> 0

type Suit = | Spade | Heart | Diamond | Club

type Card = | Card of Rank * Suit

type BidSuit = | Suit of Suit | NoTrump

type BidLevel = | OneLevel | TwoLevel | ThreeLevel | FourLevel | FiveLevel | SixLevel | SevenLevel

type Bid = | Pass | Bid of BidLevel * BidSuit | Double | Redouble

// TODO-NMB: Categorize (active pattern? e.g. (Balanced|SemiBalanced|Unbalanced)) | &c....
