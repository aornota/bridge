module Aornota.Bridge.Common.Domain

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two

type Suit = | Spade | Heart | Diamond | Club

type Card = | Card of Rank * Suit

type BidSuit = | Suit of Suit | NoTrump

type BidLevel = | OneLevel | TwoLevel | ThreeLevel | FourLevel | FiveLevel | SixLevel | SevenLevel

type Bid = | Pass | Bid of BidLevel * BidSuit | Double | Redouble
