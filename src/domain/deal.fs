module Aornota.Bridge.Domain.Deal

open Aornota.Bridge.Domain.Auction
open Aornota.Bridge.Domain.Core

type RelativeVulnerability = | Favourable | Equal | Unfavourable

type Vulnerabilities = private { NorthSouthVulnerability' : Vulnerability ; EastWestVulnerability' : Vulnerability } with
    static member Make(northSouthVulnerability, eastWestVulnerability) = { NorthSouthVulnerability' = northSouthVulnerability ; EastWestVulnerability' = eastWestVulnerability }
    member this.Vulnerability(partnership) = match partnership with | NorthSouth -> this.NorthSouthVulnerability' | EastWest -> this.EastWestVulnerability'
    member this.Vulnerability(position) = this.Vulnerability(Partnership.ForPosition(position))
    member this.RelativeVulnerability(partnership:Partnership) =
        match this.Vulnerability(partnership), this.Vulnerability(partnership.Opposition) with
        | NotVulnerable, Vulnerable -> Favourable
        | NotVulnerable, NotVulnerable | Vulnerable, Vulnerable -> Equal
        | Vulnerable, NotVulnerable -> Unfavourable

type Seat = | FirstSeat | SecondSeat | ThirdSeat | FourthSeat

type Deal = private {
    Vulnerabilities' : Vulnerabilities
    FirstSeatHand' : Hand
    SecondSeatHand' : Hand
    ThirdSeatHand' : Hand
    FourthSeatHand' : Hand
    Auction' : Auction } with
    static member Make(dealer, northSouthVulnerability, eastWestVulnerability) =
        let deck = Deck.MakeShuffled()
        let firstSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let secondSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let thirdSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let fourthSeatCards, _ = deck.Deal(CARDS_PER_HAND)
        {
            Vulnerabilities' = Vulnerabilities.Make(northSouthVulnerability, eastWestVulnerability)
            FirstSeatHand' = Hand.Make(firstSeatCards)
            SecondSeatHand' = Hand.Make(secondSeatCards)
            ThirdSeatHand' = Hand.Make(thirdSeatCards)
            FourthSeatHand' = Hand.Make(fourthSeatCards)
            Auction' = Auction.Make(dealer)
        }
    member this.Dealer = this.Auction'.Dealer
    member this.Vulnerabilities = this.Vulnerabilities'
    member this.SeatAndHand(position) =
        match this.Auction'.Dealer, position with
        | North, North | East, East | South, South | West, West -> FirstSeat, this.FirstSeatHand'
        | North, East | East, South | South, West | West, North -> SecondSeat, this.SecondSeatHand'
        | North, South | East, West | South, North | West, East -> ThirdSeat, this.ThirdSeatHand'
        | North, West | East, North | South, East | West, South -> FourthSeat, this.FourthSeatHand'
    member this.Seat(position) = fst (this.SeatAndHand(position))
    member this.Hand(position) = snd (this.SeatAndHand(position))
    member this.AuctionState = this.Auction'.State
    member this.Bids = this.Auction'.Bids
    member this.Bid(bidder, bid) = { this with Auction' = this.Auction'.Bid(bidder, bid) }
