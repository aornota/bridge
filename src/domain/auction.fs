module Aornota.Bridge.Domain.Auction

open Aornota.Bridge.Domain.Core

type Level = | OneLevel | TwoLevel | ThreeLevel | FourLevel | FiveLevel | SixLevel | SevenLevel with
    member this.Rank = match this with | OneLevel -> 1 | TwoLevel -> 2 | ThreeLevel -> 3 | FourLevel -> 4 | FiveLevel -> 5 | SixLevel -> 6 | SevenLevel -> 7
    member this.TricksRequired = (uint this.Rank) + 6u

type Strain = | Suit of Suit | NoTrump with
    member this.Rank = match this with | Suit suit -> suit.Rank | NoTrump -> Spade.Rank + 1

type Bid = | Pass | Bid of Level * Strain | Double | Redouble

type Stakes = Undoubled | Doubled | Redoubled

type Contract = | Contract of Level * Strain * Stakes * declarer:Position | PassedOut

type AuctionState = | Completed of Contract | AwaitingBid of Position * (Level * Strain * bool * bool) option

exception BidMustBeHigherThanException of Position * Level * Strain
exception CannotBidBecauseAuctionIsCompleteException of Position * Contract
exception CannotDoubleException of Position
exception CannotDoubleOrRedoubleException of Position
exception CannotRedoubleException of Position
exception IncorrectBidderException of Position * shouldBe:Position * shouldBeIsFirstBidder:bool
exception LatestNonPassBidCannotBeBothDoubleAndRedoubleException
exception UnableToDetermineDeclarerException of Partnership * Strain

type Auction = private { Dealer' : Position ; Bids' : (Position * Bid) list (* head is latest bid *) } with
    static member Make(dealer) = { Dealer' = dealer; Bids' = [] }
    member private this.LatestBid =
        match this.Bids' |> List.choose (fun (bidder, bid) -> match bid with | Bid (level, strain) -> Some (bidder, level, strain) | _ -> None) with
        | [] -> None
        | (bidder, position, bid) :: _ -> Some (bidder, position, bid)
    member private this.LatestNonPass = match this.Bids' |> List.filter (fun (_, bid) -> bid <> Pass) with | [] -> None | (position, bid) :: _ -> Some (position, bid)
    member private this.DoubledBy = match this.LatestNonPass with | Some (position, bid) when bid = Double -> Some position | _ -> None
    member private this.RedoubledBy = match this.LatestNonPass with | Some (position, bid) when bid = Redouble -> Some position | _ -> None
    member this.State =
        let rec passCount auction acc = match auction with | [] -> acc | (_, bid) :: t -> if bid = Pass then passCount t (acc + 1) else acc
        let contract =
            match this.LatestBid, passCount this.Bids' 0 with
            | None, 4 -> Some PassedOut
            | Some (bidder, level, strain), 3 ->
                let declarer =
                    match this.Bids' |> List.rev |> List.filter (fun (position, bid') -> match bid' with | Bid (_, strain') -> strain' = strain && not (position.IsOpponent(bidder)) | _ -> false) with
                    | [] -> raise (UnableToDetermineDeclarerException (Partnership.ForPosition(bidder), strain))
                    | (position, _) :: _ -> position
                let stakes =
                    match this.DoubledBy.IsSome, this.RedoubledBy.IsSome with
                    | false, false -> Undoubled
                    | true, false -> Doubled
                    | false, true -> Redoubled
                    | true, true -> raise LatestNonPassBidCannotBeBothDoubleAndRedoubleException
                Some (Contract (level, strain, stakes, declarer))
            | _ -> None
        match contract with
        | Some contract -> Completed contract
        | None ->
            let nextBidder = match this.Bids' with | [] -> this.Dealer' | (position, _) :: _ -> position.LHO
            match this.LatestBid with
            | None -> AwaitingBid (nextBidder, None)
            | Some (bidder, level, strain) ->
                let canDouble = this.DoubledBy.IsNone && bidder.IsOpponent(nextBidder)
                let canRedouble = this.DoubledBy.IsSome && this.RedoubledBy.IsNone && bidder.IsPartnership(nextBidder)
                AwaitingBid (nextBidder, Some (level, strain, canDouble, canRedouble))
    member this.Bid(bidder:Position, bid) =
        match this.State with
        | Completed contract -> raise (CannotBidBecauseAuctionIsCompleteException (bidder, contract))
        | AwaitingBid (nextBidder, latestBid) ->
            if nextBidder <> bidder then raise (IncorrectBidderException (bidder, nextBidder, this.Bids'.Length = 0))
            match latestBid with
            | None ->
                match bid with
                | Double | Redouble -> raise (CannotDoubleOrRedoubleException bidder)
                | _ -> { this with Bids' = (bidder, bid) :: this.Bids' }
            | Some (level, strain, canDouble, canRedouble) ->
                match bid with
                | Pass -> { this with Bids' = (bidder, bid) :: this.Bids' }
                | Bid (level', strain') ->
                    if level'.Rank < level.Rank || (level'.Rank = level.Rank && strain'.Rank <= strain.Rank) then raise (BidMustBeHigherThanException (bidder, level, strain))
                    { this with Bids' = (bidder, bid) :: this.Bids' }
                | Double ->
                    if not canDouble then raise (CannotDoubleException bidder)
                    { this with Bids' = (bidder, bid) :: this.Bids' }
                | Redouble ->
                    if not canRedouble then raise (CannotRedoubleException bidder)
                    { this with Bids' = (bidder, bid) :: this.Bids' }
    member this.Dealer = this.Dealer'
    member this.Bids = this.Bids' |> List.rev
