module Aornota.Bridge.Domain.Formatting.Deal

open Aornota.Bridge.Domain.Core
open Aornota.Bridge.Domain.Deal
open Aornota.Bridge.Domain.Evaluation.Core
open Aornota.Bridge.Domain.Formatting.Auction
open Aornota.Bridge.Domain.Formatting.Core

type RelativeVulnerability with
    member this.Text = match this with | Favourable -> "Favourable" | Equal -> "Equal" | Unfavourable -> "Unfavourable"

type Vulnerabilities with
    member this.Text = $"{NorthSouth.ShortText} {this.Vulnerability(NorthSouth).TextLower} | {EastWest.ShortText} {this.Vulnerability(EastWest).TextLower}"
    member this.ShortText = $"{NorthSouth.ShortText} {this.Vulnerability(NorthSouth).ShortTextLower} | {EastWest.ShortText} {this.Vulnerability(EastWest).ShortTextLower}"
    member this.SummaryText =
        match this.Vulnerability(NorthSouth), this.Vulnerability(EastWest) with
        | NotVulnerable, NotVulnerable -> $"Neither {Vulnerable.ShortTextLower}"
        | NotVulnerable, Vulnerable -> $"{EastWest.ShortText} {Vulnerable.ShortTextLower}"
        | Vulnerable, NotVulnerable -> $"{NorthSouth.ShortText} {Vulnerable.ShortTextLower}"
        | Vulnerable, Vulnerable -> $"Both {Vulnerable.ShortTextLower}"

type Seat with
    member this.Text = match this with | FirstSeat -> "First seat" | SecondSeat -> "Second seat" | ThirdSeat -> "Third seat" | FourthSeat -> "Fourth seat"
    member this.ShortText = match this with | FirstSeat -> "1st seat" | SecondSeat -> "2nd seat" | ThirdSeat -> "3rd seat" | FourthSeat -> "4th seat"

type Deal with
    member private this.PositionText(position, highlightDealer) = if highlightDealer && position = this.Dealer then $"|{position.ShortText}|" else $" {position.ShortText} "
    member private this.PartnershipSummary(partnership) =
        let hand1, hand2 = match partnership with | NorthSouth -> this.Hand(North), this.Hand(South) | EastWest -> this.Hand(East), this.Hand(West)
        let spades1, hearts1, diamonds1, clubs1 = hand1.SuitCounts
        let spades2, hearts2, diamonds2, clubs2 = hand2.SuitCounts
        let shapeText = $"{spades1 + spades2}={hearts1 + hearts2}={diamonds1 + diamonds2}={clubs1 + clubs2}"
        $"{partnership.ShortText} -> {hand1.Hcp + hand2.Hcp} HCP | {shapeText} | {this.Vulnerabilities.Vulnerability(partnership).TextLower}"
    member this.Summary(highlightDealer, withPartnershipSummaries) =
        let northHand, eastHand, southHand, westHand = this.Hand(North), this.Hand(East), this.Hand(South), this.Hand(West)
        [
            $"{this.PositionText(North, highlightDealer)} -> {northHand.Text}"
            $"{this.PositionText(East, highlightDealer)} -> {eastHand.Text}"
            $"{this.PositionText(South, highlightDealer)} -> {southHand.Text}"
            $"{this.PositionText(West, highlightDealer)} -> {westHand.Text}"
            if withPartnershipSummaries then
                ""
                this.PartnershipSummary(NorthSouth)
                this.PartnershipSummary(EastWest)
        ]
    member this.Diagram(withPartnershipSummaries) =
        let addSpacesLeft count (text:string) = $"""{String.replicate count " "}{text}"""
        let padRight width (text:string) = if text.Length >= width then text.Substring(0, width) else $"""{text}{String.replicate (width - text.Length) " "}"""
        let shapeText (hand:Hand) = $"{hand.ShapeCategory.TextUpper} ({hand.SpecificShapeText})"
        let hcpText (hand:Hand) = $"{hand.Hcp} HCP"
        let northHand, eastHand, southHand, westHand = this.Hand(North), this.Hand(East), this.Hand(South), this.Hand(West)
        let northShapeText = shapeText northHand
        let northWidth = northShapeText.Length
        let northSpades, northHearts, northDiamonds, northClubs = northHand.CardsText(Spade), northHand.CardsText(Heart), northHand.CardsText(Diamond), northHand.CardsText(Club)
        let southShapeText = shapeText southHand
        let southWidth = southShapeText.Length
        let southSpades, southHearts, southDiamonds, southClubs = southHand.CardsText(Spade), southHand.CardsText(Heart), southHand.CardsText(Diamond), southHand.CardsText(Club)
        let northSouthCardsMax = [ northSpades ; northHearts ; northDiamonds ; northClubs ; southSpades ; southHearts ; southDiamonds ; southClubs ] |> List.map (fun text -> text.Length) |> List.max
        let northSouthIndent = (max northWidth southWidth- northSouthCardsMax) / 2
        let northShapeIndent = if northWidth > southWidth then 0 else (southWidth - northWidth) / 2
        let southShapeIndent = if southWidth > northWidth then 0 else (northWidth - southWidth) / 2
        let eastShapeText = shapeText eastHand
        let eastWidth = eastShapeText.Length
        let eastSpades, eastHearts, eastDiamonds, eastClubs = eastHand.CardsText(Spade), eastHand.CardsText(Heart), eastHand.CardsText(Diamond), eastHand.CardsText(Club)
        let eastCardsMax = [ eastSpades ; eastHearts ; eastDiamonds ; eastClubs ] |> List.map (fun text -> text.Length) |> List.max
        let eastIndent = (eastWidth - eastCardsMax) / 2
        let westShapeText = shapeText westHand
        let westWidth = westShapeText.Length
        let westSpades, westHearts, westDiamonds, westClubs = westHand.CardsText(Spade), westHand.CardsText(Heart), westHand.CardsText(Diamond), westHand.CardsText(Club)
        let westCardsMax = [ westSpades ; westHearts ; westDiamonds ; westClubs ] |> List.map (fun text -> text.Length) |> List.max
        let westIndent = (westWidth - westCardsMax) / 2
        [
            $"{this.Vulnerabilities.SummaryText |> padRight (westWidth + 2 + northSouthIndent)}{northSpades}"
            northHearts |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northDiamonds |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northClubs |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            ""
            hcpText northHand |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northShapeText |> addSpacesLeft (westWidth + 2 + northShapeIndent)
            ""
            $"{westSpades |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent + 15)}{eastSpades |> addSpacesLeft eastIndent}"
            $"{westHearts |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent+ 3)}{this.PositionText(North, true) |> padRight 12}{eastHearts |> addSpacesLeft eastIndent}"
            $"{westDiamonds |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent + 15)}{eastDiamonds |> addSpacesLeft eastIndent}"
            $"{westClubs |> addSpacesLeft westIndent|> padRight ((westWidth + northSouthIndent) - 2)}{this.PositionText(West, true) |> padRight 10}{this.PositionText(East, true) |> padRight 7}{eastClubs |> addSpacesLeft eastIndent}"
            ""
            $"{hcpText westHand |> addSpacesLeft westIndent|> padRight (westWidth + northSouthIndent + 3)}{this.PositionText(South, true) |> padRight 12}{hcpText eastHand |> addSpacesLeft eastIndent}"
            $"{westShapeText |> padRight (westWidth + northSouthIndent + 15)}{shapeText eastHand}"
            ""
            southSpades |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southHearts |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southDiamonds |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southClubs |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            ""
            hcpText southHand |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southShapeText |> addSpacesLeft (westWidth + 2 + southShapeIndent)
            if withPartnershipSummaries then
                ""
                this.PartnershipSummary(NorthSouth)
                this.PartnershipSummary(EastWest)
        ]
    member this.AuctionDiagram = auctionDiagram (this.Dealer, this.AuctionState, this.Bids)
