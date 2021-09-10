module Aornota.Bridge.Domain.Evaluation.Core

open Aornota.Bridge.Domain.Core

exception UnexpectedHandShapeException

type Hand with
    member this.Hcp = this.Cards |> Seq.sumBy (fun card -> card.Rank.Hcp)
    member this.CardsForSuit(suit) = this.Cards |> Seq.filter (fun card -> card.Suit = suit) |> List.ofSeq
    member this.CountForSuit(suit) = this.CardsForSuit(suit).Length
    member this.SuitCounts = this.CountForSuit(Spade), this.CountForSuit(Heart), this.CountForSuit(Diamond), this.CountForSuit(Club)
    member this.Shape =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        match [ spadeCount ; heartCount ; diamondCount ; clubCount ] |> List.sortDescending with
        // longest suit is four cards
        | [ 4 ; 3 ; 3 ; 3 ] -> FourThreeThreeThree | [ 4 ; 4 ; 3 ; 2 ] -> FourFourThreeTwo | [ 4 ; 4 ; 4 ; 1 ] -> FourFourFourOne
        // longest suit is five cards
        | [ 5 ; 3 ; 3 ; 2 ] -> FiveThreeThreeTwo | [ 5 ; 4 ; 2 ; 2 ] -> FiveFourTwoTwo | [ 5 ; 4 ; 3 ; 1 ] -> FiveFourThreeOne | [ 5 ; 4 ; 4 ; 0 ] -> FiveFourFourZero
        | [ 5 ; 5 ; 2 ; 1 ] -> FiveFiveTwoOne | [ 5 ; 5 ; 3 ; 0 ] -> FiveFiveThreeZero
        // longest suit is six cards
        | [ 6 ; 3 ; 2 ; 2 ] -> SixThreeTwoTwo | [ 6 ; 3 ; 3 ; 1 ] -> SixThreeThreeOne | [ 6 ; 4 ; 2 ; 1 ] -> SixFourTwoOne | [ 6 ; 4 ; 3 ; 0 ] -> SixFourThreeZero
        | [ 6 ; 5 ; 1 ; 1 ] -> SixFiveOneOne | [ 6 ; 5 ; 2 ; 0 ] -> SixFiveTwoZero | [ 6 ; 6 ; 1 ; 0 ] -> SixSixOneZero
        // longest suit is seven cards
        | [ 7 ; 2 ; 2 ; 2 ] -> SevenTwoTwoTwo | [ 7 ; 3 ; 2 ; 1 ] -> SevenThreeTwoOne | [ 7 ; 3 ; 3 ; 0 ] -> SevenThreeThreeZero | [ 7 ; 4 ; 1 ; 1 ] -> SevenFourOneOne
        | [ 7 ; 4 ; 2 ; 0 ] -> SevenFourTwoZero | [ 7 ; 5 ; 1 ; 0 ] -> SevenFiveOneZero | [ 7 ; 6 ; 0 ; 0 ] -> SevenSixZeroZero
        // longest suit is eight cards
        | [ 8 ; 2 ; 2 ; 1 ] -> EightTwoTwoOne | [ 8 ; 3 ; 1 ; 1 ] -> EightThreeOneOne | [ 8 ; 3 ; 2 ; 0 ] -> EightThreeTwoZero | [ 8 ; 4 ; 1 ; 0 ] -> EightFourOneZero
        | [ 8 ; 5 ; 0 ; 0 ] -> EightFiveZeroZero
        // longest suit is nine cards
        | [ 9 ; 2 ; 1 ; 1 ] -> NineTwoOneOne | [ 9 ; 2 ; 2 ; 0 ] -> NineTwoTwoZero | [ 9 ; 3 ; 1 ; 0 ] -> NineThreeOneZero | [ 9 ; 4 ; 0 ; 0 ] -> NineFourZeroZero
        // longest suit is ten cards
        | [ 10 ; 1 ; 1 ; 1 ] -> TenOneOneOne | [ 10 ; 2 ; 1 ; 0 ] -> TenTwoOneZero | [ 10 ; 3 ; 0 ; 0 ] -> TenThreeZeroZero
        // longest suit is eleven cards
        | [ 11 ; 1 ; 1 ; 0 ] -> ElevenOneOneZero | [ 11 ; 2 ; 0 ; 0 ] -> ElevenTwoZeroZero
        // longest suit is twelve cards
        | [ 12 ; 1 ; 0 ; 0 ] -> TwelveOneZeroZero
        // longest suit is thirteen cards
        | [ 13 ; 0 ; 0 ; 0 ] -> ThirteenZeroZeroZero
        | _ -> raise UnexpectedHandShapeException
    member this.ShapeCategory = ShapeCategory.Make(this.Shape)
    member this.SpecificShapeText =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        $"{spadeCount}={heartCount}={diamondCount}={clubCount}"
