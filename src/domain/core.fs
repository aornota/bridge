module Aornota.Bridge.Domain.Core

open Aornota.Bridge.Common.Mathy

let [<Literal>] CARDS_PER_HAND = 13

type [<Measure>] hcp // high-card points

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two with
    member this.Hcp = match this with | Ace -> 4<hcp> | King -> 3<hcp> | Queen -> 2<hcp> | Jack -> 1<hcp> | _ -> 0<hcp>

type Suit = | Spade | Heart | Diamond | Club with
    member this.IsMajor = match this with | Spade | Heart -> true | Diamond | Club -> false
    member this.IsMinor = not this.IsMajor
    member this.Rank = match this with | Spade -> 4 | Heart -> 3 | Diamond -> 2 | Club -> 1

type Card = private { Rank' : Rank ; Suit' : Suit } with
    static member Make(rank, suit) = { Rank' = rank ; Suit' = suit }
    member this.Rank = this.Rank'
    member this.Suit = this.Suit'

exception IncorrectDistinctCardCountForUnshuffledDeckException of required:int
exception InsufficientCardsInDeckException of requested:int

type Deck = private { DeckCards' : Card list } with
    static member private RequiredUnshuffledCount = CARDS_PER_HAND * 4
    static member private Unshuffled =
        let cards =
            [ Spade ; Heart ; Diamond ; Club ]
            |> List.collect (fun suit -> [ Ace ; King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ] |> List.map (fun rank -> Card.Make(rank, suit)))
            |> Set.ofList
        if cards.Count <> Deck.RequiredUnshuffledCount then raise (IncorrectDistinctCardCountForUnshuffledDeckException Deck.RequiredUnshuffledCount)
        cards
    static member MakeShuffled() = { DeckCards' = Deck.Unshuffled |> List.ofSeq |> List.zip (randoms Deck.RequiredUnshuffledCount) |> List.sortBy fst |> List.map snd }
    member this.Count = this.DeckCards'.Length
    member this.Deal(count) =
        if count > this.Count then raise (InsufficientCardsInDeckException count)
        this.DeckCards' |> List.take count, { DeckCards' = this.DeckCards' |> List.skip count }

type Shape =
    | FourThreeThreeThree | FourFourThreeTwo | FourFourFourOne // longest suit is four cards
    | FiveThreeThreeTwo | FiveFourTwoTwo | FiveFourThreeOne | FiveFourFourZero | FiveFiveTwoOne | FiveFiveThreeZero // longest suit is five cards
    | SixThreeTwoTwo | SixThreeThreeOne | SixFourTwoOne | SixFourThreeZero | SixFiveOneOne | SixFiveTwoZero | SixSixOneZero // longest suit is six cards
    | SevenTwoTwoTwo | SevenThreeTwoOne | SevenThreeThreeZero | SevenFourOneOne | SevenFourTwoZero | SevenFiveOneZero | SevenSixZeroZero // longest suit is seven cards
    | EightTwoTwoOne | EightThreeOneOne | EightThreeTwoZero | EightFourOneZero | EightFiveZeroZero // longest suit is eight cards
    | NineTwoOneOne | NineTwoTwoZero | NineThreeOneZero | NineFourZeroZero // longest suit is nine cards
    | TenOneOneOne | TenTwoOneZero | TenThreeZeroZero // longest suit is ten cards
    | ElevenOneOneZero | ElevenTwoZeroZero // longest suit is eleven cards
    | TwelveOneZeroZero // longest suit is twelve cards
    | ThirteenZeroZeroZero // longest suit is thirteen cards

type ShapeCategory =
    | Balanced // no singletons or voids and at most one doubleton
    | SemiBalanced // no singletons or voids and exactly two doubletons
    | Unbalanced // at least one singleton or void (or three doubletons) and exactly nine cards between two longest suits
    | VeryUnbalanced // at least one singleton or void and ten or more cards between two longest suits
    with
    static member Make = function
        | FourThreeThreeThree | FourFourThreeTwo | FiveThreeThreeTwo -> Balanced
        | FiveFourTwoTwo | SixThreeTwoTwo -> SemiBalanced
        | FourFourFourOne | FiveFourThreeOne | FiveFourFourZero | SixThreeThreeOne | SevenTwoTwoTwo -> Unbalanced
        | _ -> VeryUnbalanced

exception IncorrectDistinctCardCountForHandException of required:int

type Hand = private { HandCards' : Set<Card> } with
    static member Make(cards) =
        let asSet = cards |> Set.ofList
        if asSet.Count <> CARDS_PER_HAND then raise (IncorrectDistinctCardCountForHandException CARDS_PER_HAND)
        { HandCards' = asSet }
    member this.Cards = this.HandCards' |> Seq.sortBy (fun card -> card.Suit, card.Rank) |> List.ofSeq

type Position = | North | East | South | West with
    member this.LHO = match this with | North -> East | East -> South | South -> West | West -> North
    member this.Partner = match this with | North -> South | East -> West | South -> North | West -> East
    member this.RHO = match this with | North -> West | East -> North | South -> East | West -> South
    member this.IsPartner(position) = this.Partner = position
    member this.IsPartnership(position) = this = position || this.IsPartner(position)
    member this.IsOpponent(position) = not (this.IsPartnership(position))

type Partnership = NorthSouth | EastWest with
    static member ForPosition(position) = match position with North | South -> NorthSouth | East | West -> EastWest
    member this.Opposition = match this with | NorthSouth -> EastWest | EastWest -> NorthSouth

type Vulnerability = | NotVulnerable | Vulnerable
