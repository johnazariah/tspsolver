module TSPBRKGA

open Domain
open UntypedBRKGA

let encodeTSP : Encoder<Tour> =
    let encoder (t : Tour) : Chromosome =
        let cities = match t with Tour x -> x
        let genes =
            [| for _ in 0..(cities.Length - 1) -> rand.NextDouble() |]
            |> Array.sort
        cities
        |> Array.map (fun c -> genes[c.zeroBasedUnapply])
        |> Chromosome.Apply
    encoder

let decodeTSP : Decoder<Tour> =
    let decoder (chr : Chromosome) =
        chr.Genes
        |> Array.mapi (fun index gene -> (index, gene))
        |> Array.sortBy snd
        |> Array.map (fst >> NodeId.zeroBasedApply)
        |> Tour
    decoder

let fitnessTSP (wcg : WeightedCompleteGraph) : FitnessFunction<Tour> =
    fun (tour : Tour) ->
        (wcg.TourLength tour) <|> (float >> Fitness)

let referenceBestTour =
    [|
        NodeId.directApply 1
        NodeId.directApply 8
        NodeId.directApply 38
        NodeId.directApply 31
        NodeId.directApply 44
        NodeId.directApply 18
        NodeId.directApply 7
        NodeId.directApply 28
        NodeId.directApply 6
        NodeId.directApply 37
        NodeId.directApply 19
        NodeId.directApply 27
        NodeId.directApply 17
        NodeId.directApply 43
        NodeId.directApply 30
        NodeId.directApply 36
        NodeId.directApply 46
        NodeId.directApply 33
        NodeId.directApply 20
        NodeId.directApply 47
        NodeId.directApply 21
        NodeId.directApply 32
        NodeId.directApply 39
        NodeId.directApply 48
        NodeId.directApply 5
        NodeId.directApply 42
        NodeId.directApply 24
        NodeId.directApply 10
        NodeId.directApply 45
        NodeId.directApply 35
        NodeId.directApply 4
        NodeId.directApply 26
        NodeId.directApply 2
        NodeId.directApply 29
        NodeId.directApply 34
        NodeId.directApply 41
        NodeId.directApply 16
        NodeId.directApply 22
        NodeId.directApply 3
        NodeId.directApply 23
        NodeId.directApply 14
        NodeId.directApply 25
        NodeId.directApply 13
        NodeId.directApply 11
        NodeId.directApply 12
        NodeId.directApply 15
        NodeId.directApply 40
        NodeId.directApply 9
    |]
    |> Tour