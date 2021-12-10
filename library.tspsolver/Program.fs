
open TSPParser
open UntypedBRKGA

open FsToolkit.ErrorHandling

let dataRoot = @"C:\code\TSP\tspsolver\data\tsplib95"
let input =
    [| dataRoot; "att48.tsp" |]
    |> System.IO.Path.Combine
    |> System.IO.File.ReadAllText

option {
    let! wcg = parseTextToWeightedCompleteGraph input
    let pp =
        {
            ChromosomeLength = ChromosomeLength wcg.Dimension.unapply
            InitialPopulationCount = PopulationCount (wcg.Dimension.unapply * 2)
            EncodeFunction = TSPBRKGA.encodeTSP
            DecodeFunction = TSPBRKGA.decodeTSP
            FitnessFunction = TSPBRKGA.fitnessTSP wcg
        }
    let ep = EvolutionParameters.Default
    do UntypedBRKGA.Solve ("TSP") pp ep 20_000

    TSPBRKGA.fitnessTSP wcg TSPBRKGA.referenceBestTour
    |> printfn "The Reference Best Tour had Fitness of %O"
} |> ignore
