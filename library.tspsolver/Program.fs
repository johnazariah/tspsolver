open TSPSolver.TSPParser

let dataRoot = @"C:\code\TSP\tspsolver\data\tsplib95"
let input =
    [| dataRoot; "att48.tsp" |]
    |> System.IO.Path.Combine
    |> System.IO.File.ReadAllText

parseTextToProblem input
|> printfn "%O"