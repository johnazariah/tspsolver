module UntypedBRKGA

open System
open System.IO

let rand = Random ()

type 't``[]`` with
    member this.Print (elementSelector : 't -> 'a) (elementFormatter : 'a -> string) name =
        this
        |> Array.map (elementSelector >> elementFormatter)
        |> (fun rg -> System.String.Join(',', rg))
        |> printfn "%s : %s" name

type Chromosome = private { Genes' : float[]; Length' : ChromosomeLength }
with
    member this.Genes  = this.Genes'
    member this.Length = this.Length'

    static member Random (length : ChromosomeLength) =
        let genes =
            [|
                for _ in 0 .. (length.unapply - 1) ->
                    rand.NextDouble()
            |]
        { Genes' = genes; Length' = length }

    static member Apply (genes : float[]) =
        { Genes' = genes; Length' = ChromosomeLength genes.Length }

    member this.ParametrizedUniformCrossover biasForThisParent otherParent =
        let genes =
            [|
                for i in 0 .. (this.Length'.unapply - 1) ->
                    if (rand.NextDouble() <= biasForThisParent)
                    then this.Genes'[i]
                    else otherParent.Genes'[i]
            |]
        { Genes' = genes; Length' = this.Length' }

and ChromosomeLength = | ChromosomeLength of int
with
    member this.unapply = match this with ChromosomeLength x -> x

and PopulationCount = | PopulationCount of int
with
    member inline this.unapply = match this with PopulationCount x -> x

and Fitness = | Fitness of float
with
    member inline private this.unapply = match this with Fitness x -> x
    static member op_LessThan (l : Fitness, r: Fitness) = l.unapply < r.unapply
    static member Worst = Fitness System.Double.MaxValue
    override this.ToString() =
        sprintf "%.2f" this.unapply

and TaggedChromosome<'t> = { Chromosome : Chromosome; Fitness : Fitness; Tag : 't }
with
    static member inline FitnessSelector cwf = cwf.Fitness
    static member inline apply decodeFunction fitnessFunction chromosome =
        let tag     = chromosome |> decodeFunction
        let fitness = tag        |> fitnessFunction
        { Chromosome = chromosome; Fitness = fitness; Tag = tag }

and Encoder<'t>         = 't -> Chromosome
and Decoder<'t>         = Chromosome -> 't
and FitnessFunction<'t> = 't -> Fitness

type PopulationParameters<'t> = {
    ChromosomeLength       : ChromosomeLength
    InitialPopulationCount : PopulationCount
    EncodeFunction         : Encoder<'t>
    DecodeFunction         : Decoder<'t>
    FitnessFunction        : FitnessFunction<'t>
}

type EvolutionParameters = private {
    ElitePercentage  : double
    MutantPercentage : double
    EliteBias        : double
}
with
    static member Initialize elitePercentage mutantPercentage eliteBias =
        let inline ensureClamped f n = if (f >= 0.0) && f < (1.0) then f else failwith $"{n} is not clamped to [0.0, 1.0)"
        {
            ElitePercentage  = ensureClamped elitePercentage  "elitePercentage"
            MutantPercentage = ensureClamped mutantPercentage "mutantPercentage"
            EliteBias        = ensureClamped eliteBias        "eliteBias"
        }

    static member Default =
        EvolutionParameters.Initialize 0.25 0.15 0.75

    override this.ToString() =
        $"{{ Elites: %.2f{this.ElitePercentage * 100.}%%; Mutants: %.2f{this.MutantPercentage * 100.}%%; Elite Bias: %.2f{this.EliteBias * 100.}%% }}"

let inline proportion total percentage = percentage * float total |> Math.Ceiling |> int

type Population<'t when 't : comparison> = private | Population of TaggedChromosome<'t>[]
with
    member this.Unapply = match this with Population x -> x
    member this.Best = this.Unapply[0]

    static member GetRandomChromosomes (pp : PopulationParameters<'t>) n =
        let random _ =
            Chromosome.Random pp.ChromosomeLength
            |> TaggedChromosome.apply<'t> pp.DecodeFunction pp.FitnessFunction

        [| for i in 0 .. (n - 1) -> random i |]
        |> Array.sortBy TaggedChromosome<'t>.FitnessSelector

    static member Random (pp : PopulationParameters<'t>) =
        Population.GetRandomChromosomes pp pp.InitialPopulationCount.unapply
        |> Population

    member this.Evolve (pp : PopulationParameters<'t>) (ep : EvolutionParameters) =
        let populationCount  = pp.InitialPopulationCount.unapply
        let members          = this.Unapply

        let eliteCount       = proportion members.Length  ep.ElitePercentage
        let mutantCount      = proportion populationCount ep.MutantPercentage
        let childrenCount    = populationCount - (eliteCount + mutantCount)

        let inline pickRandom (min, max) =
            members[System.Random.Shared.Next(min, max)]

        let elites =
            members |> Array.take eliteCount

        let mutants =
            Population.GetRandomChromosomes pp mutantCount

        let children =
            let crossover (elite, other) =
                elite.Chromosome.ParametrizedUniformCrossover ep.EliteBias other.Chromosome
                |> TaggedChromosome.apply<'t> pp.DecodeFunction pp.FitnessFunction

            [|
                for _ in 0 .. (childrenCount - 1) ->
                    (pickRandom (0, eliteCount), pickRandom (eliteCount, members.Length))
            |]
            |> Array.Parallel.map crossover

        [| elites; children; mutants |]
        |> Array.concat
        |> Array.sortBy TaggedChromosome<_>.FitnessSelector
        |> Population

let inline private SolveInternal<'t when 't : comparison> (populationParameters : PopulationParameters<'t>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let outerTime, innerTime = System.Diagnostics.Stopwatch(), System.Diagnostics.Stopwatch();
    let dot = num_iterations / 100

    outerTime.Start()
    let mutable population = Population.Random populationParameters
    let initial = population.Best
    let results =
        [|
            for i in 0 .. num_iterations ->
                if i % dot = 0 then printf "."

                innerTime.Restart()
                population <- population.Evolve populationParameters evolutionParameters
                innerTime.Stop()
                {| Iteration = i; IterationTime = innerTime.ElapsedMilliseconds; BestFitness = population.Best.Fitness |}
        |]
    printfn ""
    outerTime.Stop()
    {| TotalTime = outerTime.ElapsedMilliseconds; Results = results; Initial = initial; Final = population.Best |}

let Solve<'t when 't : comparison> (experimentName : string) (populationParameters : PopulationParameters<'t>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let results = SolveInternal<'t> populationParameters evolutionParameters num_iterations

    let fileName = (FileInfo $"BRKGA_Classic_{experimentName}_{System.DateTime.Now.Ticks}.csv").FullName
    let output = new StreamWriter(fileName)

    let emit (s : string) =
        output.WriteLine s

    try
        emit <| $"# {{ Experiment : {experimentName}; Iterations : {num_iterations}; Runtime (ms) : {results.TotalTime}; }}"
        emit <| $"# {{ HyperParameters : %O{evolutionParameters} }}"
        emit <| $"# {{ Population : {populationParameters.InitialPopulationCount}; InitialFitness : %O{results.Initial.Fitness}; FinalFitness : %O{results.Final.Fitness} }}"
        emit <| $"# {{ DecodedChromosome : %O{results.Final.Tag} }}"

        emit <| "Iteration, Fitness, Time"
        for r in results.Results |> Array.sortBy (fun t -> t.Iteration) do
            emit <| $"{r.Iteration}, %O{r.BestFitness}, {r.IterationTime}"


        printfn $"# {{ Experiment : {experimentName}; Iterations : {num_iterations}; Runtime (ms) : {results.TotalTime}; }}"
        printfn $"# {{ HyperParameters : %O{evolutionParameters} }}"
        printfn $"# {{ Population : {populationParameters.InitialPopulationCount}; InitialFitness : %O{results.Initial.Fitness}; FinalFitness : %O{results.Final.Fitness} }}"
        printfn $"# {{ DecodedChromosome : %O{results.Final.Tag} }}"
        printfn $"Wrote results to {fileName}"
    finally
        output.Flush ()
        output.Close ()