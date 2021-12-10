module Domain

type Name = | Name of string
with
    static member inline apply = Name
    member inline this.unapply = match this with | Name x -> x

type Comment = | Comment of string
with
    static member inline apply = Comment
    member inline this.unapply = match this with | Comment x -> x

type Dimension = | Dimension of int
with
    static member inline apply = Dimension
    member inline this.unapply = match this with | Dimension x -> x

type ProblemType =
    | TSP
    | ATSP
    | SOP
    | HCP
    | CVRP
    | TOUR

type NodeId = private | NodeId of int32
with
    static member directApply x = NodeId x
    static member zeroBasedApply x = NodeId (x + 1)
    member this.zeroBasedUnapply = match this with | NodeId x -> (x - 1)
    override this.ToString () =
        match this with NodeId x -> x
        |> sprintf "<%d>"

type Coordinate = | Coordinate of float
with
    member inline this.unapply = match this with Coordinate x -> x
    static member inline (-) (i : Coordinate, j : Coordinate) =
        i.unapply - j.unapply

type Demand = | Demand of int32
with
    static member inline apply = Demand
    member inline this.unapply = match this with | Demand x -> x

type Edge = { Node1 : NodeId; Node2 : NodeId }
with
    static member apply (node1 : NodeId, node2 : NodeId) =
        { Node1 = node1; Node2 = node2 }

type Tour = | Tour of NodeId[]
with
    override this.ToString () =
        match this with | Tour x -> x
        |> Array.map (fun n -> n.ToString ())
        |> (fun rg -> System.String.Join (", ", rg))
        |> sprintf "[ %s ]"

type Weight = | Weight of int
with
    static member inline apply = Weight
    member inline private this.unapply = match this with | Weight x -> x
    static member inline Zero = Weight 0
    static member inline Infinity = Weight -1
    static member inline (+) (l : Weight, r : Weight) =
        l.unapply + r.unapply
        |> Weight

    static member (<|>) (this : Weight, cast : int -> 't) =
        this.unapply |> cast


type WeightedCompleteGraph (name : Name, problemType : ProblemType,  comment : Comment, dimension : Dimension) = class
    let d = dimension.unapply
    let mutable fullMatrix : Weight[,] = Array2D.init d d (fun _ _ -> Weight.Infinity)

    member val Name        = name
    member val ProblemType = problemType
    member val Comment     = comment
    member val Dimension   = dimension
    member _.Item
        with get (x : NodeId, y : NodeId) = fullMatrix[x.zeroBasedUnapply, y.zeroBasedUnapply]
        and set (x : NodeId, y : NodeId) (value : Weight) =
            fullMatrix[x.zeroBasedUnapply, y.zeroBasedUnapply] <- value

    member this.TourLength tour =
        let cities = match tour with | Tour x -> x
        let mutable weight = Weight.Zero
        let mutable prev = cities[0]
        for i in 1 .. cities.Length - 1 do
            let curr = cities[i]
            weight <- weight + this[prev, curr]
            prev   <- curr
        weight <- weight + this[cities[cities.Length - 1], cities[0]] // loop back
        weight

    override this.ToString () =
        let sb = System.Text.StringBuilder ()
        let emitLine (s: string) = sb.AppendLine s |> ignore
        let emit     (s: string) = sb.Append s     |> ignore

        emitLine ""
        emitLine "{"
        emitLine $"\tName: {this.Name.unapply}"
        emitLine $"\tType: {this.ProblemType}"
        emitLine $"\tComment: {this.Comment.unapply}"
        emitLine $"\tDimension: {this.Dimension.unapply}"
        emitLine $"\tWeights:"
        emit "\t["
        for _y in [ 0 .. (d - 1)] do
            let y = NodeId.zeroBasedApply _y
            emit "\n\t"
            for _x in [ 0 .. (d - 1)] do
                this[NodeId.zeroBasedApply _x, y] <|> (fun v -> emit $" %4d{v}")
        emitLine ("\n\t]")
        emit "}"
        sb.ToString ()
end

