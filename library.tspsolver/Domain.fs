namespace TSPSolver

module Domain =
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

    type NodeId = | NodeId of int32
    with
        static member inline apply = NodeId
        member inline this.unapply = match this with | NodeId x -> x

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

    type Weight = | Weight of int
    with
        static member inline apply = Weight
        member inline this.unapply = match this with | Weight x -> x
        static member inline Zero = Weight 0
        static member inline Infinity = Weight -1

    type WeightedCompleteGraph (name : Name, problemType : ProblemType,  comment : Comment, dimension : Dimension) = class
        let d = dimension.unapply
        let mutable fullMatrix : Weight[,] = Array2D.init d d (fun _ _ -> Weight.Infinity)

        member val Name        = name
        member val ProblemType = problemType
        member val Comment     = comment
        member val Dimension   = dimension
        member _.Item
            with get (x : NodeId, y : NodeId) = fullMatrix[x.unapply, y.unapply]
            and set (x : NodeId, y : NodeId) (value : Weight) =
                fullMatrix[x.unapply, y.unapply] <- value

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
                let y = NodeId _y
                emit "\n\t"
                for _x in [ 0 .. (d - 1)] do
                    emit $" %4d{this[NodeId _x, y].unapply}"
            emitLine ("\n\t]")
            emit "}"
            sb.ToString ()
    end
