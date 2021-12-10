//
// Parses TSP files into a weighted graph
//
// http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp95.pdf
module TSPParser

open Domain
open FParsec
open FsToolkit.ErrorHandling

[<AutoOpen>]
module private AST =
    open System.Collections.Generic

    type CVRPCapacity = | CVRPCapacity of int
    with
        static member inline apply = CVRPCapacity
        member inline this.unapply = match this with | CVRPCapacity x -> x

    type EdgeWeightType =
        | EXPLICIT
        | EUC_2D
        | EUC_3D
        | MAX_2D
        | MAX_3D
        | MAN_2D
        | MAN_3D
        | CEIL_2D
        | GEO
        | ATT
        | XRAY1
        | XRAY2
        | SPECIAL

    type EdgeWeightFormat =
        | FUNCTION
        | FULL_MATRIX
        | UPPER_ROW
        | LOWER_ROW
        | UPPER_DIAG_ROW
        | LOWER_DIAG_ROW
        | UPPER_COL
        | LOWER_COL
        | UPPER_DIAG_COL
        | LOWER_DIAG_COL

    type EdgeDataFormat =
        | EDGE_LIST
        | ADJ_LIST

    type NodeCoordType =
        | TWOD_COORDS
        | THREED_COORDS
        | NO_COORDS

    type DataDisplayType =
        | COORD_DISPLAY
        | TWOD_DISPLAY
        | NO_DISPLAY

    type NodeCoord2D = { NodeId : NodeId; X : Coordinate; Y : Coordinate }
    with
        static member apply ((nodeId, x), y) = { NodeId = nodeId; X = x; Y = y }
        static member private L2 (i : NodeCoord2D, j : NodeCoord2D) =
            let xd = i.X - j.X
            let yd = i.Y - j.Y
            (xd * xd) + (yd * yd)
            |> sqrt

        static member EuclideanDistance =
            NodeCoord2D.L2 >> int32

        static member CeilingEuclideanDistance =
            NodeCoord2D.L2 >> ceil >> int32

        static member PseudoEuclideanDistance (i : NodeCoord2D, j : NodeCoord2D) =
            let xd = i.X - j.X
            let yd = i.Y - j.Y
            let rij = sqrt ((xd * xd) + (yd * yd) / 10.0)
            let tij = floor rij
            int32 (if tij < rij then tij + 1.0 else tij)

        static member ManhattanDistance (i : NodeCoord2D, j : NodeCoord2D) =
            let xd = i.X - j.X |> abs
            let yd = i.Y - j.Y |> abs
            xd + yd
            |> int32

        static member MaximumDistance (i : NodeCoord2D, j : NodeCoord2D) =
            let xd = i.X - j.X |> abs |> int32
            let yd = i.Y - j.Y |> abs |> int32
            max xd yd

        static member GeographicalDistance  (i : NodeCoord2D, j : NodeCoord2D) =
            let RRR = 6378.388

            let toRadians (f : float) =
                let deg = floor f
                let min = f - deg
                System.Math.PI * (deg + 5.0 * min / 3.0) / 180.0

            let (lat_i, long_i) = (toRadians i.X.unapply, toRadians i.Y.unapply)
            let (lat_j, long_j) = (toRadians j.X.unapply, toRadians j.Y.unapply)

            let q1 = long_i - long_j |> cos
            let q2 = lat_i - lat_j   |> cos
            let q3 = lat_i + lat_j   |> cos
            let f = 0.5*((1.0+q1)*q2 - (1.0-q1)*q3) |> acos
            RRR * f + 1.0
            |> int32

    and NodeCoord3D = { NodeId : NodeId; X : Coordinate; Y : Coordinate; Z : Coordinate}
    with
        static member apply (((nodeId, x), y), z) = { NodeId = nodeId; X = x; Y = y; Z = z }
        static member EuclideanDistance (i : NodeCoord3D, j : NodeCoord3D) =
            let xd = i.X - j.X
            let yd = i.Y - j.Y
            let zd = i.Z - j.Z
            (xd * xd) + (yd * yd) + (zd * zd)
            |> sqrt
            |> int32

        static member ManhattanDistance (i : NodeCoord3D, j : NodeCoord3D) =
            let xd = i.X - j.X |> abs
            let yd = i.Y - j.Y |> abs
            let zd = i.Z - j.Z |> abs
            xd + yd + zd
            |> int32

        static member MaximumDistance (i : NodeCoord3D, j : NodeCoord3D) =
            let xd = i.X - j.X |> abs |> int32
            let yd = i.Y - j.Y |> abs |> int32
            let zd = i.Z - j.Z |> abs |> int32
            xd
            |> max yd
            |> max zd

    type NodeCoordSection =
        | Nodes2D of NodeCoord2D list
        | Nodes3D of NodeCoord3D list

    type DepotsSection =
        | Depots of NodeId list

    type DemandsSection =
        | Demands of Dictionary<NodeId, Demand>

    type EdgeDataSection =
        | EdgeList of Edge list
        | AdjList of Dictionary<NodeId, NodeId list>

    type FixedEdgesSection =
        | FixedEdgeList of Edge list

    type DisplayDataSection =
        | Nodes2D of NodeCoord2D list

    type ToursSection =
        | Tours of Tour list
    and Tour =
        | Nodes of NodeId list

    type EdgeWeightSection =
        | WeightLines of WeightLine list
        member inline this.unapply = match this with | WeightLines x -> x

    and WeightLine =
        | WeightLine of Weight list
        member inline this.unapply = match this with | WeightLine x -> x

    type EofSection =
        | EOF

    type Problem =
        {
            Name               : Name               option
            Comment            : Comment            option
            Type               : ProblemType        option
            Dimension          : Dimension          option
            EdgeWeightType     : EdgeWeightType     option
            CVRPCapacity       : CVRPCapacity       option
            EdgeWeightFormat   : EdgeWeightFormat   option
            EdgeDataFormat     : EdgeDataFormat     option
            NodeCoordType      : NodeCoordType      option
            DataDisplayType    : DataDisplayType    option
            NodeCoordSection   : NodeCoordSection   option
            DepotsSection      : DepotsSection      option
            DemandsSection     : DemandsSection     option
            EdgeDataSection    : EdgeDataSection    option
            FixedEdgesSection  : FixedEdgesSection  option
            DisplayDataSection : DisplayDataSection option
            ToursSection       : ToursSection       option
            EdgeWeightSection  : EdgeWeightSection  option
            EofSection         : EofSection         option
        }
    with
        static member zero =
            {
                Name               = None
                Comment            = None
                Type               = None
                Dimension          = None
                CVRPCapacity       = None
                EdgeWeightType     = None
                EdgeWeightFormat   = None
                EdgeDataFormat     = None
                NodeCoordType      = None
                DataDisplayType    = None
                NodeCoordSection   = None
                DepotsSection      = None
                DemandsSection     = None
                EdgeDataSection    = None
                FixedEdgesSection  = None
                DisplayDataSection = None
                ToursSection       = None
                EdgeWeightSection  = None
                EofSection         = None
            }

    type ProblemComponent =
        | SpecificationName             of Name
        | SpecificationComment          of Comment
        | SpecificationType             of ProblemType
        | SpecificationDimension        of Dimension
        | SpecificationCVRPCapacity     of CVRPCapacity
        | SpecificationEdgeWeightType   of EdgeWeightType
        | SpecificationEdgeWeightFormat of EdgeWeightFormat
        | SpecificationEdgeDataFormat   of EdgeDataFormat
        | SpecificationNodeCoordType    of NodeCoordType
        | SpecificationDataDisplayType  of DataDisplayType
        | SectionNodeCoords             of NodeCoordSection
        | SectionDepots                 of DepotsSection
        | SectionDemands                of DemandsSection
        | SectionEdgeData               of EdgeDataSection
        | SectionFixedEdges             of FixedEdgesSection
        | SectionDisplayData            of DisplayDataSection
        | SectionTours                  of ToursSection
        | SectionEdgeWeights            of EdgeWeightSection
        | SectionEof                    of EofSection
    with
        static member coalesce =
            List.fold
                (
                    fun (problem : Problem) -> function
                        | SpecificationName             c -> { problem with Name               = Some c }
                        | SpecificationComment          c -> { problem with Comment            = Some c }
                        | SpecificationType             c -> { problem with Type               = Some c }
                        | SpecificationDimension        c -> { problem with Dimension          = Some c }
                        | SpecificationCVRPCapacity     c -> { problem with CVRPCapacity       = Some c }
                        | SpecificationEdgeWeightType   c -> { problem with EdgeWeightType     = Some c }
                        | SpecificationEdgeWeightFormat c -> { problem with EdgeWeightFormat   = Some c }
                        | SpecificationEdgeDataFormat   c -> { problem with EdgeDataFormat     = Some c }
                        | SpecificationNodeCoordType    c -> { problem with NodeCoordType      = Some c }
                        | SpecificationDataDisplayType  c -> { problem with DataDisplayType    = Some c }
                        | SectionNodeCoords             c -> { problem with NodeCoordSection   = Some c }
                        | SectionDepots                 c -> { problem with DepotsSection      = Some c }
                        | SectionDemands                c -> { problem with DemandsSection     = Some c }
                        | SectionEdgeData               c -> { problem with EdgeDataSection    = Some c }
                        | SectionFixedEdges             c -> { problem with FixedEdgesSection  = Some c }
                        | SectionDisplayData            c -> { problem with DisplayDataSection = Some c }
                        | SectionTours                  c -> { problem with ToursSection       = Some c }
                        | SectionEdgeWeights            c -> { problem with EdgeWeightSection  = Some c }
                        | SectionEof                    c -> { problem with EofSection         = Some c }
                )
                Problem.zero

[<AutoOpen>]
module private Transform =
    let toWeightedCompleteGraph (problem : Problem) : WeightedCompleteGraph option =
        let copyExplicitEdgeWeights (problem : Problem) (g : WeightedCompleteGraph) : WeightedCompleteGraph option =
            let dim = g.Dimension.unapply

            let copyFullWeights (lines : WeightLine list) =
                let copySingleLine (g : WeightedCompleteGraph, y : int) (wl : WeightLine) =
                    wl.unapply
                    |> List.fold (fun x w -> g[NodeId.zeroBasedApply x, NodeId.zeroBasedApply y] <- w;  x + 1) 0
                    |> (fun x -> if x > dim then failwith $"Out of bounds! Expecting {dim} weights in line {y} but found {x}")
                    (g, y + 1)
                try
                    lines
                    |> List.fold copySingleLine (g, 0)
                    |> (fun (g, y) -> if y > dim then failwith $"Out of bounds! Expecting {dim} lines but found {y}" else Some g)
                with
                | _ -> None

            option {
                let! format = problem.EdgeWeightFormat
                let! data   = problem.EdgeWeightSection
                return!
                    match format with
                    | FULL_MATRIX       -> copyFullWeights data.unapply
                    | FUNCTION          -> failwith "Not Yet Implemented"
                    | UPPER_ROW         -> failwith "Not Yet Implemented"
                    | LOWER_ROW         -> failwith "Not Yet Implemented"
                    | UPPER_DIAG_ROW    -> failwith "Not Yet Implemented"
                    | LOWER_DIAG_ROW    -> failwith "Not Yet Implemented"
                    | UPPER_COL         -> failwith "Not Yet Implemented"
                    | LOWER_COL         -> failwith "Not Yet Implemented"
                    | UPPER_DIAG_COL    -> failwith "Not Yet Implemented"
                    | LOWER_DIAG_COL    -> failwith "Not Yet Implemented"
            }

        let computeEdgeWeights (f2, f3) (problem : Problem) (g : WeightedCompleteGraph) : WeightedCompleteGraph option =
            let dim = g.Dimension.unapply
            let computeEdgeWeight (weight_func : ('n * 'n -> int) option) (unapply : 'n -> int) (nodes : 'n list) =
                option {
                    let! f = weight_func
                    let m = nodes |> List.map (fun n -> (unapply n, n)) |> Map.ofList
                    for p1 in 0 .. (dim - 1) do
                        for p2 in 0 .. (dim - 1) do
                            let d = f (m[p1], m[p2])
                            g[NodeId.zeroBasedApply p2, NodeId.zeroBasedApply p1] <- Weight d
                    return g
                }

            problem.NodeCoordSection
            |> Option.bind (function
                | NodeCoordSection.Nodes2D nodes -> computeEdgeWeight f2 (fun (n : NodeCoord2D) -> n.NodeId.zeroBasedUnapply) nodes
                | NodeCoordSection.Nodes3D nodes -> computeEdgeWeight f3 (fun (n : NodeCoord3D) -> n.NodeId.zeroBasedUnapply) nodes)

        option {
            let! name           = problem.Name
            let! comment        = problem.Comment
            let! dimension      = problem.Dimension
            let! problemType    = problem.Type
            let! edgeWeightType = problem.EdgeWeightType

            let g = WeightedCompleteGraph (name, problemType, comment, dimension)

            return!
                match edgeWeightType with
                | EXPLICIT -> copyExplicitEdgeWeights problem g
                | EUC_2D   -> computeEdgeWeights (Some NodeCoord2D.EuclideanDistance,        Some NodeCoord3D.EuclideanDistance) problem g
                | EUC_3D   -> computeEdgeWeights (Some NodeCoord2D.EuclideanDistance,        Some NodeCoord3D.EuclideanDistance) problem g
                | MAX_2D   -> computeEdgeWeights (Some NodeCoord2D.MaximumDistance,          Some NodeCoord3D.MaximumDistance)   problem g
                | MAX_3D   -> computeEdgeWeights (Some NodeCoord2D.MaximumDistance,          Some NodeCoord3D.MaximumDistance)   problem g
                | MAN_2D   -> computeEdgeWeights (Some NodeCoord2D.ManhattanDistance,        Some NodeCoord3D.ManhattanDistance) problem g
                | MAN_3D   -> computeEdgeWeights (Some NodeCoord2D.ManhattanDistance,        Some NodeCoord3D.ManhattanDistance) problem g
                | CEIL_2D  -> computeEdgeWeights (Some NodeCoord2D.CeilingEuclideanDistance, None)                               problem g
                | GEO      -> computeEdgeWeights (Some NodeCoord2D.GeographicalDistance,     None)                               problem g
                | ATT      -> computeEdgeWeights (Some NodeCoord2D.PseudoEuclideanDistance,  None)                               problem g
                | XRAY1    -> failwith "Not Yet Implemented"
                | XRAY2    -> failwith "Not Yet Implemented"
                | SPECIAL  -> failwith "Not Yet Implemented"
        }

[<AutoOpen>]
module private Parse =
    let spacesExceptNewline : Parser<unit, unit> = (skipChar '\t' <|> skipChar ' ') |> many |>> ignore
    let ws_x_nl p = spacesExceptNewline >>. p .>> spacesExceptNewline
    let ws p = spaces >>. p .>> spaces
    let wstr = pstring >> ws >> attempt

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
#if true && PARSER_TRACE
        fun stream ->
            printfn $"{stream.Position}: Entering {label}"
            let reply = p stream
            printfn $"{stream.Position}: Leaving {label} ({reply.Status})"
            reply
#else
        p <?> label
#endif

    let tillEndOfLine : Parser<string, unit> =
        manyCharsTill anyChar newline

    let parseSpecification prefix parser =
        ws (skipStringCI prefix) >>. ws (skipChar ':') >>. parser .>> opt newline

    let specificationName : Parser<ProblemComponent, unit>  =
        parseSpecification "NAME"    tillEndOfLine
        |>> Name.apply
        |>> ProblemComponent.SpecificationName
        <!> "NAME"

    let specificationComment : Parser<ProblemComponent, unit> =
        parseSpecification "COMMENT" tillEndOfLine
        |>> Comment.apply
        |>> ProblemComponent.SpecificationComment
        <!> "COMMENT"

    let specificationType =
        choice
            [
                wstr "TSP"  >>. preturn TSP
                wstr "ATSP" >>. preturn ATSP
                wstr "SOP"  >>. preturn SOP
                wstr "HCP"  >>. preturn HCP
                wstr "CVRP" >>. preturn CVRP
                wstr "TOUR" >>. preturn TOUR
            ]
        |> parseSpecification "TYPE"
        |>> ProblemComponent.SpecificationType
        <!> "TYPE"

    let specificationDimension : Parser<ProblemComponent, unit> =
        parseSpecification "DIMENSION" (pint32 .>> newline)
        |>> Dimension.apply
        |>> ProblemComponent.SpecificationDimension
        <!> "DIMENSION"

    let specificationCapacity : Parser<ProblemComponent, unit> =
        parseSpecification "CAPACITY"  (pint32 .>> newline)
        |>> CVRPCapacity.apply
        |>> ProblemComponent.SpecificationCVRPCapacity
        <!> "CAPACITY"

    let specificationEdgeWeightType : Parser<ProblemComponent, unit> =
        choice
            [
                wstr "EXPLICIT" >>. preturn EXPLICIT
                wstr "EUC_2D"   >>. preturn EUC_2D
                wstr "EUC_3D"   >>. preturn EUC_3D
                wstr "MAX_2D"   >>. preturn MAX_2D
                wstr "MAX_3D"   >>. preturn MAX_3D
                wstr "MAN_2D"   >>. preturn MAN_2D
                wstr "MAN_3D"   >>. preturn MAN_3D
                wstr "CEIL_2D"  >>. preturn CEIL_2D
                wstr "GEO"      >>. preturn GEO
                wstr "ATT"      >>. preturn ATT
                wstr "XRAY1"    >>. preturn XRAY1
                wstr "XRAY2"    >>. preturn XRAY2
                wstr "SPECIAL"  >>. preturn SPECIAL
            ]
        |> parseSpecification "EDGE_WEIGHT_TYPE"
        |>> ProblemComponent.SpecificationEdgeWeightType
        <!> "EDGE_WEIGHT_TYPE"

    let specificationEdgeWeightFormat : Parser<ProblemComponent, unit> =
        choice
            [
                wstr "FUNCTION"       >>. preturn FUNCTION
                wstr "FULL_MATRIX"    >>. preturn FULL_MATRIX
                wstr "UPPER_ROW"      >>. preturn UPPER_ROW
                wstr "LOWER_ROW"      >>. preturn LOWER_ROW
                wstr "UPPER_DIAG_ROW" >>. preturn UPPER_DIAG_ROW
                wstr "LOWER_DIAG_ROW" >>. preturn LOWER_DIAG_ROW
                wstr "UPPER_COL"      >>. preturn UPPER_COL
                wstr "LOWER_COL"      >>. preturn LOWER_COL
                wstr "UPPER_DIAG_COL" >>. preturn UPPER_DIAG_COL
                wstr "LOWER_DIAG_COL" >>. preturn LOWER_DIAG_COL
            ]
        |> parseSpecification "EDGE_WEIGHT_FORMAT"
        |>> ProblemComponent.SpecificationEdgeWeightFormat
        <!> "EDGE_WEIGHT_FORMAT"

    let specificationEdgeDataFormat : Parser<ProblemComponent, unit> =
        choice
            [
                wstr "EDGE_LIST" >>. preturn EDGE_LIST
                wstr "ADJ_LIST"  >>. preturn ADJ_LIST
            ]
        |> parseSpecification "EDGE_DATA_FORMAT"
        |>> ProblemComponent.SpecificationEdgeDataFormat
        <!> "EDGE_DATA_FORMAT"

    let specificationNodeCoordType : Parser<ProblemComponent, unit> =
        choice
            [
                wstr "TWOD_COORDS"   >>. preturn TWOD_COORDS
                wstr "THREED_COORDS" >>. preturn THREED_COORDS
                wstr "NO_COORDS"     >>. preturn NO_COORDS
            ]
        |> parseSpecification "NODE_COORD_TYPE"
        |>> ProblemComponent.SpecificationNodeCoordType
        <!> "NODE_COORD_TYPE"

    let specificationDataDisplayType : Parser<ProblemComponent, unit> =
        choice
            [
                wstr "COORD_DISPLAY" >>. preturn COORD_DISPLAY
                wstr "TWOD_DISPLAY"  >>. preturn TWOD_DISPLAY
                wstr "NO_DISPLAY"    >>. preturn NO_DISPLAY
            ]
        |> parseSpecification "DISPLAY_DATA_TYPE"
        |>> ProblemComponent.SpecificationDataDisplayType
        <!> "DISPLAY_DATA_TYPE"

    let NodeId =
        ws_x_nl pint32
        |>> NodeId.directApply
        <!> "NodeId"

    let Coordinate  =
        ws_x_nl pfloat <|> ws_x_nl (pint32 |>> float)
        |>> Coordinate.Coordinate
        <!> "Coordinate"

    let NodeCoord2D =
        NodeId .>>. Coordinate.>>. Coordinate .>> newline
        |>> NodeCoord2D.apply
        <!> "Coord2D"

    let NodeCoord3D =
        NodeId .>>. Coordinate.>>. Coordinate.>>. Coordinate .>> newline
        |>> NodeCoord3D.apply
        <!> "Coord3D"

    let EdgeLine =
        NodeId .>>. NodeId .>> newline
        |>> Edge.apply
        <!> "Edge"

    let Weight =
        ws_x_nl pint32
        |>> Weight
        <!> "Weight"

    let WeightLine =
        many1 Weight .>> newline
        |>> WeightLine
        <!> "WeightLine"

    let sectionNodeCoord : Parser<ProblemComponent, unit> =
        let Nodes2D = many NodeCoord2D |>> NodeCoordSection.Nodes2D
        let Nodes3D = many NodeCoord3D |>> NodeCoordSection.Nodes3D

        wstr "NODE_COORD_SECTION" >>. (Nodes2D <|> Nodes3D)
        |>> ProblemComponent.SectionNodeCoords
        <!> "NODE_COORD_SECTION"

    let sectionDepots : Parser<ProblemComponent, unit> =
        fail "Not Yet Implemented"

    let sectionDemands : Parser<ProblemComponent, unit> =
        fail "Not Yet Implemented"

    let sectionEdgeData : Parser<ProblemComponent, unit> =
        fail "Not Yet Implemented"

    let sectionFixedEdges : Parser<ProblemComponent, unit> =
        wstr "FIXED_EDGES_SECTION" >>. (many (attempt EdgeLine)) .>> wstr "-1"
        |>> FixedEdgesSection.FixedEdgeList
        |>> ProblemComponent.SectionFixedEdges
        <!> "FIXED_EDGES_SECTION"

    let sectionDisplayData : Parser<ProblemComponent, unit> =
        wstr "DISPLAY_DATA_SECTION" >>. (many NodeCoord2D |>> DisplayDataSection.Nodes2D)
        |>> ProblemComponent.SectionDisplayData
        <!> "DISPLAY_DATA_SECTION"

    let sectionTours : Parser<ProblemComponent, unit> =
        fail "Not Yet Implemented"

    let sectionEdgeWeight : Parser<ProblemComponent, unit> =
        wstr "EDGE_WEIGHT_SECTION" >>. (many (attempt WeightLine))
        |>> WeightLines
        |>> ProblemComponent.SectionEdgeWeights
        <!> "EDGE_WEIGHT_SECTION"

    let sectionEof: Parser<ProblemComponent, unit> =
        wstr "EOF" >>. preturn EOF
        |>> ProblemComponent.SectionEof
        <!> "EOF"

    let parseProblem =
        [
            specificationName
            specificationType
            specificationComment
            specificationDimension
            specificationCapacity
            specificationEdgeWeightType
            specificationEdgeWeightFormat
            specificationEdgeDataFormat
            specificationNodeCoordType
            specificationDataDisplayType
            sectionNodeCoord
            sectionDepots
            sectionDemands
            sectionEdgeData
            sectionFixedEdges
            sectionDisplayData
            sectionTours
            sectionEdgeWeight
            sectionEof
        ]
        |> Seq.map attempt
        |> choice
        |> many
        |>> ProblemComponent.coalesce
        <!> "Problem"

    let parseWeightedCompleteGraph =
        parseProblem
        |>> toWeightedCompleteGraph
        <!> "WeightedCompleteGraph"

let parseTextToWeightedCompleteGraph str =
    match run parseWeightedCompleteGraph str with
    | Success(result, _, _) -> result
    | Failure(err,    _, _) -> sprintf "Failure:%s[%s]" str err |> failwith

let test p str =
    match run p str with
    | Success(result, state, pos) ->
        printfn $"{{ Success : {result}; State : {state}; Position : {pos} }}"
    | Failure(errorMsg, parserError, state) ->
        printfn $"{{ Failure : {errorMsg}; State : {state}; ParserError : {parserError} }}"
