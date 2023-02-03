open Draw
open Geometry
open Utility

let RemoveSuperTriangles p1 p2 p3 (ts:Triangle list) =
    let helper pt = pt = p1 || pt = p2 || pt = p3
    List.where(fun (t:Triangle) -> (helper t.p1 || helper t.p2 || helper t.p3) = false) ts

let inCC p (t:Triangle) = t.InCircumcircle p
let edges (t:Triangle) = t.edges

let rec Calculate (ts:Triangle list) (ps:Point list) =
    match ps with
    | [] -> ts
    | p::pr ->
        let badTs, validTs = List.partition (inCC p) ts
        let newTs =
            badTs
            |> List.map(edges)
            |> List.concat
            |> Unique
            |> List.map(EdgePointToTriangle p)
        Calculate (validTs@newTs) pr

let Bounds (random:Random) = random.Next 100 1900

let GetPoints random count =
    List.map(fun _ -> (Bounds random, Bounds random)) [1..count]
    |> List.map(fun (x,y) -> (float32 x, float32 y))
    |> List.map(fun (x, y) -> Point (x, y))

let GetTriangles random count =
    List.map(fun _ -> Triangle.Create (GetPoints random 3)) [1..count]

let GetArgs (args:string array) =
    (int args[0]), (int args[1])

[<EntryPoint>]
let Main args =
    let seed, count = GetArgs args
    let p1, p2, p3 = Point(-1000F, 0F), Point(3000F, 0F), Point(1000F, 4000F)
    Calculate [Triangle.Create([p1;p2;p3])] (GetPoints (Random seed) count)
    |> RemoveSuperTriangles p1 p2 p3
    |> TriToPointsEdges
    // ||> PrintPointsEdges
    ||> DrawNodalMap seed count
    0


