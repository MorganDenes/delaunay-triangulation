module Utility

open Geometry

let Distinct (x:'a list) =
    List.distinct x

let Unique x =
    List.groupBy (fun x -> x) x
    |> List.where (fun (_, v) -> v.Length = 1)
    |> List.map(fun (k, _) -> k)

type Random(seed:int) =
    let r = System.Random(seed)
    member this.Next min max = r.Next(min, max)

let PrintPoints points =
    List.iter(fun (p:Point) -> printfn $"{p.x}:{p.y}") points
    points

let PrintEdges edges =
    let points (p:Point) = $"{p.x}:{p.y}"
    List.iter(fun (e:Edge) -> printfn $"{points e.p1} -> {points e.p2}") edges
    edges

let PrintPointsEdges points edges =
    PrintPoints points, PrintEdges edges

let TriToPointsEdges (tri:Triangle list) =
    let points = List.map(fun (t:Triangle) -> t.points) tri |> List.concat |> Distinct
    let edges = List.map(fun (t:Triangle) -> t.edges) tri |> List.concat |> Distinct
    points, edges

