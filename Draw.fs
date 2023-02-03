module Draw

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing

open Geometry

let private Fill (color: Color) (context: IImageProcessingContext) =
    context.Fill(color)

let private DrawPoint (shape: IPath) radius (context: IImageProcessingContext) =
    context.Draw(Pens.Solid(Brushes.Solid(Color.Black), radius), shape)

let private DrawLine width p1 p2 (context: IImageProcessingContext) =
    context.DrawLines(Color.Black, width, p1, p2)

let private Mutate (image: Image) mutaters =
    image.Mutate <| mutaters

let private DrawPoints points context =
    List.map (fun (p:Point) -> new EllipsePolygon(p.F, 1F)) points
    |> List.iter (fun p -> DrawPoint p 3F context |> ignore)
    context

let private DrawLines lines context =
    List.map (fun (l:Edge) -> (l.p1.F, l.p2.F)) lines
    |> List.iter (fun (p1, p2) -> DrawLine 1F p1 p2 context |> ignore)
    context

let DrawNodalMap seed count points lines =
    let image = new Image<Rgba32>(2000, 2000)
    let mutators = fun x ->
        Fill Color.White x
        |> DrawPoints points
        |> DrawLines lines
        |> ignore
    Mutate image mutators
    System.IO.Directory.CreateDirectory("delaunay_results") |> ignore
    image.Save($"delaunay_results/delaunay_trianglation_{seed}_{count}.png")

