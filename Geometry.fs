module Geometry

open SixLabors.ImageSharp

type Point(x:float32, y:float32) =
    member this.x = x
    member this.y = y
    member this.F = PointF(x, y)

let Distance (p1:Point) (p2:Point) =
    ((p1.x - p2.x) ** 2F + (p1.y - p2.y) ** 2F) ** 0.5F

type Edge(P1:Point,P2:Point) =
    member this.p1 = P1
    member this.p2 = P2
    member this.points = [P1;P2]
    override this.GetHashCode() = 0
    override this.Equals other =
        let a = other :?> Edge
        a.p1 = this.p1 && a.p2 = this.p2 || a.p1 = this.p2 && a.p2 = this.p1

type Triangle(p1:Point,p2:Point,p3:Point) =
    let R =
        let a, b, c = Distance p1 p2, Distance p2 p3, Distance p3 p1
        let s = 0.5F * (a + b + c)
        (a * b * c) / (4F * ((s * (s - a) * (s - b) * (s - c)) ** 0.5f))
    let Circumcenter =
        let helper (a:Point) (b:float32) (c:float32) = (a.x ** 2F + a.y ** 2F) * (b - c)
        let D = 2F * ( p1.x * (p2.y - p3.y) + p2.x * (p3.y - p1.y) + p3.x * (p1.y - p2.y)  )
        let Ux = ((helper p1 p2.y p3.y) + (helper p2 p3.y p1.y) + (helper p3 p1.y p2.y)) / D
        let Uy = ((helper p1 p3.x p2.x) + (helper p2 p1.x p3.x) + (helper p3 p2.x p1.x)) / D
        Point(Ux, Uy)
    member this.p1 = p1
    member this.p2 = p2
    member this.p3 = p3
    member this.points = [p1;p2;p3]
    member this.edges = [Edge(p1,p2);Edge(p2,p3);Edge(p3,p1)]
    member this.InCircumcircle p =
        Distance Circumcenter p <= R
    static member Create(ps:Point list) = 
        if ps.Length <> 3 then failwith "Needs to only be three points"
        else Triangle(ps[0],ps[1],ps[2])


let EdgePointToTriangle point (edge:Edge) =
    Triangle(edge.p1,edge.p2,point)

