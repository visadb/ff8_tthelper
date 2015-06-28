module PolygonModule

open FsUnit
open NUnit.Framework
open System.Drawing

type private Orientation =
    Colinear | CW | CCW

    static member Of (p: Point) (q: Point) (r: Point) =
        let value = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
        if value < 0 then CCW
        else if value = 0 then Colinear
        else CW
        
type Segment =
    val A: Point
    val B: Point
    new(a: Point, b: Point) = { A = a; B = b }
    new(x1, y1, x2, y2) = { A = Point(x1,y1); B = Point(x2,y2)}
    new((x1, y1), (x2, y2)) = { A = Point(x1,y1); B = Point(x2,y2)}

    member private this.ContainsColinearPoint (p: Point) =
        p.X <= (max this.A.X this.B.X) && p.X >= (min this.A.X this.B.X)
     && p.Y <= (max this.A.Y this.B.Y) && p.Y >= (min this.A.Y this.B.Y)

    member this.Intersects (other: Segment): bool =
        let o1 = Orientation.Of this.A this.B other.A
        let o2 = Orientation.Of this.A this.B other.B
        let o3 = Orientation.Of other.A other.B this.A
        let o4 = Orientation.Of other.A other.B this.B

        o1 <> o2 && o3 <> o4
     || o1 = Colinear && (this.ContainsColinearPoint other.A)
     || o2 = Colinear && (this.ContainsColinearPoint other.B)
     || o3 = Colinear && (other.ContainsColinearPoint this.A)
     || o4 = Colinear && (other.ContainsColinearPoint this.B)

type Polygon(vertices: Point list) =

    member this.Contains (point: Point): bool =
        true // TODO


[<TestFixture>]
type ``Segment intersection test`` ()=
    
    [<Test>]
    member x.``Non intersecting segments`` ()=                    //   | |
        let s1 = Segment((1,0), (1,5))                            //   | |
        let s2 = Segment((2,0), (2,5))                            //   | |
        s1.Intersects s2 |> should be False                       //   | |

    [<Test>]
    member x.``Colinear separate segments`` ()=                   //   
        let s1 = Segment((0,0), (3,0))                            //   
        let s2 = Segment((4,0), (6,0))                            //   
        s1.Intersects s2 |> should be False                       //   -----  -----

    [<Test>]
    member x.``Colinear overlapping segments`` ()=                //   
        let s1 = Segment((0,0), (4,0))                            //   
        let s2 = Segment((3,0), (6,0))                            //   
        s1.Intersects s2 |> should be True                        //   ----~~----

    [<Test>]
    member x.``Equal segments`` ()=                               //
        let s1 = Segment((0,0), (5,0))                            //
        let s2 = Segment((0,0), (5,0))                            //
        s1.Intersects s2 |> should be True                        //   ~~~~~~

    [<Test>]
    member x.``Segments intersecting in the middle`` ()=          //   \  /
        let s1 = Segment((0,0), (5,5))                            //    \/
        let s2 = Segment((0,5), (5,0))                            //    /\
        s1.Intersects s2 |> should be True                        //   /  \

    [<Test>]
    member x.``Segments intersecting in the first point`` ()=     //   |
        let s1 = Segment((0,0), (5,0))                            //   |
        let s2 = Segment((0,0), (0,5))                            //   |
        s1.Intersects s2 |> should be True                        //   |______

    [<Test>]
    member x.``Segments intersecting in the second point`` ()=    //   |\
        let s1 = Segment((0,0), (0,5))                            //   | \
        let s2 = Segment((5,0), (0,5))                            //   |  \
        s1.Intersects s2 |> should be True                        //   |   \

    [<Test>]
    member x.``Segment touching other segment in the middle`` ()= //      |
        let s1 = Segment((0,0), (5,0))                            //      |
        let s2 = Segment((3,0), (3,5))                            //      |
        s1.Intersects s2 |> should be True                        // _____|___