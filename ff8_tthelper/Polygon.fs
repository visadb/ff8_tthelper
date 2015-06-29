module PolygonModule

open System.Drawing

type private Orientation =
    Colinear | CW | CCW

    static member Of (p: Point) (q: Point) (r: Point) =
        let value = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
        if value < 0 then CCW
        else if value = 0 then Colinear
        else CW

    override this.ToString() =
        match this with Colinear -> "Colinear" | CW -> "CW" | CCW -> "CCW"

type Segment =
    val A: Point
    val B: Point
    val AIncl: bool
    val BIncl: bool
    new(a: Point, b: Point, aIncl: bool, bIncl: bool) =
        { A = a; B = b; AIncl = aIncl; BIncl = bIncl }
    new(a: Point, b: Point) =
        Segment(a, b, true, true)
    new((x1, y1), (x2, y2), aIncl: bool, bIncl: bool) =
        Segment(Point(x1,y1), Point(x2,y2), aIncl, bIncl)
    new((x1, y1), (x2, y2)) =
        Segment(Point(x1,y1), Point(x2,y2), true, true)

    member private this.ContainsColinearPoint (p: Point) pIncl =
        pIncl
     && (p <> this.A || this.AIncl)
     && (p <> this.B || this.BIncl)
     && p.X <= (max this.A.X this.B.X) && p.X >= (min this.A.X this.B.X)
     && p.Y <= (max this.A.Y this.B.Y) && p.Y >= (min this.A.Y this.B.Y)

    member this.Contains (p: Point) =
        (Orientation.Of this.A this.B p) = Colinear && this.ContainsColinearPoint p true

    member this.Intersects (other: Segment): bool =
        let o1 = Orientation.Of this.A this.B other.A
        let o2 = Orientation.Of this.A this.B other.B
        let o3 = Orientation.Of other.A other.B this.A
        let o4 = Orientation.Of other.A other.B this.B

        let basicTest = o1 <> Colinear && o2 <> Colinear && o3 <> Colinear && o4 <> Colinear
                     && o1 <> o2 && o3 <> o4

        basicTest
     || o1 = Colinear && (this.ContainsColinearPoint other.A other.AIncl)
     || o2 = Colinear && (this.ContainsColinearPoint other.B other.BIncl)
     || o3 = Colinear && (other.ContainsColinearPoint this.A this.AIncl)
     || o4 = Colinear && (other.ContainsColinearPoint this.B this.BIncl)

    member this.Translated by = Segment(this.A + by, this.B + by)

    override this.ToString(): string =
        // TODO: Reflect inclusiveness
        sprintf "Seg (%d,%d)->(%d,%d)" this.A.X this.A.Y this.B.X this.B.Y

    override this.Equals(otherObj: obj) =
        let other = otherObj :?> Segment
        this.A = other.A && this.B = other.B

    override this.GetHashCode() = hash this

// NOTE! linked segments must be consecutive!
type Polygon(segments0: Segment seq) =

    let segments =
        let dySign (s: Segment) = sign (s.B.Y - s.A.Y)
        let dySignIsOpposite s1 s2 = dySign s1 <> 0 && (dySign s1) = -(dySign s2)

        Seq.append segments0 [Seq.head segments0] |> Seq.pairwise 
            |> Seq.map (fun (s1, s2) ->
                if s1.B = s2.A && not (dySignIsOpposite s1 s2) then
                    Segment(s1.A, s1.B, true, false)
                else
                    s1
               )

    let (minX, minY, maxX, maxY) =
        segments |> Seq.fold (fun (minX,minY,maxX,maxY) s ->
            (min s.A.X minX, min s.A.Y minY, max s.A.X maxX, max s.A.Y maxY)
        ) (System.Int32.MaxValue,System.Int32.MaxValue,System.Int32.MinValue,System.Int32.MinValue)
    let inBoundingBox (p: Point) =
        p.X >= minX && p.X <= maxX && p.Y >= minY && p.Y <= maxY

    let containsRayCasting (p: Point) =
        let ray = Segment(p, Point(maxX + 50, p.Y), false, false)
        let onSegment = segments |> Seq.exists (fun s -> s.Contains p)
        let intersectCount = lazy (segments |> Seq.map ray.Intersects
                                            |> Seq.sumBy (fun i -> if i then 1 else 0))
        onSegment || intersectCount.Force() % 2 = 1

    new(vertices: Point seq) =
        let segments = Seq.append vertices [Seq.head vertices] |> Seq.pairwise |> Seq.map Segment
        Polygon(segments)

    new(vertices: (int*int) seq) =
        Polygon(vertices |> Seq.map Point)

    member this.BoundingBox = Rectangle(minX, minY, maxX - minX + 1, maxY - minY + 1)

    member this.Contains (p: Point): bool =
        if not (inBoundingBox p) then false
        else containsRayCasting p

    member this.Contains (p: int*int): bool =
        this.Contains(Point(fst p, snd p))

    override this.ToString(): string =
        sprintf "Polygon (%A)" segments

module SegmentTests =
    open FsUnit
    open NUnit.Framework

    [<TestFixture>]
    type ``Segment intersection test`` ()=

        [<Test>]
        member x.``Non isecting`` ()=                                 //   | |
            let s1 = Segment((1,0), (1,5))                            //   | |
            let s2 = Segment((2,0), (2,5))                            //   | |
            s1.Intersects s2 |> should be False                       //   | |

        [<Test>]
        member x.``Colinear separate`` ()=                            //   -----  -----
            let s1 = Segment((0,0), (3,0))                            //
            let s2 = Segment((4,0), (6,0))                            //
            s1.Intersects s2 |> should be False                       //

        [<Test>]
        member x.``Colinear overlapping`` ()=                         //   ----~~----
            let s1 = Segment((0,0), (4,0))                            //
            let s2 = Segment((3,0), (6,0))                            //
            s1.Intersects s2 |> should be True                        //

        [<Test>]
        member x.``Equal`` ()=                                        //   ~~~~~~
            let s1 = Segment((0,0), (5,0))                            //
            let s2 = Segment((0,0), (5,0))                            //
            s1.Intersects s2 |> should be True                        //

        [<Test>]
        member x.``isecting in the middle`` ()=                       //   \  /
            let s1 = Segment((0,0), (5,5))                            //    \/
            let s2 = Segment((0,5), (5,0))                            //    /\
            s1.Intersects s2 |> should be True                        //   /  \

        [<Test>]
        member x.``isecting in the first point`` ()=                  //    ______
            let s1 = Segment((0,0), (5,0))                            //   |
            let s2 = Segment((0,0), (0,5))                            //   |
            s1.Intersects s2 |> should be True                        //   |

        [<Test>]
        member x.``isecting in the second point`` ()=                 //   |   /
            let s1 = Segment((0,0), (0,5))                            //   |  /
            let s2 = Segment((5,0), (0,5))                            //   | /
            s1.Intersects s2 |> should be True                        //   |/

        [<Test>]
        member x.``touching other segment in the middle`` ()=         // _________
            let s1 = Segment((0,0), (5,0))                            //      |
            let s2 = Segment((3,0), (3,5))                            //      |
            s1.Intersects s2 |> should be True                        //      |

        [<Test>]
        member x.``isecting in the first point, fst exclusive`` ()=   //    ._____
            let s1 = Segment((0,0), (5,0), false, true)               //   |
            let s2 = Segment((0,0), (0,5), true, true)                //   |
            s1.Intersects s2 |> should be False                       //   |

        [<Test>]
        member x.``isecting in the first point, snd exclusive`` ()=   //    ______
            let s1 = Segment((0,0), (5,0), true, true)                //   :
            let s2 = Segment((0,0), (0,5), false, true)               //   |
            s1.Intersects s2 |> should be False                       //   |

        [<Test>]
        member x.``isecting in the first point, both exclusive`` ()=  //    ______
            let s1 = Segment((0,0), (5,0), false, true)               //   :
            let s2 = Segment((0,0), (0,5), false, true)               //   |
            s1.Intersects s2 |> should be False                       //   |

        [<Test>]
        member x.``isecting in the second point, fst exclusive`` ()=  //   |   /
            let s1 = Segment((0,0), (0,5), true, false)               //   |  /
            let s2 = Segment((5,0), (0,5), true, true)                //   | /
            s1.Intersects s2 |> should be False                       //   :/

        [<Test>]
        member x.``isecting in the second point, snd exclusive`` ()=  //   |   /
            let s1 = Segment((0,0), (0,5), true, true)                //   |  /
            let s2 = Segment((5,0), (0,5), true, false)               //   | /
            s1.Intersects s2 |> should be False                       //   |,

        [<Test>]
        member x.``touching in the middle, fst end exclusive`` ()=    // _________
            let s1 = Segment((0,0), (5,0), true, true)                //      :
            let s2 = Segment((3,0), (3,5), false, true)               //      |
            s1.Intersects s2 |> should be False                       //      |

        [<Test>]
        member x.``touching in the middle, snd end exclusive`` ()=    // _________
            let s1 = Segment((0,0), (5,0), true, true)                //      :
            let s2 = Segment((3,5), (3,0), true, false)               //      |
            s1.Intersects s2 |> should be False                       //      |

module PolygonTests =
    open FsUnit
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open System.Drawing

    type PointInPolygonConstraint(polygon: Polygon) =
        inherit Constraint()
        let mutable point: (int*int) = 0,0

        override this.Matches(actual: obj): bool =
            let givenPoint = actual :?> int*int
            let pointInPolygon = polygon.Contains(givenPoint)
            let actualSuffix = if pointInPolygon then "is in polygon" else "is not in polygon"
            point <- givenPoint 
            this.actual <- sprintf "point %A %s" givenPoint actualSuffix
            pointInPolygon
            
        override this.WriteDescriptionTo(writer: MessageWriter) =
            writer.Write(sprintf "point %A is in polygon" point)

    let InPolygon(polygon) = PointInPolygonConstraint(polygon)

    [<TestFixture>]
    type ``Polygon test`` ()=
        let simple = Polygon([20,30; 25,25; 30,50; 40,20; 50,15; 50,55; 30,60; 15,50])
        let square3by3 = Polygon([0,0; 2,0; 2,2; 0,2])
        let nr8 =
            let outline = [30,10;40,20;40,30;35,40;40,50;40,60;30,70;20,60;20,50;25,40;20,30;20,20;30,10]
                            |> Seq.pairwise |> Seq.map Segment
            let upperHole = [30,15;35,20;35,30;30,35;25,30;25,20;30,15] |> Seq.pairwise |> Seq.map Segment
            let lowerHole: Segment seq = upperHole |> Seq.map (fun s -> s.Translated(Size(0,30)))
            Polygon (Seq.concat [outline; upperHole; lowerHole])

        let shouldBeIn polygon p = p |> should be (InPolygon polygon)
        let shouldNotBeIn polygon p = p |> should not' (be (InPolygon polygon))

        let drawPolygon name (p: Polygon) =
            let bb = p.BoundingBox
            let bitmap = new Bitmap(bb.X + bb.Width+5, bb.Y + bb.Height + 5, Imaging.PixelFormat.Format24bppRgb)
            [ for y in 0..bitmap.Height-1 do
                for x in 0..bitmap.Width-1 do
                    if p.Contains(Point(x,y)) then yield (x,y,Color.White) ]
                |> List.iter bitmap.SetPixel
            bitmap.Save(@"D:\temp\"+name+".png", Imaging.ImageFormat.Png)
            

        [<Test>] member x.``in, 1 isect``()=                        (45,40) |> shouldBeIn    simple
        [<Test>] member x.``in, 3 isects``()=                       (22,40) |> shouldBeIn    simple
        [<Test>] member x.``in, isect through concave vertex``()=   (22,50) |> shouldBeIn    simple
        [<Test>] member x.``in, on concave vertex``()=              (30,50) |> shouldBeIn    simple
        [<Test>] member x.``out, outside bounding box``()=          ( 5,40) |> shouldNotBeIn simple
        [<Test>] member x.``out, inside bounding box, 0 isects``()= (40,58) |> shouldNotBeIn simple
        [<Test>] member x.``out, inside bounding box, 2 isects``()= (17,40) |> shouldNotBeIn simple
        [<Test>] member x.``out, inside concave indent``()=         (30,40) |> shouldNotBeIn simple

        [<Test>] member x.``in, in square``()=                       (1,1) |> shouldBeIn square3by3
        [<Test>] member x.``in, on square top left corner``()=       (0,0) |> shouldBeIn square3by3
        [<Test>] member x.``in, on square bottom right corner``()=   (2,2) |> shouldBeIn square3by3
        [<Test>] member x.``in, on square left edge``()=             (0,1) |> shouldBeIn square3by3
        [<Test>] member x.``in, on square right edge``()=            (2,1) |> shouldBeIn square3by3
        [<Test>] member x.``in, on square bottom edge``()=           (1,2) |> shouldBeIn square3by3

        [<Test>] member x.``in, in the bottom left leg of 8``()=    (22,55) |> shouldBeIn    nr8
        [<Test>] member x.``in, in the top right hand of 8``()=     (37,25) |> shouldBeIn    nr8
        [<Test>] member x.``in, ray exits through vertex``()=       (30,40) |> shouldBeIn    nr8
        [<Test>] member x.``in, on leftmost segment``()=            (20,25) |> shouldBeIn    nr8
        [<Test>] member x.``in, on rightmost segment``()=           (40,25) |> shouldBeIn    nr8
        [<Test>] member x.``out, in the left crack of 8``()=        (21,33) |> shouldNotBeIn nr8
        [<Test>] member x.``out, in the top hole of 8``()=          (30,25) |> shouldNotBeIn nr8

        [<Test>]
        member x.``draw bitmaps``()=
            drawPolygon "simple" simple
            drawPolygon "square3by3" square3by3
            drawPolygon "nr8" nr8
