module Polygon

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
        sprintf "Seg %s%d,%d->%d,%d%s" (if this.AIncl then "[" else "]") 
                                       this.A.X this.A.Y this.B.X this.B.Y
                                       (if this.BIncl then "]" else "[") 

    override this.Equals(otherObj: obj) =
        let other = otherObj :?> Segment
        this.A = other.A && this.B = other.B

    override this.GetHashCode() = hash this

exception private Unreachable

// NOTE! linked segments must be consecutive and must not
//       contain 2 consecutive horizontal segments
type Polygon(segments: Segment seq) =

    let connectedComponents =
        let dySign (s: Segment) = sign (s.B.Y - s.A.Y)
        segments |> Seq.fold (fun (complete, current, currentFirstVertex: Point option) s ->
            if currentFirstVertex.IsNone then
                (complete, Seq.singleton s, Some(s.A))
            else if currentFirstVertex.Value = s.B then
                (Seq.append complete [Seq.append current [s]], Seq.empty, Option.None)
            else if dySign s = 0 && dySign (Seq.last current) = 0 then
                raise <| System.ArgumentException(
                    sprintf "consecutive horizontal segments: %A %A" (Seq.last current) s)
            else if s.A <> (Seq.last current).B then
                raise <| System.ArgumentException(
                    sprintf "consecutive segments not connected: %A %A" (Seq.last current) s)
            else
                (complete, Seq.append current [s], currentFirstVertex)
        ) (Seq.empty, Seq.empty, Option.None)
            |> (fun (components, currentComponent, _) -> 
                    if Seq.isEmpty components then
                        raise <| System.ArgumentException("no connected components")
                    if not (Seq.isEmpty currentComponent) then
                        raise <| System.ArgumentException(sprintf "orphan segments: %A" currentComponent)
                    components |> List.ofSeq
                )

    let containmentTestSegments =
      lazy
        let dySign (s: Segment) = sign (s.B.Y - s.A.Y)
        let dySignIsOpposite s1 s2 = dySign s1 <> 0 && (dySign s1) = -(dySign s2)
        let isPotentialRayExitOrEntryVertex (s1: Segment) (s2: Segment) =
            s1.B = s2.A && not (dySignIsOpposite s1 s2)

        connectedComponents |> Seq.collect (fun connectedComponent ->
            Seq.append connectedComponent [Seq.head connectedComponent; Seq.nth 1 connectedComponent]
                |> Seq.windowed 3
                |> Seq.map (fun segmentTriplet ->
                    let s1,s2,s3 = match segmentTriplet with
                                    | [|s1;s2;s3|] -> s1,s2,s3
                                    | _ -> raise Unreachable
                    if dySign s2 = 0 && not (dySignIsOpposite s1 s3) then
                        s1
                    else if not (dySignIsOpposite s1 s2) then
                        Segment(s1.A, s1.B, true, false)
                    else
                        s1
                   )) |> List.ofSeq

    let (minX, minY, maxX, maxY) =
        segments |> Seq.fold (fun (minX,minY,maxX,maxY) s ->
            (min s.A.X minX, min s.A.Y minY, max s.A.X maxX, max s.A.Y maxY)
        ) (System.Int32.MaxValue,System.Int32.MaxValue,System.Int32.MinValue,System.Int32.MinValue)
    let inBoundingBox (p: Point) =
        p.X >= minX && p.X <= maxX && p.Y >= minY && p.Y <= maxY

    let containsRayCasting (p: Point) =
        let ray = Segment(p, Point(maxX + 50, p.Y), false, false)
        let onSegment = segments |> Seq.exists (fun s -> s.Contains p)
        let intersectCount = lazy (containmentTestSegments.Force() |> Seq.map ray.Intersects
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

    member this.Contains ((x,y): int*int): bool =
        this.Contains(Point(x, y))

    member this.Contains(x: int, y: int): bool =
        this.Contains(Point(x, y))

    member this.Translated (by: Size): Polygon =
        Polygon(segments |> Seq.map (fun s -> s.Translated by))

    member this.Segments = segments

    member this.Merge (other: Polygon): Polygon =
        Polygon(Seq.concat [other.Segments; this.Segments])

    override this.ToString(): string =
        sprintf "Polygon (%A)" segments
            
module SegmentTests =
    open FsUnit
    open NUnit.Framework

    [<TestFixture>]
    type ``Segment tests`` ()=

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

        [<Test>]
        member x.``does not contain point`` ()=                       // _________
            let s1 = Segment((0,0), (5,0), true, true)                //      .
            s1.Contains (Point(3,1)) |> should be False               //

        [<Test>]
        member x.``point in middle`` ()=                              // _____.___
            let s1 = Segment((0,0), (5,0), true, true)                //
            s1.Contains (Point(3,0)) |> should be True                //

        [<Test>]
        member x.``point in exclusive end`` ()=                       // ________.
            let s1 = Segment((0,0), (5,0), true, false)               //      
            s1.Contains (Point(5,0)) |> should be False               //

        [<Test>]
        member x.``point in inclusive end`` ()=                       // ________.
            let s1 = Segment((0,0), (5,0), true, true)                //      
            s1.Contains (Point(5,0)) |> should be True                //

        [<Test>]
        member x.``toString inclusive-exclusive`` ()=
            Segment((1,2), (3,4), true, false).ToString() |> should equal "Seg [1,2->3,4["

        [<Test>]
        member x.``toString exclusive-inclusive`` ()=
            Segment((1,2), (3,4), false, true).ToString() |> should equal "Seg ]1,2->3,4]"

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

    let throwWithMessage (m:string) (t:System.Type) = Throws.TypeOf(t).And.Message.EqualTo(m)

    let InPolygon(polygon) = PointInPolygonConstraint(polygon)

    let drawPolygon(p: Polygon, filePath: string, drawArea0: Rectangle option) =
        let drawArea = defaultArg drawArea0 (Rectangle(p.BoundingBox.Location - Size(5,5),
                                                       p.BoundingBox.Size + Size(10,10)))
        let bitmap = new Bitmap(drawArea.Width, drawArea.Height, Imaging.PixelFormat.Format24bppRgb)
        [ for y in drawArea.Y..drawArea.Y+drawArea.Height-1 do
            for x in drawArea.X..drawArea.X+drawArea.Width-1 do
                if p.Contains(Point(x,y)) then yield (x-drawArea.X, y-drawArea.Y, Color.White) ]
            |> List.iter bitmap.SetPixel
        bitmap.Save(filePath, Imaging.ImageFormat.Png)
        bitmap.Dispose()


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
        let horizSegs = Polygon([0,30; 10,20; 20,20; 25,10; 40,10; 40,30;
                                     35,30; 35,15; 30,15; 25,25; 15,25; 10,30])

        let shouldBeIn polygon p = p |> should be (InPolygon polygon)
        let shouldNotBeIn polygon p = p |> should not' (be (InPolygon polygon))

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

        [<Test>] member x.``out, left of non-edge h-seg``()=            ( 5,20) |> shouldNotBeIn horizSegs
        [<Test>] member x.``out, left of edge h-seg``()=                ( 5,10) |> shouldNotBeIn horizSegs
        [<Test>] member x.``out, left of 2 bottom edge h-segs``()=      (-5,30) |> shouldNotBeIn horizSegs
        [<Test>] member x.``out, left of bottom-side edge h-seg``()=    ( 0,15) |> shouldNotBeIn horizSegs
        [<Test>] member x.``out, left of bottom-side non-edge h-seg``()=( 0,25) |> shouldNotBeIn horizSegs

        [<Test>]
        [<Ignore("For debugging only")>]
        member x.``draw bitmaps``()=
            drawPolygon(simple, @"D:\temp\simple.png", None)
            drawPolygon(square3by3, @"D:\temp\square3by3.png", None)
            drawPolygon(nr8, @"D:\temp\nr8.png", None)
            drawPolygon(horizSegs, @"D:\temp\withHorizSegment.png", None)

        [<Test>]
        member x.``should notice consecutive non-connected segments`` ()=
            (fun () -> Polygon([Segment((0,0),(5,0)); Segment((6,0),(6,10)); Segment((6,10),(0,0))]) |> ignore)
                |> should (throwWithMessage "consecutive segments not connected: Seg [0,0->5,0] Seg [6,0->6,10]") typeof<System.ArgumentException>

        [<Test>]
        member x.``should notice consecutive horizontal segments`` ()=
            (fun () -> Polygon([Segment((0,0),(3,0)); Segment((3,0),(5,0)); Segment((5,0),(5,10)); Segment((6,10),(0,0))]) |> ignore)
                |> should (throwWithMessage "consecutive horizontal segments: Seg [0,0->3,0] Seg [3,0->5,0]") typeof<System.ArgumentException>

        [<Test>]
        member x.``should notice orphan segments`` ()=
            (fun () -> Polygon([Segment((0,0),(5,0)); Segment((5,0),(5,10)); Segment((5,10),(0,0)); Segment((10,10),(15,15))]) |> ignore)
                |> should (throwWithMessage "orphan segments: seq [Seg [10,10->15,15]]") typeof<System.ArgumentException>

        [<Test>]
        member x.``should notice lack of connected components`` ()=
            (fun () -> Polygon([Segment((0,0),(5,0)); Segment((5,0),(5,10))]) |> ignore)
                |> should (throwWithMessage "no connected components") typeof<System.ArgumentException>
