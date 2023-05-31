let {describe, it, assertEq, assertEqMsg, assertEqNumMsg} = module(Expln_test)
open Expln_2D.Std2D

type pnt = {x: float, y: float}
let mkp = p => ex->vecMul(p.x)->vecAdd(ey->vecMul(p.y))->vecEnd
type vec = {begin:pnt, end:pnt}
let mkv = (v:vec) => pntVec(v.begin->mkp, v.end->mkp)

let precision = 0.000001

let assertEqNum = (a, b, msg) => assertEqNumMsg(a, b, precision, msg)

let assertEqPnt = (p1:point,p2:point,msg:string) => {
    assertEqNum(p1->pntX, p2->pntX, "pntX / " ++ msg)
    assertEqNum(p1->pntY, p2->pntY, "pntY / " ++ msg)
}

let assertEqVec = (v1:vector, v2:vector, msg:string) => {
    assertEqPnt(v1->vecBegin, v2->vecBegin, "vecBegin / " ++ msg)
    assertEqPnt(v1->vecEnd, v2->vecEnd, "vecEnd / " ++ msg)
}

describe("utility test functions", _ => {
    it("work", _ => {
        let p = {x:39.62, y:71.03}->mkp
        assertEqNum(p->pntX, 39.62, "p->pntX")
        assertEqNum(p->pntY, 71.03, "p->pntY")
        let v = {begin:{x:-7., y: 11.}, end:{x:23., y: -1.}}->mkv
        assertEqNum(v->vecBegin->pntX, -7., "v->vecBegin->pntX")
        assertEqNum(v->vecBegin->pntY, 11., "v->vecBegin->pntY")
        assertEqNum(v->vecEnd->pntX, 23., "v->vecEnd->pntX")
        assertEqNum(v->vecEnd->pntY, -1., "v->vecEnd->pntY")
    })
})

describe("Expln_2d", _ => {
    it("test all", _ => {
        assertEqVec(ex->vecRot((Js.Math._PI /. 2.)->rad), ey, "vecRot")

        assertEqNum(deg(45.) -> toRad, 0.785398, "toRad")
        assertEqNum(rad(2.14675) -> toDeg, 122.9997147, "toDeg")
        
        //let pntLen: point => float
        assertEqNum({x:3., y:4.}->mkp->pntLen, 5., "pntLen")

        //let pntSub: (point,point) => point
        assertEqPnt({x:1., y:7.}->mkp->pntSub({x:6.,y:-2.}->mkp), {x:-5., y:9.}->mkp, "pntSub")

        //let pntAdd: (point,point) => point
        assertEqPnt({x:1., y:7.}->mkp->pntAdd({x:6.,y:-2.}->mkp), {x:7., y:5.}->mkp, "pntAdd")

        //let pntTr: (point, vector) => point
        assertEqPnt(
            {x:1., y:7.}->mkp->pntTr({begin:{x:-3., y:7.}, end:{x:4., y:-1.}}->mkv),
            {x:8., y:-1.}->mkp,
            "pntTr"
        )

        //let pntTrDir: (point, vector, float) => point
        assertEqPnt(
            {x:3., y:2.}->mkp -> pntTrDir({begin:{x:100., y:-50.}, end:{x:104., y:-53.}}->mkv, 5.),
            {x:7., y:-1.}->mkp,
            "pntTrDir"
        )

        //let pntMul: (point, float) => point
        assertEqPnt({x:-6., y:9.}->mkp -> pntMul(2.), {x:-12., y:18.}->mkp, "pntMul")

        //let pntDiv: (point, float) => point
        assertEqPnt({x:-12., y:18.}->mkp -> pntDiv(2.), {x:-6., y:9.}->mkp, "pntDiv")

        //let pntVec: (point,point) => vector
        assertEqVec(
            {x:7., y:10.}->mkp -> pntVec({x:-7., y:100.}->mkp),
            {begin:{x:7., y:10.}, end:{x:-7., y:100.}}->mkv,
            "pntVec"
        )

        //let pntRot: (point, angle) => point
        assertEqPnt(
            {x:Js.Math.sqrt(3.) /. 2., y: 0.5}->mkp -> pntRot(deg(-150.)),
            {x:-0.5, y:-.Js.Math.sqrt(3.) /. 2.}->mkp,
            "pntRot"
        )
        
       let testVec = {begin:{x:3., y:4.}, end:{x:6., y:8.}}->mkv

        //let vecLen: vector => float
        assertEq(testVec -> vecLen, 5.)

        //let vecRev: vector => vector
        assertEqVec(testVec->vecRev, {begin:{x:3., y:4.}, end:{x:0., y:0.}}->mkv, "vecRev")

        //let vecMul: (vector, float) => vector
        assertEqVec(testVec->vecMul(3.), {begin:{x:3., y:4.}, end:{x:12., y:16.}}->mkv, "vecMul")

        //let vecMulVec: (vector, vector) => float
        assertEqNum(testVec->vecRot(deg(60.))->vecMulVec(testVec), 12.5, "vecMulVec")

        //let vecDiv: (vector, float) => vector
        assertEqVec(testVec->vecDiv(2.), {begin:{x:3., y:4.}, end:{x:4.5, y:6.}}->mkv, "vecDiv")

        //let vecAdd: (vector, vector) => vector
        assertEqVec(
            testVec->vecAdd({begin:{x:3., y:4.}, end:{x:6., y:8.}}->mkv),
            {begin:{x:3., y:4.}, end:{x:9., y:12.}}->mkv,
            "vecAdd"
        )

        //let vecRot: (vector, angle) => vector
        assertEqVec(testVec->vecRot(deg(-90.)), {begin:{x:3., y:4.}, end:{x:7., y:1.}}->mkv, "vecRot")

        //let vecNorm: vector => vector
        assertEqVec(testVec->vecNorm, {begin:{x:3., y:4.}, end:{x: 3. +. 3. /. 5., y:4. +. 4. /. 5.}}->mkv, "vecNorm")

        //let vecSwapEnds: vector => vector
        assertEqVec(testVec->vecSwapEnds, {begin:{x:6., y:8.}, end:{x:3., y:4.}}->mkv, "vecSwapEnds")

        //let vecBeginAt: (vector, point) => vector
        assertEqVec(
            testVec->vecBeginAt({x:100., y: -30.}->mkp),
            {begin:{x:100., y: -30.}, end:{x:103., y:-26.}}->mkv,
            "vecBeginAt"
        )

        //let vecEndAt: (vector, point) => vector
        assertEqVec(
            testVec->vecEndAt({x:100., y: -30.}->mkp),
            {begin:{x:97., y: -34.}, end:{x:100., y: -30.}}->mkv,
            "vecEndAt"
        )

        //let vecTr: (vector, vector) => vector
        assertEqVec(
            testVec->vecTr({begin:{x:-7., y: 11.}, end:{x:23., y: -1.}}->mkv),
            {begin:{x:33., y:-8.}, end:{x:36., y:-4.}}->mkv,
            "vecTr"
        )

        //let vecTrDir: (vector, vector, float) => vector
        assertEqVec(testVec->vecTrDir(testVec, 5.), {begin:{x:6., y:8.}, end:{x:9., y:12.}}->mkv, "vecTrDir")
    })
})

describe("boundaries", _ => {
    it("works", _ => {
        //let bndFromPoints: array<point> => boundaries
        let b = bndFromPoints([mkp({x:4., y:11.})])
        assertEqNum(b->bndMinX, 4., "bndFromPoints1.MinX")
        assertEqNum(b->bndMaxX, 4., "bndFromPoints1.MaxX")
        assertEqNum(b->bndMinY, 11., "bndFromPoints1.MinY")
        assertEqNum(b->bndMaxY, 11., "bndFromPoints1.MaxY")

        let b = bndFromPoints([mkp({x:4., y:11.}), mkp({x:-14., y:-110.})])
        assertEqNum(b->bndMinX, -14., "bndFromPoints2.MinX")
        assertEqNum(b->bndMaxX, 4., "bndFromPoints2.MaxX")
        assertEqNum(b->bndMinY, -110., "bndFromPoints2.MinY")
        assertEqNum(b->bndMaxY, 11., "bndFromPoints2.MaxY")

        //bndFromVectors
        let b = bndFromVectors([ex])
        assertEqNum(b->bndMinX, 0., "bndFromVectors.b1.MinX")
        assertEqNum(b->bndMaxX, 1., "bndFromVectors.b1.MaxX")
        assertEqNum(b->bndMinY, 0., "bndFromVectors.b1.MinY")
        assertEqNum(b->bndMaxY, 0., "bndFromVectors.b1.MaxY")
        let b = bndFromVectors([ex,ey])
        assertEqNum(b->bndMinX, 0., "bndFromVectors.b2.MinX")
        assertEqNum(b->bndMaxX, 1., "bndFromVectors.b2.MaxX")
        assertEqNum(b->bndMinY, 0., "bndFromVectors.b2.MinY")
        assertEqNum(b->bndMaxY, 1., "bndFromVectors.b2.MaxY")
        let b = bndFromVectors([ex->vecBeginAt(mkp({x:1., y:1.}))->vecMul(3.),ey->vecBeginAt(mkp({x:1., y:1.}))->vecMul(3.)])
        assertEqNum(b->bndMinX, 1., "bndFromVectors.b3.MinX")
        assertEqNum(b->bndMaxX, 4., "bndFromVectors.b3.MaxX")
        assertEqNum(b->bndMinY, 1., "bndFromVectors.b3.MinY")
        assertEqNum(b->bndMaxY, 4., "bndFromVectors.b3.MaxY")

        //boundaries corners
        let b = bndFromPoints([mkp({x:5., y:11.}), mkp({x:100., y:80.})])
        assertEqPnt(b->bndLeftBottom, mkp({x:5., y:11.}), "bndLeftBottom")
        assertEqPnt(b->bndLeftTop, mkp({x:5., y:80.}), "bndLeftTop")
        assertEqPnt(b->bndRightBottom, mkp({x:100., y:11.}), "bndRightBottom")
        assertEqPnt(b->bndRightTop, mkp({x:100., y:80.}), "bndRightTop")

        //bndAddMarginPct
        let b = bndFromPoints([mkp({x:0., y:0.}), mkp({x:100., y:80.})])->bndAddMarginPct(~all=0.1, ())
        assertEqNum(b->bndMinX, -10., "bndAddMarginPct.all.MinX")
        assertEqNum(b->bndMaxX, 110., "bndAddMarginPct.all.MaxX")
        assertEqNum(b->bndMinY, -10., "bndAddMarginPct.all.MinY")
        assertEqNum(b->bndMaxY, 90., "bndAddMarginPct.all.MaxY")
        let b = bndFromPoints([mkp({x:0., y:0.}), mkp({x:100., y:200.})])
            ->bndAddMarginPct(~left=0.1, ~right=0.2, ~top=0.3, ~bottom=0.4, ())
        assertEqNum(b->bndMinX, -10., "bndAddMarginPct.notAll.MinX")
        assertEqNum(b->bndMaxX, 120., "bndAddMarginPct.notAll.MaxX")
        assertEqNum(b->bndMinY, -80., "bndAddMarginPct.notAll.MinY")
        assertEqNum(b->bndMaxY, 260., "bndAddMarginPct.notAll.MaxY")

        //bndAddMargin
        let b = bndFromPoints([mkp({x:0., y:0.}), mkp({x:100., y:80.})])->bndAddMargin(~all=5., ())
        assertEqNum(b->bndMinX, -5., "bndAddMargin.all.MinX")
        assertEqNum(b->bndMaxX, 105., "bndAddMargin.all.MaxX")
        assertEqNum(b->bndMinY, -5., "bndAddMargin.all.MinY")
        assertEqNum(b->bndMaxY, 85., "bndAddMargin.all.MaxY")
        let b = bndFromPoints([mkp({x:0., y:0.}), mkp({x:100., y:200.})])
            ->bndAddMargin(~left=1., ~right=2., ~top=3., ~bottom=4., ())
        assertEqNum(b->bndMinX, -1., "bndAddMargin.notAll.MinX")
        assertEqNum(b->bndMaxX, 102., "bndAddMargin.notAll.MaxX")
        assertEqNum(b->bndMinY, -4., "bndAddMargin.notAll.MinY")
        assertEqNum(b->bndMaxY, 203., "bndAddMargin.notAll.MaxY")

        //let bndAddPoint: (boundaries,point) => boundaries
        let b = bndFromPoints([mkp({x:4., y:11.}), mkp({x:-14., y:-110.})])
        assertEqNum(b->bndMinX, -14., "bndAddPoint.bndFromPoints.MinX")
        assertEqNum(b->bndMaxX, 4., "bndAddPoint.bndFromPoints.MaxX")
        assertEqNum(b->bndMinY, -110., "bndAddPoint.bndFromPoints.MinY")
        assertEqNum(b->bndMaxY, 11., "bndAddPoint.bndFromPoints.MaxY")
        let b = b->bndAddPoint(mkp({x:40., y:30.}))
        assertEqNum(b->bndMinX, -14., "bndAddPoint.MinX")
        assertEqNum(b->bndMaxX, 40., "bndAddPoint.MaxX")
        assertEqNum(b->bndMinY, -110., "bndAddPoint.MinY")
        assertEqNum(b->bndMaxY, 30., "bndAddPoint.MaxY")

        //let bndAddPoints: (boundaries,array<point>) => boundaries
        let b = bndFromPoints([mkp({x:4., y:11.}), mkp({x:-14., y:-110.})])
        assertEqNum(b->bndMinX, -14., "bndAddPoints.bndFromPoints.MinX")
        assertEqNum(b->bndMaxX, 4., "bndAddPoints.bndFromPoints.MaxX")
        assertEqNum(b->bndMinY, -110., "bndAddPoints.bndFromPoints.MinY")
        assertEqNum(b->bndMaxY, 11., "bndAddPoints.bndFromPoints.MaxY")
        let b = b->bndAddPoints([mkp({x:-21., y:-203.}), mkp({x:59., y:72.})])
        assertEqNum(b->bndMinX, -21., "bndAddPoints.MinX")
        assertEqNum(b->bndMaxX, 59., "bndAddPoints.MaxX")
        assertEqNum(b->bndMinY, -203., "bndAddPoints.MinY")
        assertEqNum(b->bndMaxY, 72., "bndAddPoints.MaxY")

        //let bndMerge: (boundaries,boundaries) => boundaries
        let b1 = bndFromPoints([mkp({x:-1., y:-4.})])
        assertEqNum(b1->bndMinX, -1., "bndMerge.b1.bndFromPoints.MinX")
        assertEqNum(b1->bndMaxX, -1., "bndMerge.b1.bndFromPoints.MaxX")
        assertEqNum(b1->bndMinY, -4., "bndMerge.b1.bndFromPoints.MinY")
        assertEqNum(b1->bndMaxY, -4., "bndMerge.b1.bndFromPoints.MaxY")
        let b2 = bndFromPoints([mkp({x:3., y:-7.})])
        assertEqNum(b2->bndMinX, 3., "bndMerge.b2.bndFromPoints.MinX")
        assertEqNum(b2->bndMaxX, 3., "bndMerge.b2.bndFromPoints.MaxX")
        assertEqNum(b2->bndMinY, -7., "bndMerge.b2.bndFromPoints.MinY")
        assertEqNum(b2->bndMaxY, -7., "bndMerge.b2.bndFromPoints.MaxY")
        let b = b1->bndMerge(b2)
        assertEqNum(b->bndMinX, -1., "bndMerge.MinX")
        assertEqNum(b->bndMaxX, 3., "bndMerge.MaxX")
        assertEqNum(b->bndMinY, -7., "bndMerge.MinY")
        assertEqNum(b->bndMaxY, -4., "bndMerge.MaxY")

        //let bndMergeAll: array<boundaries> => boundaries
        let b1 = bndFromPoints([mkp({x:-1., y:-4.})])
        assertEqNum(b1->bndMinX, -1., "bndMergeAll.b1.MinX")
        assertEqNum(b1->bndMaxX, -1., "bndMergeAll.b1.MaxX")
        assertEqNum(b1->bndMinY, -4., "bndMergeAll.b1.MinY")
        assertEqNum(b1->bndMaxY, -4., "bndMergeAll.b1.MaxY")
        let b1m = bndMergeAll([b1])
        assertEqNum(b1m->bndMinX, -1., "bndMergeAll.b1m.MinX")
        assertEqNum(b1m->bndMaxX, -1., "bndMergeAll.b1m.MaxX")
        assertEqNum(b1m->bndMinY, -4., "bndMergeAll.b1m.MinY")
        assertEqNum(b1m->bndMaxY, -4., "bndMergeAll.b1m.MaxY")
        let b2 = bndFromPoints([mkp({x:3., y:-7.})])
        assertEqNum(b2->bndMinX, 3., "bndMergeAll.b2.MinX")
        assertEqNum(b2->bndMaxX, 3., "bndMergeAll.b2.MaxX")
        assertEqNum(b2->bndMinY, -7., "bndMergeAll.b2.MinY")
        assertEqNum(b2->bndMaxY, -7., "bndMergeAll.b2.MaxY")
        let b2m = bndMergeAll([b1,b2])
        assertEqNum(b2m->bndMinX, -1., "bndMergeAll.b2m.MinX")
        assertEqNum(b2m->bndMaxX, 3., "bndMergeAll.b2m.MaxX")
        assertEqNum(b2m->bndMinY, -7., "bndMergeAll.b2m.MinY")
        assertEqNum(b2m->bndMaxY, -4., "bndMergeAll.b2m.MaxY")
        let b3 = bndFromPoints([mkp({x:-4., y:-2.})])
        assertEqNum(b3->bndMinX, -4., "bndMergeAll.b3.MinX")
        assertEqNum(b3->bndMaxX, -4., "bndMergeAll.b3.MaxX")
        assertEqNum(b3->bndMinY, -2., "bndMergeAll.b3.MinY")
        assertEqNum(b3->bndMaxY, -2., "bndMergeAll.b3.MaxY")
        let b3m = bndMergeAll([b1,b2,b3])
        assertEqNum(b3m->bndMinX, -4., "bndMergeAll.b3m.MinX")
        assertEqNum(b3m->bndMaxX, 3., "bndMergeAll.b3m.MaxX")
        assertEqNum(b3m->bndMinY, -7., "bndMergeAll.b3m.MinY")
        assertEqNum(b3m->bndMaxY, -2., "bndMergeAll.b3m.MaxY")

        //let bndIncludes: (boundaries, point) => bool
        let b = bndFromPoints([mkp({x:4., y:11.}), mkp({x:-14., y:-110.})])
        assertEqNum(b->bndMinX, -14., "bndIncludes.bndFromPoints.MinX")
        assertEqNum(b->bndMaxX, 4., "bndIncludes.bndFromPoints.MaxX")
        assertEqNum(b->bndMinY, -110., "bndIncludes.bndFromPoints.MinY")
        assertEqNum(b->bndMaxY, 11., "bndIncludes.bndFromPoints.MaxY")
        assertEqMsg(b->bndIncludes(mkp({x:-14., y:-110.})), true, "bndIncludes.1")
        assertEqMsg(b->bndIncludes(mkp({x:4., y:-11.})), false, "bndIncludes.2")
        assertEqMsg(b->bndIncludes(mkp({x:3.999, y:-10.999})), true, "bndIncludes.3")

        //let bndWidth: boundaries => float
        //let bndHeight: boundaries => float
        let b = bndFromPoints([mkp({x:4., y:11.}), mkp({x:-14., y:-110.})])
        assertEqNum(b->bndWidth, 18., "bndWidth")
        assertEqNum(b->bndHeight, 121., "bndHeight")
    })
})