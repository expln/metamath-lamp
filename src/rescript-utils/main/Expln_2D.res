open Expln_utils_common

module type Utils2D = {
    type point
    type vector
    type angle
    type boundaries

    let ex: vector
    let ey: vector

    let deg: float => angle
    let rad: float => angle
    let toDeg: angle => float
    let toRad: angle => float

    let pntX: point => float
    let pntY: point => float
    let pntLen: point => float
    let pntSub: (point,point) => point
    let pntAdd: (point,point) => point
    let pntTr: (point, vector) => point
    let pntTrDir: (point, vector, float) => point
    let pntMul: (point, float) => point
    let pntDiv: (point, float) => point
    let pntVec: (point,point) => vector
    let pntRot: (point, angle) => point

    let vecBegin: vector => point
    let vecEnd: vector => point
    let vecLen: vector => float
    let vecRev: vector => vector
    let vecMul: (vector, float) => vector
    let vecMulVec: (vector, vector) => float
    let vecDiv: (vector, float) => vector
    let vecAdd: (vector, vector) => vector
    let vecRot: (vector, angle) => vector
    let vecNorm: vector => vector
    let vecSwapEnds: vector => vector
    let vecBeginAt: (vector, point) => vector
    let vecEndAt: (vector, point) => vector
    let vecTr: (vector, vector) => vector
    let vecTrDir: (vector, vector, float) => vector

    let bndFromPoints: array<point> => boundaries
    let bndFromVectors: array<vector> => boundaries
    let bndAddPoint: (boundaries,point) => boundaries
    let bndAddPoints: (boundaries,array<point>) => boundaries
    let bndLeftBottom: boundaries => point
    let bndLeftTop: boundaries => point
    let bndRightBottom: boundaries => point
    let bndRightTop: boundaries => point
    let bndAddMargin: (
        boundaries, 
        ~all:float=?,
        ~left:float=?,
        ~right:float=?,
        ~top:float=?,
        ~bottom:float=?,
        ()
    ) => boundaries
    let bndAddMarginPct: (
        boundaries, 
        ~all:float=?,
        ~left:float=?,
        ~right:float=?,
        ~top:float=?,
        ~bottom:float=?,
        ()
    ) => boundaries
    let bndMerge: (boundaries,boundaries) => boundaries
    let bndMergeAll: array<boundaries> => boundaries
    let bndMinX: boundaries => float
    let bndMinY: boundaries => float
    let bndMaxX: boundaries => float
    let bndMaxY: boundaries => float
    let bndIncludes: (boundaries, point) => bool
    let bndWidth: boundaries => float
    let bndHeight: boundaries => float
}

module Std2D: Utils2D = {
    type point = {x: float, y: float}
    type vector = {begin: point, end: point}
    type angle = float
    type boundaries = {minX: float, minY: float, maxX: float, maxY: float}

    let ex = {begin:{x:0., y:0.}, end:{x:1., y:0.}}
    let ey = {begin:{x:0., y:0.}, end:{x:0., y:1.}}

    let deg = (d:float):angle => d /. 180. *. Js.Math._PI
    let rad = (r:float):angle => r
    let toDeg = (a:angle):float => a /. Js.Math._PI *. 180.
    let toRad = (a:angle):float => a

    let pntX = (p:point):float => p.x
    let pntY = (p:point):float => p.y
    let pntLen = (p:point):float => Js.Math.sqrt(p.x *. p.x +. p.y *. p.y)
    let pntSub = (a:point,b:point):point => {x: a.x -. b.x, y: a.y -. b.y}
    let pntAdd = (a:point,b:point):point => {x: a.x +. b.x, y: a.y +. b.y}
    let pntTrDelta = (p:point,dx:float,dy:float):point => {x: p.x +. dx, y: p.y +. dy}
    let pntTr = (p:point, v:vector):point => p->pntTrDelta(v.end.x -. v.begin.x, v.end.y -. v.begin.y)
    let pntMul = (p:point, x:float):point => {x: p.x *. x, y: p.y *. x}
    let pntDiv = (p:point, x:float):point => {x: p.x /. x, y: p.y /. x}
    let pntVec = (b:point,e:point):vector => {begin:b, end:e}
    let pntRot = (p:point, a:angle):point => {
        x: p.x *. Js.Math.cos(a) -. p.y *. Js.Math.sin(a),
        y: p.x *. Js.Math.sin(a) +. p.y *. Js.Math.cos(a),
    }


    let vecBegin = (v:vector):point => v.begin
    let vecEnd = (v:vector):point => v.end
    let vecLen = (v:vector):float => v.end->pntSub(v.begin)->pntLen
    let vecMul = (v:vector, x:float):vector => {begin: v.begin, end: v.end->pntSub(v.begin)->pntMul(x)->pntAdd(v.begin)}
    let vecMulVec = (v1:vector, v2:vector):float => {
        let a = v1.end->pntSub(v1.begin)
        let b = v2.end->pntSub(v2.begin)
        a.x *. b.x +. a.y *. b.y
    }
    let vecDiv = (v:vector, x:float):vector => {begin: v.begin, end: v.end->pntSub(v.begin)->pntDiv(x)->pntAdd(v.begin)}
    let vecAdd = (a:vector, b:vector):vector => {begin: a.begin, end: a.end->pntAdd(b.end->pntSub(b.begin))}
    let vecRot = (v:vector, a:angle):vector => {
        begin: v.begin,
        end: v.end->pntSub(v.begin)->pntRot(a)->pntAdd(v.begin)
    }
    let vecNorm = (v:vector):vector => v->vecDiv(v->vecLen)
    let vecSwapEnds = (v:vector):vector => {begin: v.end, end:v.begin}
    let vecBeginAt = (v:vector, p:point):vector => {begin: p, end: p -> pntTr(v)}
    let vecEndAt = (v:vector, p:point):vector => v->vecSwapEnds->vecBeginAt(p)->vecSwapEnds
    let vecTrDelta = (v:vector, dx:float, dy:float):vector => {
        begin: v.begin->pntTrDelta(dx,dy),
        end: v.end->pntTrDelta(dx,dy)
    }
    let vecTr = (v:vector, t:vector):vector => {
        let dx = t.end.x -. t.begin.x
        let dy = t.end.y -. t.begin.y
        v->vecTrDelta(dx,dy)
    }
    let vecTrDir = (v:vector, dir:vector, dist:float):vector => v->vecTr(dir->vecNorm->vecMul(dist))
    let pntTrDir = (p:point, dir:vector, dist:float):point => p->pntTr(dir->vecNorm->vecMul(dist))
    let vecRev = (v:vector):vector => v->vecRot(rad(Js_math._PI))

    let bndMinX = (b:boundaries):float => b.minX
    let bndMinY = (b:boundaries):float => b.minY
    let bndMaxX = (b:boundaries):float => b.maxX
    let bndMaxY = (b:boundaries):float => b.maxY
    let bndIncludes = (b:boundaries, p:point):bool => b.minX <= p.x && p.x < b.maxX && b.minY <= p.y && p.y < b.maxY
    let bndWidth = (b:boundaries):float => b.maxX -. b.minX
    let bndHeight = (b:boundaries):float => b.maxY -. b.minY
    let bndFromPoints = (ps:array<point>):boundaries => {
        if (ps->Js.Array2.length == 0) {
            exn("Cannot create boundaries from an empty array of points.")
        }
        let minX = ref((ps->Array.getUnsafe(0)).x)
        let minY = ref((ps->Array.getUnsafe(0)).y)
        let maxX = ref(minX.contents)
        let maxY = ref(minY.contents)
        for i in 1 to ps->Js.Array2.length - 1 {
            let p = ps->Array.getUnsafe(i)
            minX := Js.Math.min_float(minX.contents, p.x)
            minY := Js.Math.min_float(minY.contents, p.y)
            maxX := Js.Math.max_float(maxX.contents, p.x)
            maxY := Js.Math.max_float(maxY.contents, p.y)
        }
        {minX:minX.contents, minY:minY.contents, maxX:maxX.contents, maxY:maxY.contents}
    }
    let bndFromVectors = (vs:array<vector>): boundaries => {
        if (vs->Js.Array2.length == 0) {
            exn("Cannot create boundaries from an empty array of vectors.")
        }
        let p1 = vs->Array.getUnsafe(0)->vecBegin
        let p2 = vs->Array.getUnsafe(0)->vecEnd
        let minX = ref(Js.Math.min_float(p1.x, p2.x))
        let minY = ref(Js.Math.min_float(p1.y, p2.y))
        let maxX = ref(Js.Math.max_float(p1.x, p2.x))
        let maxY = ref(Js.Math.max_float(p1.y, p2.y))
        for i in 1 to vs->Js.Array2.length - 1 {
            let p1 = vs->Array.getUnsafe(i)->vecBegin
            let p2 = vs->Array.getUnsafe(i)->vecEnd
            minX := Js.Math.min_float(minX.contents, Js.Math.min_float(p1.x, p2.x))
            minY := Js.Math.min_float(minY.contents, Js.Math.min_float(p1.y, p2.y))
            maxX := Js.Math.max_float(maxX.contents, Js.Math.max_float(p1.x, p2.x))
            maxY := Js.Math.max_float(maxY.contents, Js.Math.max_float(p1.y, p2.y))
        }
        {minX:minX.contents, minY:minY.contents, maxX:maxX.contents, maxY:maxY.contents}
    }
    let bndAddPoint = (b:boundaries, p:point):boundaries => {
        minX:Js.Math.min_float(b.minX,p.x),
        minY:Js.Math.min_float(b.minY,p.y),
        maxX:Js.Math.max_float(b.maxX,p.x),
        maxY:Js.Math.max_float(b.maxY,p.y),
    }
    let bndMerge = (b1:boundaries, b2:boundaries):boundaries => {
        minX:Js.Math.min_float(b1.minX,b2.minX),
        minY:Js.Math.min_float(b1.minY,b2.minY),
        maxX:Js.Math.max_float(b1.maxX,b2.maxX),
        maxY:Js.Math.max_float(b1.maxY,b2.maxY),
    }
    let bndAddPoints = (b:boundaries, ps:array<point>):boundaries => {
        if (ps->Js.Array2.length == 0) {
            b
        } else {
            b->bndMerge(bndFromPoints(ps))
        }
    }
    let bndLeftBottom = (b:boundaries):point => {
        {x:b.minX, y:b.minY}
    }
    let bndLeftTop = (b:boundaries):point => {
        {x:b.minX, y:b.maxY}
    }
    let bndRightBottom = (b:boundaries):point => {
        {x:b.maxX, y:b.minY}
    }
    let bndRightTop = (b:boundaries):point => {
        {x:b.maxX, y:b.maxY}
    }
    let bndAddMargin = (
        b:boundaries, 
        ~all:option<float>=?,
        ~left:option<float>=?,
        ~right:option<float>=?,
        ~top:option<float>=?,
        ~bottom:option<float>=?,
        ()
    ):boundaries => {
        switch all {
            | Some(margin) => {
                { minX: b.minX -. margin, minY: b.minY -. margin, maxX: b.maxX +. margin, maxY: b.maxY +. margin }
            }
            | None => {
                {
                    minX: b.minX -. left->Belt_Option.getWithDefault(0.),
                    minY: b.minY -. bottom->Belt_Option.getWithDefault(0.),
                    maxX: b.maxX +. right->Belt_Option.getWithDefault(0.),
                    maxY: b.maxY +. top->Belt_Option.getWithDefault(0.),
                }
            }
        }
    }
    let bndAddMarginPct = (
        b:boundaries, 
        ~all:option<float>=?,
        ~left:option<float>=?,
        ~right:option<float>=?,
        ~top:option<float>=?,
        ~bottom:option<float>=?,
        ()
    ):boundaries => {
        switch all {
            | Some(allPct) => {
                let size = Js_math.max_float(b->bndWidth, b->bndHeight)
                let margin = size *. allPct
                {minX: b.minX -. margin, minY: b.minY -. margin, maxX: b.maxX +. margin, maxY: b.maxY +. margin}
            }
            | None => {
                let w = b->bndWidth
                let h = b->bndHeight
                {
                    minX: b.minX -. w *. left->Belt_Option.getWithDefault(0.),
                    minY: b.minY -. h *. bottom->Belt_Option.getWithDefault(0.),
                    maxX: b.maxX +. w *. right->Belt_Option.getWithDefault(0.),
                    maxY: b.maxY +. h *. top->Belt_Option.getWithDefault(0.),
                }
            }
        }
    }
    let bndMergeAll = (bs:array<boundaries>):boundaries => {
        if (bs->Js.Array2.length == 0) {
            exn("Cannot merge empty array of boundaries.")
        } else {
            let b = ref(bs->Array.getUnsafe(0))
            for i in 1 to bs->Js.Array2.length - 1 {
                b := b.contents->bndMerge(bs->Array.getUnsafe(i))
            }
            b.contents
        }
    }
}

module Svg2D: Utils2D = {
    type point = Std2D.point
    type vector = Std2D.vector
    type angle = Std2D.angle
    type boundaries = Std2D.boundaries

    let ex = Std2D.ex
    let ey = Std2D.ey->Std2D.vecRev

    let deg = (d:float) => Std2D.deg(-.d)
    let rad = (r:float) => Std2D.rad(-.r)
    let toDeg = (a:angle) => -.Std2D.toDeg(a)
    let toRad = (a:angle) => -.Std2D.toRad(a)

    let pntX = Std2D.pntX
    let pntY = Std2D.pntY
    let pntLen = Std2D.pntLen
    let pntSub = Std2D.pntSub
    let pntAdd = Std2D.pntAdd
    let pntTr = Std2D.pntTr
    let pntMul = Std2D.pntMul
    let pntDiv = Std2D.pntDiv
    let pntVec = Std2D.pntVec
    let pntRot = Std2D.pntRot

    let vecBegin = Std2D.vecBegin
    let vecEnd = Std2D.vecEnd
    let vecLen = Std2D.vecLen
    let vecMul = Std2D.vecMul
    let vecMulVec = Std2D.vecMulVec
    let vecDiv = Std2D.vecDiv
    let vecAdd = Std2D.vecAdd
    let vecRot = Std2D.vecRot
    let vecNorm = Std2D.vecNorm
    let vecSwapEnds = Std2D.vecSwapEnds
    let vecBeginAt = Std2D.vecBeginAt
    let vecEndAt = Std2D.vecEndAt
    let vecTr = Std2D.vecTr
    let vecTrDir = Std2D.vecTrDir
    let pntTrDir = Std2D.pntTrDir
    let vecRev = Std2D.vecRev

    let bndMinX = Std2D.bndMinX
    let bndMinY = Std2D.bndMinY
    let bndMaxX = Std2D.bndMaxX
    let bndMaxY = Std2D.bndMaxY
    let bndIncludes = Std2D.bndIncludes
    let bndWidth = Std2D.bndWidth
    let bndHeight = Std2D.bndHeight
    let bndFromPoints = Std2D.bndFromPoints
    let bndFromVectors = Std2D.bndFromVectors
    let bndAddPoint = Std2D.bndAddPoint
    let bndMerge = Std2D.bndMerge
    let bndAddPoints = Std2D.bndAddPoints
    let bndLeftBottom = Std2D.bndLeftTop
    let bndLeftTop = Std2D.bndLeftBottom
    let bndRightBottom = Std2D.bndRightTop
    let bndRightTop = Std2D.bndRightBottom
    let bndAddMargin = (
        b:boundaries, 
        ~all:option<float>=?,
        ~left:option<float>=?,
        ~right:option<float>=?,
        ~top:option<float>=?,
        ~bottom:option<float>=?,
        ()
    ):boundaries => Std2D.bndAddMargin( b, ~all=?all, ~left=?left, ~right=?right, ~top=?bottom, ~bottom=?top, () )
    let bndAddMarginPct = (
        b:boundaries, 
        ~all:option<float>=?,
        ~left:option<float>=?,
        ~right:option<float>=?,
        ~top:option<float>=?,
        ~bottom:option<float>=?,
        ()
    ):boundaries => Std2D.bndAddMarginPct( b, ~all=?all, ~left=?left, ~right=?right, ~top=?bottom, ~bottom=?top, () )
    let bndMergeAll = Std2D.bndMergeAll
}


