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
        ~bottom:float=?
    ) => boundaries
    let bndAddMarginPct: (
        boundaries, 
        ~all:float=?,
        ~left:float=?,
        ~right:float=?,
        ~top:float=?,
        ~bottom:float=?
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

module Std2D: Utils2D
module Svg2D: Utils2D