open Expln_2d
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open MM_provers
open Local_storage_utils

let viewBox = b => 
    `${b->bndMinX->Belt.Float.toString}` 
        ++ ` ${b->bndMinY->Belt.Float.toString}` 
        ++ ` ${b->bndWidth->Belt.Float.toString}`
        ++ ` ${b->bndHeight->Belt.Float.toString}`

let vecLine = (v:vector, ~color:string="black", ~strokeWidth:float=0.1, ()):(reElem,boundaries) => {
    let b = v->vecBegin
    let e = v->vecEnd
    (
        <line 
            x1={b->pntX->Belt.Float.toString}
            y1={b->pntY->Belt.Float.toString}
            x2={e->pntX->Belt.Float.toString}
            y2={e->pntY->Belt.Float.toString}
            stroke=color
            strokeWidth={strokeWidth->Belt_Float.toString}
        />,
        bndFromPoints([v->vecBegin, v->vecEnd])
    )
}

@react.component
let make = (
    // ~ctx:mmContext,
    // ~args:array<expr>,
    // ~label:string,
    // ~asrt:expr,
    // ~symColors1:option<Belt_HashMapString.t<string>>=?,
    // ~symColors2:option<Belt_HashMapString.t<string>>=?,
    // ~essOnly:bool=true,
) => {
    let dir = ex->vecRot(15.->deg)->vecMult(0.3)
    let (l1,b1) = ex->vecTr(dir)->vecLine(~color="red", ())
    let (l2,b2) = ey->vecTr(dir)->vecLine(~color="green", ())
    let (l3,b3) = ex->vecRot(45.->deg)->vecTr(dir)->vecLine(~color="blue", ())
    let (l4,b4) = dir->vecLine(~color="black", ())
    <svg
        viewBox=viewBox(bndMergeAll([b1,b2,b3]))
        width={300.->Belt.Float.toString}
        height={300.->Belt.Float.toString}
        transform="scale(1, -1)"
    >
        l1
        l2
        l3
    </svg>
}