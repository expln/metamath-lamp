open MM_context
open MM_wrk_settings
open Expln_React_Modal
open Expln_React_Mui
open MM_wrk_pre_ctx_data
open MM_wrk_editor
open MM_wrk_LoadCtx
open MM_parser
open MM_proof_table
open Common

type state = {
    ctx:mmContext,
    frame:frame,
    hyps:array<expr>,
    asrt:expr,
    proofTable:option<proofTable>,
    symColors:Belt_HashMapString.t<string>,
}

let createInitialState = (~preCtxData:preCtxData, ~ctx:mmContext, ~frame:frame):state => {
    let frmIntToCtxInt = (i:int):int => {
        if (i < 0) {
            i
        } else {
            switch ctx->ctxSymToInt(frame.frameVarToSymb[i]) {
                | None => raise(MmException({msg:`ctx->ctxSymToInt == None in frmIntToCtxInt`}))
                | Some(n) => n
            }
        }
    }

    let frmExprToCtxExpr = (expr:expr):expr => {
        expr->Js_array2.map(frmIntToCtxInt)
    }

    let asrt = frame.asrt->frmExprToCtxExpr
    let proofTable = switch frame.proof {
        | None => None
        | Some(proof) => {
            let proofRoot = MM_proof_verifier.verifyProof(
                ~ctx,
                ~expr=asrt,
                ~proof,
                ~isDisjInCtx = (_,_) => true,
            )
            Some(createProofTableFromProof(proofRoot))
        }
    }
    let typeColors = preCtxData.settingsV.val->settingsGetTypeColors
    {
        ctx,
        frame,
        hyps:frame.hyps->Js.Array2.map(hyp => frmExprToCtxExpr(hyp.expr)),
        asrt,
        proofTable,
        symColors: createSymbolColors(~ctx, ~typeColors),
    }
}

let loadFrameContext = (
    ~srcs:array<mmCtxSrcDto>,
    ~label:string,
    ~onProgress:float=>unit,
    ~onDone: result<mmContext,string>=>unit,
):unit => {
    beginLoadingMmContext(
        ~scopes = createMmScopesForFrame( ~srcs, ~label, ),
        ~onProgress,
        ~onDone,
        ~dontChangeNestingLevelForLastElem=true,
        ()
    )
}

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
    label:string,
}

let propsAreSame = (a:props, b:props):bool => {
    true
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
    label,
}:props) => {
    let (loadPct, setLoadPct) = React.useState(() => 0.)
    let (state, setState) = React.useState(() => None)

    switch state {
        | None => {
            `Loading ${floatToPctStr(loadPct)}`->React.string
        }
        | Some(state) => {
            "Loaded"->React.string
        }
    }

}, propsAreSame)