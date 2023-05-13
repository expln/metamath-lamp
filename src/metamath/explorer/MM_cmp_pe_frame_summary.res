open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open MM_proof_tree
open MM_proof_tree_dto
open Expln_React_Modal
open Local_storage_utils
open Common
open MM_cmp_user_stmt
open MM_cmp_pe_frame_summary_state

type props = {
    modalRef:modalRef,
    settingsVer:int,
    preCtxVer:int,
    preCtx:mmContext,
    frame:frame,
    order:int,
    typeColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.preCtxVer == b.preCtxVer
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    settingsVer,
    preCtxVer,
    preCtx,
    frame,
    order,
    typeColors,
    editStmtsByLeftClick,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))

    React.useEffect2(() => {
        setState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))
        None
    }, (settingsVer, preCtxVer))

    let rndLabel = ():reElem => {
        <span>
            {
                React.string(
                    order->Belt_Int.toString ++ " " ++ frame.label
                )
            }
        </span>
    }

    let rndAsrt = () => {
        <MM_cmp_pe_stmt
            modalRef
            settingsVer
            ctxVer=preCtxVer
            ctx=state.frmCtx
            stmt=state.asrt
            symColors=state.symColors
            symRename=state.symRename
            editStmtsByLeftClick
        />
    }

    <Paper>
        {rndLabel()}
        <Divider/>
        {rndAsrt()}
    </Paper>

}, propsAreSame)