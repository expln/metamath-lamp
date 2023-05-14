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
    typeColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    preCtx:mmContext,
    frame:frame,
    order:int,
}

let propsAreSame = (a:props,b:props):bool => {
    a.typeColors === b.typeColors
    && a.editStmtsByLeftClick === b.editStmtsByLeftClick
    && a.preCtx === b.preCtx
    && a.frame === b.frame
    && a.order === b.order
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    typeColors,
    editStmtsByLeftClick,
    preCtx,
    frame,
    order,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))

    React.useEffect5(() => {
        setState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))
        None
    }, (typeColors, editStmtsByLeftClick, preCtx, frame, order))

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
            ctx=state.frmCtx
            stmt=state.asrt
            symColors=state.symColors
            symRename=state.symRename
            editStmtsByLeftClick
        />
    }

    <table>
        <tbody>
            <tr>
                <td>
                    <Paper elevation=3>
                        {rndLabel()}
                        <Divider/>
                        {rndAsrt()}
                    </Paper>
                </td>
            </tr>
        </tbody>
    </table>


}, propsAreSame)