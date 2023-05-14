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
    syntaxTypes:array<int>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,

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
    syntaxTypes,
    frms,
    parenCnt,
    frame,
    order,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))

    React.useEffect5(() => {
        setState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))
        None
    }, (typeColors, editStmtsByLeftClick, preCtx, frame, order))

    let rndLabel = ():reElem => {
        let asrtType = if (frame.isAxiom) {"Axiom"} else {"Theorem"}
        <span style=ReactDOM.Style.make(~marginLeft="10px", ())>
            { React.string( order->Belt_Int.toString ++ " " ++ asrtType ++ " ") }
            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                { frame.label->React.string }
            </span>
        </span>
    }

    let rndDescr = () => {
        <>
            <Divider/>
            <span>
                {
                    frame.descr->Belt.Option.getWithDefault("This assertion doesn't have any description.")->React.string
                }
            </span>
        </>
    }

    let rndHyps = () => {
        if (state.eHyps->Js.Array2.length == 0) {
            <></>
        } else {
            state.eHyps->Js.Array2.mapi((hyp,i) => {
                <React.Fragment key={i->Belt.Int.toString}>
                    <Divider/>
                    <Row>
                        <span style=ReactDOM.Style.make(~marginLeft="10px", ())>
                            {circleChar->React.string}
                        </span>
                        <MM_cmp_pe_stmt
                            modalRef
                            ctx=state.frmCtx
                            syntaxTypes
                            frms
                            parenCnt
                            stmt=hyp
                            symColors=state.symColors
                            symRename=state.symRename
                            editStmtsByLeftClick
                        />
                    </Row>
                </React.Fragment>
            })->React.array
        }
    }

    let rndAsrt = () => {
        <MM_cmp_pe_stmt
            modalRef
            ctx=state.frmCtx
            syntaxTypes
            frms
            parenCnt
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
                    <Paper elevation=3 style=ReactDOM.Style.make(~backgroundColor="rgb(255,255,235)", ())>
                        {rndLabel()}
                        {rndDescr()}
                        {rndHyps()}
                        <Divider/>
                        {rndAsrt()}
                    </Paper>
                </td>
            </tr>
        </tbody>
    </table>


}, propsAreSame)