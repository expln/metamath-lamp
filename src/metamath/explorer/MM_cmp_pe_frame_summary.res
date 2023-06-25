open Expln_React_common
open Expln_React_Mui
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open Expln_React_Modal
open Common
open MM_cmp_pe_frame_summary_state

let paddingLeft = "5px"
let paddingRight = paddingLeft

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
    openFrameExplorer:string=>unit,
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
    openFrameExplorer,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))

    React.useEffect5(() => {
        setState(_ => makeInitialState(~preCtx, ~frame, ~typeColors))
        None
    }, (typeColors, editStmtsByLeftClick, preCtx, frame, order))

    let actToggleDescrIsExpanded = () => {
        setState(toggleDescrIsExpanded)
    }

    let rndExpBtn = () => {
        if (frame.descr->Belt.Option.isSome) {
            <span>
                {React.string(nbsp ++ nbsp)}
                <span 
                    onClick=clickHnd(~act=actToggleDescrIsExpanded,())
                    style=ReactDOM.Style.make(
                        ~display="inline-block", 
                        ~transform=if(state.descrIsExpanded) {"rotate(90deg)"} else {"none"},
                        ~fontFamily="monospace",
                        ~fontSize="1.5em",
                        ~color="grey",
                        ~fontWeight="bold",
                        ~cursor="pointer",
                        ()
                    )
                >
                    { React.string( ">" ) }
                </span>
            </span>
        } else {
            <></>
        }
    }

    let rndLabel = ():reElem => {
        let asrtType = if (frame.isAxiom) {
            <span style=ReactDOM.Style.make(~color="red", ())>
                {"Axiom"->React.string}
            </span>
        } else {
            <span style=ReactDOM.Style.make(~color="green", ())>
                {"Theorem"->React.string}
            </span>
        }
        <span style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ()) >
            <span
                style=ReactDOM.Style.make(~cursor="pointer", ())
                onClick=clickHnd(~act=()=>openFrameExplorer(frame.label), ())
            >
                { React.string( order->Belt_Int.toString ++ " ") }
                asrtType
                <span 
                    style=ReactDOM.Style.make(~fontWeight="bold", ())
                >
                    { (" " ++ frame.label)->React.string }
                </span>
            </span>
            {rndExpBtn()}
        </span>
    }

    let rndDescr = () => {
        if (frame.descr->Belt.Option.isSome && state.descrIsExpanded) {
            <>
                <Divider/>
                <div style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
                    {
                        frame.descr->Belt.Option.getWithDefault("This assertion doesn't have any description.")->React.string
                    }
                </div>
            </>
        } else {
            <></>
        }
    }

    let rndDisj = () => {
        switch state.disj {
            | None => <span style=ReactDOM.Style.make(~display="none", ()) />
            | Some(disj) => {
                <>
                    <Divider/>
                    <span style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
                        { (`Disj:` ++ disjGrpDelim)->React.string }
                        {MM_cmp_pe_frame_summary_state.rndDisj(disj)}
                    </span>
                </>
            }
        }
    }

    let rndHyps = () => {
        if (state.eHyps->Js.Array2.length == 0) {
            <></>
        } else {
            state.eHyps->Js.Array2.mapi((hyp,i) => {
                <React.Fragment key={i->Belt.Int.toString}>
                    <Divider/>
                    <table style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
                        <tbody>
                            <tr>
                                <td style=ReactDOM.Style.make(~verticalAlign="top", ())>
                                    <span style=ReactDOM.Style.make(~marginLeft="10px", ())>
                                        {circleChar->React.string}
                                    </span>
                                </td>
                                <td>
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
                                </td>
                            </tr>
                        </tbody>
                    </table>
                </React.Fragment>
            })->React.array
        }
    }

    let rndAsrt = () => {
        <div style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
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
        </div>
    }

    <table>
        <tbody>
            <tr>
                <td>
                    <Paper elevation=3 style=ReactDOM.Style.make(~backgroundColor="rgb(255,255,235)", ())>
                        {rndLabel()}
                        {rndDescr()}
                        {rndDisj()}
                        {rndHyps()}
                        <Divider/>
                        {rndAsrt()}
                    </Paper>
                </td>
            </tr>
        </tbody>
    </table>


}, propsAreSame)