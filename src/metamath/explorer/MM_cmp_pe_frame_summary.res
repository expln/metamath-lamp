open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open Expln_React_Modal
open Common
open MM_cmp_pe_frame_summary_state
open MM_wrk_settings

let paddingLeft = "5px"
let paddingRight = paddingLeft

type props = {
    modalRef:modalRef,
    typeColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    typeOrderInDisj:Belt_HashMapInt.t<int>,

    settings:settings,
    preCtx:mmContext,
    symColors: Belt_HashMapString.t<string>,
    syntaxTypes:array<int>,
    frms: frms,
    parenCnt: parenCnt,

    frame:frame,
    order:option<int>,
    matchedIdxs:option<MM_wrk_pattern_search.matchedIndices>,
    openFrameExplorer:option<string=>unit>,
    openExplorer:option<(~initPatternFilterStr:string=?, ~initDependsOnFilter:string=?)=>unit>,
    addAsrtByLabel:option<string=>promise<result<unit,string>>>,
}

let propsAreSame = (a:props,b:props):bool => {
    a.typeColors === b.typeColors
    && a.editStmtsByLeftClick === b.editStmtsByLeftClick
    && a.typeOrderInDisj === b.typeOrderInDisj
    && a.settings === b.settings
    && a.preCtx === b.preCtx
    && a.symColors === b.symColors
    && a.frame === b.frame
    && a.order === b.order
    && a.matchedIdxs === b.matchedIdxs
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    typeColors,
    typeOrderInDisj,
    editStmtsByLeftClick,
    settings,
    preCtx,
    symColors,
    syntaxTypes,
    frms,
    parenCnt,
    frame,
    order,
    matchedIdxs,
    openFrameExplorer,
    openExplorer,
    addAsrtByLabel,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~preCtx, ~frame, ~typeColors, ~typeOrderInDisj))
    let (asrtWasAddedToEditor, setAsrtWasAddedToEditor) = React.useState(() => None)

    React.useEffect6(() => {
        setState(_ => makeInitialState(~preCtx, ~frame, ~typeColors, ~typeOrderInDisj))
        None
    }, (typeColors, editStmtsByLeftClick, typeOrderInDisj, preCtx, frame, order))

    let actToggleDescrIsExpanded = () => {
        setState(toggleDescrIsExpanded)
    }

    let actAddAsrtToEditor = (label:string) => {
        switch addAsrtByLabel {
            | None => ()
            | Some(addAsrtByLabel) => {
                addAsrtByLabel(label)->promiseMap(res => {
                    setAsrtWasAddedToEditor(msgAndTimerId => {
                        switch msgAndTimerId {
                            | None => ()
                            | Some((_, timerId)) => clearTimeout(timerId)
                        }
                        Some((
                            res,
                            setTimeout(
                                () => setAsrtWasAddedToEditor(_ => None),
                                5000
                            )
                        ))
                    })
                })->Promise.done
            }
        }
    }

    let rndExpBtn = () => {
        if (frame.descr->Option.isSome) {
            <span>
                {React.string(nbsp ++ nbsp)}
                <span 
                    onClick=clickHnd(~act=actToggleDescrIsExpanded)
                    style=ReactDOM.Style.make(
                        ~fontFamily="monospace",
                        ~color="grey",
                        ~cursor="pointer",
                        ()
                    )
                >
                    { React.string( "descr" ) }
                </span>
                <span 
                    onClick=clickHnd(~act=actToggleDescrIsExpanded)
                    style=ReactDOM.Style.make(
                        ~display="inline-block", 
                        ~transform=state.descrIsExpanded 
                            ? {"rotate(90deg) translate(1px, -2px)"} 
                            : {"none"},
                        ~fontFamily="monospace",
                        ~color="grey",
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

    let rndUseBtn = () => {
        switch addAsrtByLabel {
            | None => React.null
            | Some(_) => {
                <span>
                    {React.string(nbsp ++ nbsp)}
                    <span 
                        onClick=clickHnd(~act=() => actAddAsrtToEditor(frame.label))
                        style=ReactDOM.Style.make(
                            ~fontFamily="monospace",
                            ~color="grey",
                            ~cursor="pointer",
                            ()
                        )
                    >
                        { React.string( "use" ) }
                    </span>
                    {
                        switch asrtWasAddedToEditor {
                            | None => React.null
                            | Some((Ok(_), _)) => {
                                <span style=ReactDOM.Style.make( ~fontFamily="monospace", ~color="black", () ) >
                                    { React.string( nbsp ++ "Added to the editor" ) }
                                </span>
                            }
                            | Some((Error(msg), _)) => {
                                <span style=ReactDOM.Style.make( ~fontFamily="monospace", ~color="red", () ) >
                                    { React.string( nbsp ++ "Error: " ++ msg ) }
                                </span>
                            }
                        }
                    }
                </span>
            }
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
                style=ReactDOM.Style.make(~cursor=openFrameExplorer->Option.isSome?"pointer":"default", ())
                onClick=clickHnd(~act=()=>openFrameExplorer->Option.forEach(fn => fn(frame.label)))
            >
                { React.string( order->Option.mapOr("", Belt_Int.toString(_)) ++ " ") }
                asrtType
                { React.string( " ") }
                <span 
                    style=ReactDOM.Style.make(
                        ~fontWeight="bold", 
                        ~backgroundColor=?getFrmLabelBkgColor(frame, settings), 
                        ~borderRadius="3px",
                        ()
                    )
                >
                    { (frame.label)->React.string }
                </span>
            </span>
            {rndExpBtn()}
            {rndUseBtn()}
        </span>
    }

    let rndDescr = () => {
        if (frame.descr->Belt.Option.isSome && state.descrIsExpanded) {
            <>
                <Divider/>
                <div style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
                    <MM_cmp_pe_frame_descr
                        settings
                        ctx=preCtx
                        symColors
                        openFrameExplorer={label=>openFrameExplorer->Option.forEach(func => func(label))}
                        text={
                            frame.descr->Belt.Option.getWithDefault(
                                `This ${frame.isAxiom?"axiom":"theorem"} doesn't have a description.`
                            )
                        }
                    />
                </div>
            </>
        } else {
            <></>
        }
    }

    let rndUsageCnt = () => {
        if (state.descrIsExpanded) {
            <>
                <Divider/>
                <span style=ReactDOM.Style.make(~paddingLeft, ~paddingRight, ())>
                    { React.string("Referenced by: ") }
                    <a 
                        onClick={_=>openExplorer->Option.forEach(fn=>fn(~initDependsOnFilter=frame.label))} 
                        style=ReactDOM.Style.make(~color="blue", ~textDecoration="underline", ~cursor="pointer", ())
                    >
                        {React.string(frame.usageCnt->Int.toString)}
                    </a>
                </span>
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
        if (state.eHyps->Array.length == 0) {
            <></>
        } else {
            state.eHyps->Array.mapWithIndex((hyp,i) => {
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
                                        frameRestrict=settings.allowedFrms.inSyntax
                                        parenCnt
                                        stmt=hyp
                                        symColors=state.symColors
                                        symRename=state.symRename
                                        editStmtsByLeftClick
                                        openExplorer
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
                frameRestrict=settings.allowedFrms.inSyntax
                parenCnt
                stmt=state.asrt
                symColors=state.symColors
                symRename=state.symRename
                editStmtsByLeftClick
                openExplorer
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
                        {rndUsageCnt()}
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