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

@val external window: {..} = "window"

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool=true, 
    ~title:option<string>=?, 
    ~color:option<string>=Some("primary"),
    ()
) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} ?color> icon </IconButton>
    </span>
}

type state = {
    newText: string,
    infoExpanded: bool,
}

let makeInitialState = () => {
    {
        newText: "",
        infoExpanded: false
    }
}

let setNewText = (st,text):state => {
    {
        ...st,
        newText:text
    }
}

let setInfoExpanded = (st,infoExpanded):state => {
    {
        ...st,
        infoExpanded
    }
}

let textToSyntaxTree = (
    ~wrkCtx:mmContext,
    ~syms:array<stmtSym>,
    ~syntaxTypes:array<int>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
):result<syntaxTreeNode,string> => {
    if (syntaxTypes->Js_array2.length == 0) {
        Error(`Could not determine syntax types.`)
    } else {
        switch syms->Js.Array2.find(stmtSym => wrkCtx->ctxSymToInt(stmtSym.sym)->Belt_Option.isNone) {
            | Some({sym:unrecognizedSymbol}) => {
                Error(`The statement contains an unrecognized symbol: '${unrecognizedSymbol}'`)
            }
            | None => {
                let lastSyntaxTypeInt = lastSyntaxType->Belt.Option.flatMap(wrkCtx->ctxSymToInt)->Belt.Option.getWithDefault(0)
                let syntaxTypes = syntaxTypes->Js.Array2.copy->Js.Array2.sortInPlaceWith((a,b) => {
                    if (a == lastSyntaxTypeInt) {
                        -1
                    } else if (b == lastSyntaxTypeInt) {
                        1
                    } else {
                        a - b
                    }
                })
                let stmt = syms->Js_array2.map(stmtSym => stmtSym.sym)->ctxSymsToIntsExn(wrkCtx, _)
                let expr = stmt->Js_array2.sliceFrom(1)
                let proofTree = MM_provers.proveSyntaxTypes(
                    ~wrkCtx=wrkCtx,
                    ~frms,
                    ~parenCnt,
                    ~exprs=[expr],
                    ~syntaxTypes,
                    ()
                )
                switch proofTree->ptGetSyntaxProof(expr) {
                    | None => {
                        Error(
                            `Could not prove this statement is of any of the types: ` 
                                ++ `${wrkCtx->ctxIntsToSymsExn(syntaxTypes)->Js.Array2.joinWith(", ")}`
                        )
                    }
                    | Some(node) => {
                        switch (lastSyntaxType, wrkCtx->ctxIntToSym((node->pnGetExpr)[0])) {
                            | (None, Some(provedSyntaxTypeStr)) => onLastSyntaxTypeChange(provedSyntaxTypeStr)
                            | (Some(lastSyntaxTypeStr), Some(provedSyntaxTypeStr)) => {
                                if (lastSyntaxTypeStr != provedSyntaxTypeStr) {
                                    onLastSyntaxTypeChange(provedSyntaxTypeStr)
                                }
                            }
                            | _ => ()
                        }
                        buildSyntaxTreeFromProofTree( ~ctx=wrkCtx, ~proofTree, ~typeStmt=node->pnGetExpr, )
                    }
                }
            }
        }
    }
}

let lastSyntaxTypeLocStorKey = "editor-last-syntax-type"

let getLastSyntaxType = ():option<string> => {
    locStorReadString(lastSyntaxTypeLocStorKey)
}

let setLastSyntaxType = (lastSyntaxType:string):unit => {
    locStorWriteString(lastSyntaxTypeLocStorKey, lastSyntaxType)
}

let callbackOpt = (clbkOpt:option<'a=>unit>):('a=>unit) => {
    a => clbkOpt->Belt_Option.forEach(clbk => clbk(a))
}

let rndSymbol = (
    ~isFirst:bool,
    ~key:string,
    ~sym:string,
    ~color:option<string>,
    ~symRename:option<Belt_HashMapString.t<string>>=?,
    ~onLeftClick:option<unit=>unit>=?,
    ~onAltLeftClick:option<unit=>unit>=?,
    ~longClickEnabled:bool=false,
    ~spaceBackgroundColor:option<string>=?,
    ~symbolBackgroundColor:option<string>=?,
    ~cursor:string="auto",
    ()
):reElem => {
    <React.Fragment key>
        {
            if (isFirst) {
                <></>
            } else {
                <LongClickSpan
                    onClick=?{
                        if (
                            !longClickEnabled 
                                && (onLeftClick->Belt.Option.isSome || onAltLeftClick->Belt.Option.isSome)
                        ) {
                            Some(clickHnd2(
                                clickClbkMake(~act = callbackOpt(onLeftClick), ()),
                                clickClbkMake(~alt=true, ~act=callbackOpt(onAltLeftClick), ()),
                            ))
                        } else {
                            None
                        }
                    }
                    longClickEnabled
                    onShortClick=?{onLeftClick->Belt.Option.map(onLeftClick => {
                        (clickAttrs:option<UseLongClick.clickAttrs>) => {
                            switch clickAttrs {
                                | None => onLeftClick()
                                | Some({alt}) => {
                                    if (alt) {
                                        callbackOpt(onAltLeftClick)()
                                    } else {
                                        onLeftClick()
                                    }
                                }
                            }
                        }
                    })}
                    onLongClick=?onAltLeftClick
                    style=ReactDOM.Style.make(
                        ~backgroundColor=?spaceBackgroundColor,
                        ~cursor,
                        ()
                    )
                > 
                    {" "->React.string} 
                </LongClickSpan>
            }
        }
        {
            let (color,fontWeight) = switch color {
                | None => ("black","normal")
                | Some(color) => (color,"bold")
            }
            <LongClickSpan 
                onClick=?{
                    if (
                        !longClickEnabled 
                            && (onLeftClick->Belt.Option.isSome || onAltLeftClick->Belt.Option.isSome)
                    ) {
                        Some(clickHnd2(
                            clickClbkMake(~act = callbackOpt(onLeftClick), ()),
                            clickClbkMake(~alt=true, ~act=callbackOpt(onAltLeftClick), ()),
                        ))
                    } else {
                        None
                    }
                }
                longClickEnabled
                onShortClick=?{onLeftClick->Belt.Option.map(onLeftClick => {
                    (clickAttrs:option<UseLongClick.clickAttrs>) => {
                        switch clickAttrs {
                            | None => onLeftClick()
                            | Some({alt}) => {
                                if (alt) {
                                    callbackOpt(onAltLeftClick)()
                                } else {
                                    onLeftClick()
                                }
                            }
                        }
                    }
                })}
                onLongClick=?onAltLeftClick
                style=ReactDOM.Style.make( ~color, ~fontWeight, ~backgroundColor=?symbolBackgroundColor, ~cursor, () ) 
            >
                {
                    React.string(
                        symRename->Belt_Option.flatMap(Belt_HashMapString.get(_, sym))->Belt.Option.getWithDefault(sym)
                    )
                }
            </LongClickSpan>
        }
    </React.Fragment>
}

let rndContText = (
    ~stmtCont:stmtCont,
    ~symRename:option<Belt_HashMapString.t<string>>=?,
    ~onTextLeftClick:option<int=>unit>=?,
    ~onTextAltLeftClick:option<int=>unit>=?,
    ~onTreeLeftClick:option<int=>unit>=?,
    ~onTreeAltLeftClick:option<int=>unit>=?,
    ~longClickEnabled:bool=false,
    ~cursor:string="auto",
    ~renderSelection:bool=false,
    ()
) => {
    switch stmtCont {
        | Text(syms) => {
            syms->Js.Array2.mapi((stmtSym,i) => {
                rndSymbol(
                    ~isFirst = i==0,
                    ~key=i->Belt.Int.toString,
                    ~sym=stmtSym.sym,
                    ~color=stmtSym.color,
                    ~onLeftClick = ?{onTextLeftClick->Belt_Option.map( onTextLeftClick => () => onTextLeftClick(i) )},
                    ~onAltLeftClick = ?{onTextAltLeftClick->Belt_Option.map( onTextAltLeftClick => 
                        () => onTextAltLeftClick(i)
                    )},
                    ~longClickEnabled,
                    ~cursor,
                    ~symRename?,
                    ()
                )
            })->React.array
        }
        | Tree({exprTyp, root}) => {
            let (clickedId,selectedIds) = getIdsOfSelectedNodes(stmtCont)
            let elems = []
            elems->Js.Array2.push(
                rndSymbol(
                    ~isFirst=true,
                    ~key="expr-type",
                    ~sym=exprTyp,
                    ~color=None,
                    ~cursor,
                    ~symRename?,
                    ()
                )
            )->ignore
            Expln_utils_data.traverseTree(
                ref(false),
                Subtree(root),
                (_, node) => {
                    switch node {
                        | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                        | Symbol(_) => None
                    }
                },
                ~process = (selectionIsOn, node) => {
                    switch node {
                        | Subtree(_) => ()
                        | Symbol({id, sym, color}) => {
                            let symbolIsHighlighted = selectedIds->Belt_SetInt.has(id)
                            elems->Js.Array2.push(
                                rndSymbol(
                                    ~isFirst=false,
                                    ~key=id->Belt.Int.toString,
                                    ~sym,
                                    ~color,
                                    ~onLeftClick = ?{onTreeLeftClick->Belt_Option.map( onTreeLeftClick => 
                                        () => onTreeLeftClick(id) 
                                    )},
                                    ~onAltLeftClick = ?{onTreeAltLeftClick->Belt_Option.map( onTreeAltLeftClick => 
                                        () => onTreeAltLeftClick(id)
                                    )},
                                    ~longClickEnabled,
                                    ~spaceBackgroundColor=?{
                                        if (renderSelection && symbolIsHighlighted && selectionIsOn.contents) {
                                            Some("#ADD6FF")
                                        } else {
                                            None
                                        } 
                                    },
                                    ~symbolBackgroundColor=?{ 
                                        if (renderSelection && symbolIsHighlighted) {
                                            if (id == clickedId) {
                                                Some("#99bce0")
                                            } else {
                                                Some("#ADD6FF")
                                            }
                                        } else {
                                            None
                                        } 
                                    },
                                    ~cursor,
                                    ~symRename?,
                                    ()
                                )
                            )->ignore
                            selectionIsOn := symbolIsHighlighted
                        }
                    }
                    None
                },
                ()
            )->ignore
            elems->React.array
        }
    }
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->Js_string2.replaceByRe(symbolsNotAllowedInLabelRegex, "")

let rndProofStatus = (
    ~proofStatus:option<proofStatus>,
    ~readyTooltip:option<string>=?,
    ~waitingTooltip:option<string>=?,
    ~noJstfTooltip:option<string>=?,
    ~jstfIsIncorrectTooltip:option<string>=?,
    ~onReadyIconClicked:option<unit=>unit>=?,
    ~onErrorIconClicked:option<unit=>unit>=?,
    ~onNoJstfIconClicked:option<unit=>unit>=?,
    ()
):React.element => {
    let commonStyle = ReactDOM.Style.make(
        ~fontWeight="bold", ~width="13px", ~display="inline-block",
        ()
    )
    switch proofStatus {
        | None => 
            <span 
                style=commonStyle
            >{React.string(Common.nbsp)}</span>
        | Some(status) => {
            switch status {
                | Ready =>
                    <span 
                        title=?readyTooltip
                        style={commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                            ~color="green",
                            ~cursor=if (onReadyIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        ))}
                        onClick={_=>onReadyIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >{React.string("\u2713")}</span>
                | Waiting =>
                    <span 
                        title=?waitingTooltip
                        style={commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                            ~color="orange",
                            ()
                        ))}
                    >{React.string("\u223F")}</span>
                | NoJstf =>
                    <span 
                        title=?noJstfTooltip
                        style={commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                            ~color="red",
                            ~cursor=if (onNoJstfIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        ))}
                        onClick={_=>onNoJstfIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >{React.string("?")}</span>
                | JstfIsIncorrect =>
                    <span 
                        title=?jstfIsIncorrectTooltip
                        style={commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                            ~color="red",
                            ~cursor=if (onErrorIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        ))}
                        onClick={_=>onErrorIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >{React.string("\u2717")}</span>
            }
        }
    }
}

module VisualizedJstf = {
    type props = {
        wrkCtx:mmContext,
        proofTreeDto:proofTreeDto,
        jstfText:string,
        src:exprSrcDto,
        typeColors:Belt_HashMapString.t<string>,
        preCtxColors:Belt_HashMapString.t<string>,
        wrkCtxColors:Belt_HashMapString.t<string>,
    }
    let make = React.memoCustomCompareProps( ({
        wrkCtx,
        proofTreeDto,
        jstfText,
        src,
        typeColors,
        preCtxColors,
        wrkCtxColors,
    }:props) => {
        switch parseJstf(jstfText) {
            | Error(_) | Ok(None) => React.null
            | Ok(Some(jstf)) => {
                switch src {
                    | VarType | Hypothesis(_) | AssertionWithErr(_) => React.null
                    | Assertion({args, label}) => {
                        switch wrkCtx->getFrame(label) {
                            | None => React.null
                            | Some(frame) => {
                                let asrt = wrkCtx->frmIntsToSymsExn(frame, frame.asrt)
                                let hyps = []
                                let subs = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                let frmColors = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                frame.hyps->Js.Array2.forEachi((hyp,i) => {
                                    if (hyp.typ == E) {
                                        hyps->Js.Array2.push(wrkCtx->frmIntsToSymsExn(frame, hyp.expr))->ignore
                                    } else {
                                        let frmSym = wrkCtx->frmIntToSymExn(frame, hyp.expr[1])
                                        subs->Belt_HashMapString.set(
                                            frmSym,
                                            wrkCtx->ctxIntsToSymsExn( 
                                                proofTreeDto.nodes[args[i]].expr->Js_array2.sliceFrom(1) 
                                            )
                                        )
                                        let typeSym = wrkCtx->ctxIntToSymExn(hyp.expr[0])
                                        switch typeColors->Belt_HashMapString.get(typeSym) {
                                            | None => ()
                                            | Some(color) => frmColors->Belt_HashMapString.set( frmSym, color )
                                        }
                                    }
                                })
                                <MM_cmp_jstf_to_svg
                                    hyps
                                    hypLabels=jstf.args
                                    asrt
                                    frmColors=Some(frmColors)
                                    ctxColors1=Some(preCtxColors)
                                    ctxColors2=Some(wrkCtxColors)
                                    subs
                                />
                            }
                        }
                    }
                }
            }
        }
    }, (_,_) => true )
}

type props = {
    modalRef:modalRef,

    settingsVer:int,
    preCtxVer:int,
    varsText:string,
    wrkCtx:option<mmContext>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,
    parensMap:Belt_HashMapString.t<string>,
    typeColors:Belt_HashMapString.t<string>,
    preCtxColors:Belt_HashMapString.t<string>,
    wrkCtxColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    longClickEnabled:bool,
    defaultStmtType:string,

    visualizationIsOn:bool,

    stmt:userStmt, 
    onLabelEditRequested:unit=>unit, 
    onLabelEditDone:string=>unit, 
    onLabelEditCancel:string=>unit,
    onTypEditRequested:unit=>unit, 
    onTypEditDone:userStmtType=>unit,
    onContEditRequested:unit=>unit, 
    onContEditDone:string=>unit, 
    onContEditCancel:string=>unit,
    onSyntaxTreeUpdated:stmtCont=>unit,
    onJstfEditRequested:unit=>unit, 
    onJstfEditDone:string=>unit, 
    onJstfEditCancel:string=>unit,

    onGenerateProof:unit=>unit,
    onDebug:unit=>unit,
    addStmtAbove:string=>unit,
    addStmtBelow:string=>unit,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.preCtxVer == b.preCtxVer
    && a.varsText == b.varsText
    && a.visualizationIsOn == b.visualizationIsOn

    && a.stmt.label == b.stmt.label
    && a.stmt.labelEditMode == b.stmt.labelEditMode
    && a.stmt.typ == b.stmt.typ
    && a.stmt.typEditMode == b.stmt.typEditMode
    && a.stmt.cont === b.stmt.cont
    && a.stmt.contEditMode == b.stmt.contEditMode
    && a.stmt.jstfText == b.stmt.jstfText
    && a.stmt.jstfEditMode == b.stmt.jstfEditMode

    && a.stmt.src == b.stmt.src
    && a.stmt.proofStatus === b.stmt.proofStatus
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    wrkCtx,
    frms,
    parenCnt,
    syntaxTypes,
    parensMap,
    stmt,
    onLabelEditRequested,
    onLabelEditDone,
    onLabelEditCancel,
    onTypEditRequested,
    onTypEditDone,
    onContEditRequested,
    onContEditDone,
    onContEditCancel,
    onSyntaxTreeUpdated,
    onJstfEditRequested,
    onJstfEditDone,
    onJstfEditCancel,
    onGenerateProof,
    onDebug,
    typeColors,
    preCtxColors,
    wrkCtxColors,
    visualizationIsOn,
    editStmtsByLeftClick,
    longClickEnabled,
    defaultStmtType,
    addStmtAbove,
    addStmtBelow,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState())
    let labelRef = React.useRef(Js.Nullable.null)
    let stmtTextFieldRef = React.useRef(Js.Nullable.null)

    let (syntaxTreeWasRequested, setSyntaxTreeWasRequested) = React.useState(() => None)
    let (syntaxTreeError, setSyntaxTreeError) = React.useState(() => None)
    let (selectionRange, setSelectionRange) = React.useState(() => None)
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)
    let (newTextCursorPosition, setNewTextCursorPosition) = React.useState(() => None)

    React.useEffect1(() => {
        if (stmt.labelEditMode) {
            setState(setNewText(_,stmt.label))
        } else if (stmt.typEditMode) {
            setState(setNewText(_,stmt.typ->userStmtTypeToStr))
        } else if (stmt.contEditMode) {
            let contStr = stmt.cont->contToStr
            let contStr =
                if (contStr == "" && defaultStmtType != "") {
                    defaultStmtType ++ " "
                } else {
                    contStr
                }
            setState(setNewText(_,contStr))
        } else if (stmt.jstfEditMode) {
            setState(setNewText(_,stmt.jstfText))
        }
        None
    }, [stmt.labelEditMode, stmt.typEditMode, stmt.contEditMode, stmt.jstfEditMode])

    React.useEffect1(() => {
        switch syntaxTreeError {
            | None => ()
            | Some(msg) => {
                setSyntaxTreeError(_ => None)
                openInfoDialog( ~modalRef, ~text=msg, () )
            }
        }
        None
    }, [syntaxTreeError])

    React.useEffect1(() => {
        switch stmtTextFieldRef.current->Js.Nullable.toOption {
            | None => ()
            | Some(domElem) => {
                switch selectionRange {
                    | None => ()
                    | Some((f,t)) => {
                        setSelectionRange(_ => None)
                        ReactDOM.domElementToObj(domElem)["setSelectionRange"](. f, t)
                    }
                }
            }
        }
        None
    }, [stmtTextFieldRef.current])

    React.useEffect1(() => {
        switch newTextCursorPosition {
            | None => ()
            | Some(newTextCursorPosition) => {
                setNewTextCursorPosition(_ => None)
                switch stmtTextFieldRef.current->Js.Nullable.toOption {
                    | None => ()
                    | Some(domElem) => {
                        let input = ReactDOM.domElementToObj(domElem)
                        input["selectionStart"]=newTextCursorPosition
                        input["selectionEnd"]=newTextCursorPosition
                    }
                }
            }
        }
        None
    }, [newTextCursorPosition])

    let actBuildSyntaxTree = (clickedIdx:int):unit => {
        switch wrkCtx {
            | None => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because there was an error setting MM context.`))
            | Some(wrkCtx) => {
                switch stmt.cont {
                    | Tree(_) => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because stmtCont is a tree.`))
                    | Text(syms) => {
                        switch textToSyntaxTree( 
                            ~wrkCtx, ~syms, ~syntaxTypes, ~frms, ~parenCnt,
                            ~lastSyntaxType=getLastSyntaxType(),
                            ~onLastSyntaxTypeChange=setLastSyntaxType,
                        ) {
                            | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                            | Ok(syntaxTree) => {
                                onSyntaxTreeUpdated(Tree({
                                    exprTyp:syms[0].sym, 
                                    root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors, ~wrkCtxColors, () ), 
                                    clickedNodeId:getNodeIdBySymIdx(~tree=syntaxTree, ~symIdx=clickedIdx),
                                    expLvl:0,
                                }))
                            }
                        }
                    }
                }
            }
        }
    }

    React.useEffect1(() => {
        switch syntaxTreeWasRequested {
            | None => ()
            | Some(clickedIdx) => {
                setTimeout(
                    () => {
                        setSyntaxTreeWasRequested(_ => None)
                        actBuildSyntaxTree(clickedIdx)
                    },
                    0
                )->ignore
            }
        }
        None
    }, [syntaxTreeWasRequested])

    let actToggleInfoExpanded = () => {
        setState(st => setInfoExpanded(st, !st.infoExpanded))
    }

    let actExpandProof = expanded => {
        setState(st => setInfoExpanded(st, expanded))
    }

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }

    let before = (str:string, pos:int):string => str->Js.String2.substring(~from=0,~to_=pos)
    let after = (str:string, pos:int):string => str->Js.String2.substringToEnd(~from=pos+1)

    let getLastSymbol = (str:string):string => {
        switch str->Js_string2.lastIndexOf(" ") {
            | -1 => str
            | idx => str->Js_string2.substringToEnd(~from=idx+1)
        }
    }

    let actStmtContentUpdated = (newText:string,selectionStart:int):unit => {
        let prevLen = state.newText->Js.String2.length
        let newLen = newText->Js.String2.length
        let newText = 
            if (prevLen + 1 == newLen && selectionStart > 1 && " " == newText->Js.String2.charAt(selectionStart - 1)) {
                let pos = selectionStart-1
                let prevBefore = state.newText->before(pos)
                let newBefore = newText->before(pos)
                let prevAfter = state.newText->after(pos-1)
                let newAfter = newText->after(pos)
                if ( prevBefore == newBefore && prevAfter == newAfter ) {
                    let lastSymbol = getLastSymbol(newBefore)
                    switch parensMap->Belt_HashMapString.get(lastSymbol) {
                        | None => newText
                        | Some(closingParen) => {
                            let newText = newBefore ++ "  " ++ closingParen ++ newAfter
                            setNewTextCursorPosition(_ => Some(selectionStart))
                            newText
                        }
                    }
                } else {
                    newText
                }
            } else {
                newText
            }
        actNewTextUpdated(newText)
    }
    
    let actLabelEditDone = () => {
        onLabelEditDone(state.newText->removeSymbolsNotAllowedInLabel)
    }
    
    let actLabelEditCancel = () => {
        onLabelEditCancel(state.newText->removeSymbolsNotAllowedInLabel)
    }
    
    let actTypEditDone = newTypStr => {
        onTypEditDone(userStmtTypeFromStr(newTypStr))
    }
    
    let actContEditDone = () => {
        onContEditDone(state.newText->Js_string2.trim)
    }
    
    let actContEditCancel = () => {
        onContEditCancel(state.newText->Js_string2.trim)
    }
    
    let actJstfEditDone = () => {
        actExpandProof(true)
        onJstfEditDone(state.newText->Js_string2.trim)
    }
    
    let actJstfEditCancel = () => {
        onJstfEditCancel(state.newText->Js_string2.trim)
    }
    
    let actJstfDeleted = () => {
        onJstfEditDone("")
    }

    let actUpdateSyntaxTree = (update:stmtContTreeData=>stmtContTreeData):unit => {
        switch stmt.cont {
            | Text(_) => ()
            | Tree(treeData) => {
                onSyntaxTreeUpdated(Tree(treeData->update))
            }
        }
    }

    let actTreeNodeClicked = (nodeId) => {
        actUpdateSyntaxTree(treeData => {...treeData, clickedNodeId:Some(nodeId), expLvl:0})
    }

    let actUnselect = () => {
        actUpdateSyntaxTree(treeData => {...treeData, clickedNodeId:None})
    }

    let actExpandSelection = () => {
        actUpdateSyntaxTree(updateExpLevel(_,true))
    }

    let actShrinkSelection = () => {
        actUpdateSyntaxTree(updateExpLevel(_,false))
    }

    let actAddStmtAbove = () => {
        stmt.cont->getSelectedText->Belt.Option.forEach(selectedText => {
            switch stmt.cont {
                | Text(_) => ()
                | Tree({exprTyp}) => {
                    addStmtAbove(exprTyp ++ " " ++ selectedText)
                }
            }
        })
    }

    let actAddStmtBelow = () => {
        stmt.cont->getSelectedText->Belt.Option.forEach(selectedText => {
            switch stmt.cont {
                | Text(_) => ()
                | Tree({exprTyp}) => {
                    addStmtBelow(exprTyp ++ " " ++ selectedText)
                }
            }
        })
    }

    let actCopyToClipboard = () => {
        switch getSelectedSymbols(stmt.cont) {
            | None => ()
            | Some(syms) => {
                copyToClipboard(syms->Js_array2.joinWith(" "))
                setCopiedToClipboard(timerId => {
                    switch timerId {
                        | None => ()
                        | Some(timerId) => clearTimeout(timerId)
                    }
                    Some(setTimeout(
                        () => setCopiedToClipboard(_ => None),
                        1000
                    ))
                })
            }
        }
    }

    let getSelectedRange = ():option<(int,int)> => {
        switch stmt.cont {
            | Text(_) => None
            | Tree({exprTyp, root}) => {
                let (_,selectedIds) = getIdsOfSelectedNodes(stmt.cont)
                let idxFrom = ref(None)
                let idxTo = ref(None)
                Expln_utils_data.traverseTree(
                    (ref(false),ref(exprTyp->Js_string2.length)),
                    Subtree(root),
                    (_, node) => {
                        switch node {
                            | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                            | Symbol(_) => None
                        }
                    },
                    ~process = ((selectionIsOn,charsPassed), node) => {
                        switch node {
                            | Subtree(_) => ()
                            | Symbol({id,sym}) => {
                                let symbolIsHighlighted = selectedIds->Belt_SetInt.has(id)
                                if (selectionIsOn.contents && !symbolIsHighlighted) {
                                    idxTo := Some(charsPassed.contents)
                                }
                                if (!(selectionIsOn.contents) && symbolIsHighlighted) {
                                    idxFrom := Some(charsPassed.contents + 1)
                                }
                                charsPassed := charsPassed.contents + 1 + sym->Js_string2.length
                                selectionIsOn := symbolIsHighlighted
                            }
                        }
                        idxTo.contents
                    },
                    ()
                )->ignore
                switch (idxFrom.contents, idxTo.contents) {
                    | (Some(idxFrom), None) => Some((idxFrom, stmt.cont->contToStr->Js.String2.length))
                    | (Some(idxFrom), Some(idxTo)) => Some((idxFrom, idxTo))
                    | _ => None
                }
            }
        }
    }

    let actEditSelection = () => {
        setSelectionRange(_ => getSelectedRange())
        onContEditRequested()
    }

    let rndLabel = () => {
        if (stmt.labelEditMode) {
            <Row>
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width="100px", ())
                    autoFocus=true
                    value=state.newText
                    onChange=evt2str(str => actNewTextUpdated(str->removeSymbolsNotAllowedInLabel))
                    onKeyDown=kbrdHnd(~onEnter=actLabelEditDone, ~onEsc=actLabelEditCancel, ())
                    title="Enter to save, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  
                    ~onClick=actLabelEditDone, ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,
                    ~onClick=actLabelEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            <span 
                ref=ReactDOM.Ref.domRef(labelRef)
                onClick=clickHnd(~act=onLabelEditRequested, ()) 
                title="<left-click> to change"
                style=ReactDOM.Style.make(~overflowWrap="normal", ~whiteSpace="nowrap", ())
            >
                {React.string(stmt.label)}
            </span>
        }
    }

    let rndSelectionButtons = () => {
        <Row alignItems=#center>
            <ButtonGroup variant=#outlined size=#small >
                <Button title="Expand selection" onClick={_=>actExpandSelection()}> <MM_Icons.ZoomOutMap/> </Button>
                <Button title="Shrink selection" onClick={_=>actShrinkSelection()}> <MM_Icons.ZoomInMap/> </Button>
                <Button title="Add new statement above" onClick={_=>actAddStmtAbove()}> 
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />
                </Button>
                <Button title="Add new statement below" onClick={_=>actAddStmtBelow()}> 
                    <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />
                </Button>
                <Button title="Copy to the clipboard" onClick={_=>actCopyToClipboard()}> <MM_Icons.ContentCopy/> </Button>
                <Button title="Edit" onClick={_=>actEditSelection()}> <MM_Icons.Edit/> </Button>
                <Button title="Unselect" onClick={_=>actUnselect()}> <MM_Icons.CancelOutlined/> </Button>
            </ButtonGroup>
            {
                if (copiedToClipboard->Belt.Option.isSome) {
                    React.string("Copied to the clipboard.")
                } else {React.null}
            }
        </Row>
    }

    let rndCont = () => {
        if (stmt.contEditMode) {
            let windowWidth = window["innerWidth"]
            let labelWidth = switch labelRef.current->Js.Nullable.toOption {
                | None => 0
                | Some(domElem) => ReactDOM.domElementToObj(domElem)["offsetWidth"]
            }
            <Row>
                <TextField
                    inputRef=ReactDOM.Ref.domRef(stmtTextFieldRef)
                    size=#small
                    style=ReactDOM.Style.make(
                        ~width = (windowWidth - 215 - labelWidth)->Belt_Int.toString ++ "px", 
                        ()
                    )
                    inputProps={
                        "style": {"fontFamily": "monospace"}
                    }
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange={evt => {
                        let selectionStart = (evt->ReactEvent.Form.target)["selectionStart"]
                        let value = (evt->ReactEvent.Form.target)["value"]
                        actStmtContentUpdated(value, selectionStart)
                    }}
                    onKeyDown=kbrdHnd(~onEnter=actContEditDone, ~onEsc=actContEditCancel, ())
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->Js.String2.trim != "",  
                    ~onClick=actContEditDone, ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,  
                    ~onClick=actContEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            let textIsSelected = switch stmt.cont {
                | Text(_) => false
                | Tree({clickedNodeId}) => clickedNodeId->Belt.Option.isSome
            }
            let elems = [
                <Paper 
                    onClick={
                        if (editStmtsByLeftClick) {
                            clickHnd(~act=onContEditRequested, ())
                        } else {
                            clickHnd(~alt=true, ~act=onContEditRequested, ())
                        }
                    }
                    style=ReactDOM.Style.make(
                        ~padding="1px 10px", 
                        ~backgroundColor=
                            if (stmt.stmtErr->Belt_Option.isSome || stmt.syntaxErr->Belt_Option.isSome) {
                                "rgb(255,230,230)"
                            } else {"rgb(255,255,235)"}, 
                        ~fontFamily="monospace",
                        ~fontSize="1.3em",
                        ()
                    ) 
                    title={
                        if (editStmtsByLeftClick) {
                            "<left-click> to change, Alt+<left-click> to select"
                        } else {
                            "Alt + <left-click> to change, <left-click> to select"
                        }
                    }
                >
                    {
                        let onTextLeftClick = if (editStmtsByLeftClick) {
                            _ => onContEditRequested()
                        } else {
                            idx => setSyntaxTreeWasRequested(_ => Some(idx))
                        }
                        let onTextAltLeftClick = if (!editStmtsByLeftClick) {
                            _ => onContEditRequested()
                        } else {
                            idx => setSyntaxTreeWasRequested(_ => Some(idx))
                        }
                        let onTreeLeftClick = if (editStmtsByLeftClick) {
                            _ => onContEditRequested()
                        } else {
                            actTreeNodeClicked
                        }
                        let onTreeAltLeftClick = if (!editStmtsByLeftClick) {
                            _ => onContEditRequested()
                        } else {
                            actTreeNodeClicked
                        }
                        rndContText(
                            ~stmtCont=stmt.cont, 
                            ~onTextLeftClick,
                            ~onTextAltLeftClick,
                            ~onTreeLeftClick,
                            ~onTreeAltLeftClick,
                            ~longClickEnabled,
                            ~renderSelection=true,
                            ~cursor = if (editStmtsByLeftClick) {"auto"} else {"pointer"},
                            ()
                        )
                    }
                </Paper>
            ]
            if (syntaxTreeWasRequested->Belt.Option.isSome) {
                elems->Js_array2.push(
                    <span> {"Building a syntax tree..."->React.string} </span>
                )->ignore
            }
            if (textIsSelected) {
                elems->Js_array2.push(
                    rndSelectionButtons()
                )->ignore
            }

            <Col>
                {elems->React.array}
            </Col>
        }
    }

    let rndTyp = () => {
        if (stmt.typEditMode) {
            <FormControl size=#small >
                <Select
                    value=""
                    onChange=evt2str(actTypEditDone)
                >
                    <MenuItem value="e">{React.string("H")}</MenuItem>
                    <MenuItem value="p">{React.string("P")}</MenuItem>
                </Select>
            </FormControl>
        } else {
            let typStr = switch stmt.typ {
                | E => "H"
                | P => "P"
            }
            <LongClickSpan
                onClick=clickHnd2(
                    clickClbkMake(~alt=true, ~act=onTypEditRequested, ()),
                    clickClbkMake(~act=actToggleInfoExpanded, ()),
                )
                longClickEnabled=true
                onShortClick = {_ => actToggleInfoExpanded()}
                onLongClick=onTypEditRequested
                style=ReactDOM.Style.make(~cursor="pointer", ~fontWeight="bold", ())
                title="Alt+<left-click> to change statement type between P (provable) and H (hypothesis). Alt is sometimes labelled Opt. Left-click to show/hide the justification for provable."
            >
                {React.string(typStr->Js_string2.toUpperCase)}
            </LongClickSpan>
        }
    }

    let rndJstf = () => {
        if (stmt.jstfEditMode) {
            <Row>
                <TextField
                    size=#small
                    label="Justification"
                    style=ReactDOM.Style.make(~width="600px", ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd(~onEnter=actJstfEditDone, ~onEsc=actJstfEditCancel, ())
                    title="Enter to save, Esc to cancel"
                />
                {rndIconButton(~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actJstfEditDone,
                    ~title="Save, Enter", ())}
                {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,  
                    ~onClick=actJstfEditCancel, ~title="Cancel, Esc", ~color=None, ())}
            </Row>
        } else {
            let jstfText = if (stmt.jstfText == "") { " " } else { stmt.jstfText }
            let padding = if (jstfText->Js_string2.trim == "") { "10px 30px" } else { "3px" }
            <Row >
                <Paper 
                    onClick=clickHnd(~act=onJstfEditRequested, ()) 
                    style=ReactDOM.Style.make( ~padding, ~marginTop="5px", () )
                    title="<left-click> to change"
                >
                    {React.string(jstfText)}
                </Paper>
                {
                    if (jstfText->Js_string2.trim == "") {
                        React.null
                    } else {
                        <span>
                            {rndIconButton(~icon=<MM_Icons.DeleteForever/>,
                                ~onClick=actJstfDeleted, ~title="Clear", ~color=None, ())}
                        </span>
                    }
                }
            </Row>
        }
    }

    let rndJstfVisualization = () => {
        if (
            visualizationIsOn 
            && wrkCtx->Belt.Option.isSome
            && stmt.proofTreeDto->Belt.Option.isSome
            && stmt.src->Belt.Option.isSome
        ) {
                <VisualizedJstf
                    wrkCtx={wrkCtx->Belt_Option.getExn}
                    proofTreeDto={stmt.proofTreeDto->Belt_Option.getExn}
                    jstfText=stmt.jstfText
                    src={stmt.src->Belt_Option.getExn}
                    typeColors
                    preCtxColors
                    wrkCtxColors
                />
        } else {
            React.null
        }
    }

    let rndInfoBody = () => {
        if (stmt.typ == P) {
            if (state.infoExpanded || stmt.jstfEditMode) {
                <Col>
                    {rndJstf()}
                    {rndJstfVisualization()}
                </Col>
            } else {
                React.null
            }
        } else {
            React.null
        }
    }

    <table 
        style=ReactDOM.Style.make(
            ~margin="-2px", 
            ~cursor=if (syntaxTreeWasRequested->Belt.Option.isSome) {"wait"} else {""}, 
            ()
        )>
        <tbody>
            <tr style=ReactDOM.Style.make(~verticalAlign="top", ())>
                <td>
                    {
                        rndProofStatus(
                            ~proofStatus=stmt.proofStatus, 
                            ~readyTooltip="Proof is ready, left-click to generate compressed proof",
                            ~waitingTooltip="Justification for this statement is correct",
                            ~noJstfTooltip="Justification cannot be determined automatically. Click to debug.",
                            ~jstfIsIncorrectTooltip="Justification is incorrect. Click to debug.",
                            ~onReadyIconClicked=onGenerateProof,
                            ~onErrorIconClicked=onDebug,
                            ~onNoJstfIconClicked=onDebug,
                            ()
                        )
                    }
                </td>
                <td>
                    {rndLabel()}
                </td>
                <td>
                    {rndTyp()}
                </td>
                <td>
                    {rndCont()}
                </td>
            </tr>
            <tr>
                <td>
                    {React.null}
                </td>
                <td>
                    {React.null}
                </td>
                <td>
                    {React.null}
                </td>
                <td>
                    {rndInfoBody()}
                </td>
            </tr>
        </tbody>
    </table>
}, propsAreSame)
