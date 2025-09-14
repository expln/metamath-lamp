open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_wrk_settings
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open MM_proof_tree_dto
open Expln_React_Modal
open Local_storage_utils
open Common
open Expln_utils_promise

@val external window: {..} = "window"

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool=true, 
    ~title:option<string>=?, 
    ~color:option<string>=Some("primary"),
    ~key:option<string>=?
) => {
    <span ?title>
        <IconButton ?key disabled={!active} onClick={_ => onClick()} ?color> icon </IconButton>
    </span>
}

type state = {
    newText: string,
    infoExpanded: bool,
    visExpanded: bool,
}

type viewOptions = {
    showCheckbox:bool,
    showLabel:bool,
    showType:bool,
    showJstf:bool,
    inlineMode:bool,
    smallBtns:bool,
}

let makeInitialState = () => {
    {
        newText: "",
        infoExpanded: false,
        visExpanded: false,
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

let setVisExpanded = (st,visExpanded):state => {
    {
        ...st,
        visExpanded
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
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~symRename:option<Belt_HashMapString.t<string>>=?,
    ~onLeftClick:option<unit=>unit>=?,
    ~onAltLeftClick:option<unit=>unit>=?,
    ~spaceBackgroundColor:option<string>=?,
    ~symbolBackgroundColor:option<string>=?,
    ~isHighlighted:bool=false,
    ~cursor:string="auto",
    ~title:option<string>=?
):reElem => {
    <React.Fragment key>
        {
            if (isFirst) {
                <></>
            } else {
                let style = ReactDOM.Style.make( ~backgroundColor=?spaceBackgroundColor, ~cursor, () )
                if (longClickEnabled) {
                    <LongClickSpan
                        longClickEnabled
                        longClickDelayMs
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
                        style
                        ?title
                    > 
                        {" "->React.string} 
                    </LongClickSpan>
                } else {
                    <span
                        onClick={
                            clickHnd2(
                                clickClbkMake(~act = callbackOpt(onLeftClick)),
                                clickClbkMake(~alt=true, ~act=callbackOpt(onAltLeftClick)),
                            )
                        }
                        style
                        ?title
                    > 
                        {" "->React.string} 
                    </span>
                }
                
            }
        }
        {
            let (color,fontWeight) = switch color {
                | None => ("black","normal")
                | Some(color) => (color,"bold")
            }
            let style = ReactDOM.Style.make( 
                ~color, ~fontWeight, ~backgroundColor=?symbolBackgroundColor, ~cursor, 
                ~boxShadow=?(isHighlighted?Some("0 0 2px 1px orange"):None),
                () 
            ) 
            if (longClickEnabled) {
                <LongClickSpan 
                    longClickEnabled
                    longClickDelayMs
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
                    style
                    ?title
                >
                    {
                        React.string(
                            symRename->Belt_Option.flatMap(Belt_HashMapString.get(_, sym))->Belt.Option.getWithDefault(sym)
                        )
                    }
                </LongClickSpan>
            } else {
                <span 
                    onClick={
                        clickHnd2(
                            clickClbkMake(~act = callbackOpt(onLeftClick)),
                            clickClbkMake(~alt=true, ~act=callbackOpt(onAltLeftClick)),
                        )
                    }
                    style
                    ?title
                >
                    {
                        React.string(
                            symRename->Belt_Option.flatMap(Belt_HashMapString.get(_, sym))->Belt.Option.getWithDefault(sym)
                        )
                    }
                </span>
            }
            
        }
    </React.Fragment>
}

let rndContText = (
    ~stmtCont:stmtCont,
    ~symRename:option<Belt_HashMapString.t<string>>=?,
    ~symsToHighlight:option<array<int>>=?,
    ~onTextLeftClick:option<int=>unit>=?,
    ~onTextAltLeftClick:option<int=>unit>=?,
    ~onTreeLeftClick:option<int=>unit>=?,
    ~onTreeAltLeftClick:option<int=>unit>=?,
    ~longClickEnabled:bool=false,
    ~longClickDelayMs:int=0,
    ~cursor:string="auto",
    ~renderSelection:bool=false,
    ~title:option<string>=?
):React.element => {
    switch stmtCont {
        | Text({syms}) => {
            let syms = 
                if (syms->Array.length == 0) {
                    [{sym:nbsp,color:None},{sym:nbsp,color:None},{sym:nbsp,color:None}]
                } else {
                    syms
                }
            syms->Array.mapWithIndex((stmtSym,i) => {
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
                    ~longClickDelayMs,
                    ~cursor,
                    ~symRename?,
                    ~isHighlighted=symsToHighlight->Option.mapOr(false, idxs=>idxs->Array.includes(i)),
                    ~title?
                )
            })->React.array
        }
        | Tree({exprTyp, root}) => {
            let (clickedId,selectedIds) = getIdsOfSelectedNodes(stmtCont)
            let elems = []
            elems->Array.push(
                rndSymbol(
                    ~isFirst=true,
                    ~key="expr-type",
                    ~sym=exprTyp,
                    ~color=None,
                    ~cursor,
                    ~symRename?,
                    ~longClickEnabled=false,
                    ~longClickDelayMs=0,
                    ~title?
                )
            )
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
                            let symbolIsSelected = selectedIds->Belt_SetInt.has(id)
                            let symIdx = elems->Array.length
                            elems->Array.push(
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
                                    ~longClickDelayMs,
                                    ~spaceBackgroundColor=?{
                                        if (renderSelection && symbolIsSelected && selectionIsOn.contents) {
                                            Some("#ADD6FF")
                                        } else {
                                            None
                                        } 
                                    },
                                    ~symbolBackgroundColor=?{ 
                                        if (renderSelection && symbolIsSelected) {
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
                                    ~isHighlighted=
                                        symsToHighlight->Option.mapOr(false, idxs=>idxs->Array.includes(symIdx)),
                                    ~title?
                                )
                            )
                            selectionIsOn := symbolIsSelected
                        }
                    }
                    None
                }
            )->ignore
            elems->React.array
        }
    }
}

let symbolsNotAllowedInLabelRegex = %re("/[\s:]+/g")
let removeSymbolsNotAllowedInLabel = str => str->String.replaceRegExp(symbolsNotAllowedInLabelRegex, "")

let stmtPartMarginLeft = "10px"
let stmtPartMarginTopInt = 5
let stmtPartMarginTop = stmtPartMarginTopInt->Belt.Int.toString ++ "px"

let checkMarkSymbol = "\u2713"

let rndProofStatus = (
    ~proofStatus:option<proofStatus>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~readyTooltip:option<string>=?,
    ~waitingTooltip:option<string>=?,
    ~noJstfTooltip:option<string>=?,
    ~jstfIsIncorrectTooltip:option<string>=?,
    ~onReadyIconClicked:option<unit=>unit>=?,
    ~onReadyIconAltClicked:option<unit=>unit>=?,
    ~onWaitingIconClicked:option<unit=>unit>=?,
    ~onErrorIconClicked:option<unit=>unit>=?,
    ~onNoJstfIconClicked:option<unit=>unit>=?
):React.element => {
    let commonStyle = ReactDOM.Style.make(
        ~fontWeight="bold", 
        ~width="13px", 
        ~display="inline-block",
        ~marginLeft=stmtPartMarginLeft, 
        ~marginTop=stmtPartMarginTop, 
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
                    let style = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                        ~color="green",
                        ~cursor=
                            if (onReadyIconClicked->Belt_Option.isSome || onReadyIconAltClicked->Belt_Option.isSome) {
                                "pointer"
                            } else {
                                "default"
                            }, 
                        ()
                    ))
                    switch onReadyIconAltClicked {
                        | None => {
                            <span 
                                title=?readyTooltip
                                style
                                onClick={_=>callbackOpt(onReadyIconClicked)()}
                            >{React.string(checkMarkSymbol)}</span>
                        }
                        | Some(onReadyIconAltClicked) => {
                            <LongClickSpan
                                onClick={
                                    clickHnd2(
                                        clickClbkMake(~act = callbackOpt(onReadyIconClicked)),
                                        clickClbkMake(~alt=true, ~act=onReadyIconAltClicked),
                                    )
                                }
                                longClickEnabled
                                longClickDelayMs
                                onShortClick={
                                    (clickAttrs:option<UseLongClick.clickAttrs>) => {
                                        switch clickAttrs {
                                            | None => callbackOpt(onReadyIconClicked)()
                                            | Some({alt}) => {
                                                if (alt) {
                                                    onReadyIconAltClicked()
                                                } else {
                                                    callbackOpt(onReadyIconClicked)()
                                                }
                                            }
                                        }
                                    }
                                }
                                onLongClick=onReadyIconAltClicked

                                title=?readyTooltip
                                style
                            >{React.string(checkMarkSymbol)}</LongClickSpan>
                        }
                    }
                | Waiting =>
                    <span 
                        title=?waitingTooltip
                        style={commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(
                            ~color="orange",
                            ~cursor=if (onWaitingIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        ))}
                        onClick={_=>onWaitingIconClicked->Belt_Option.forEach(clbk => clbk())}
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
                                let subs = Belt_HashMapString.make(~hintSize = frame.hyps->Array.length)
                                let frmColors = Belt_HashMapString.make(~hintSize = frame.hyps->Array.length)
                                frame.hyps->Array.forEachWithIndex((hyp,i) => {
                                    if (hyp.typ == E) {
                                        hyps->Array.push(wrkCtx->frmIntsToSymsExn(frame, hyp.expr))
                                    } else {
                                        let frmSym = wrkCtx->frmIntToSymExn(frame, hyp.expr->Array.getUnsafe(1))
                                        subs->Belt_HashMapString.set(
                                            frmSym,
                                            wrkCtx->ctxIntsToSymsExn( 
                                                (proofTreeDto.nodes->Array.getUnsafe(args->Array.getUnsafe(i))).expr->Array.sliceToEnd(~start=1)
                                            )
                                        )
                                        let typeSym = wrkCtx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(0))
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

let userStmtTypeAndIsGoalFromStr = (stmtTypeStr:string):(userStmtType,bool) => {
    switch stmtTypeStr {
        | "e" => (E,false)
        | "p" => (P,false)
        | "g" => (P,true)
        | _ => raise(MmException({msg:`Cannot convert '${stmtTypeStr}' to userStmtType and isGoal.`}))
    }
}

type props = {
    modalRef:modalRef,

    settingsVer:int,
    settings:settings,
    preCtxVer:int,
    varsText:string,
    wrkCtx:option<mmContext>,
    frms: frms,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,
    parensMap:Belt_HashMapString.t<string>,
    typeColors:Belt_HashMapString.t<string>,
    preCtxColors:Belt_HashMapString.t<string>,
    wrkCtxColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    longClickEnabled:bool,
    longClickDelayMs:int,
    defaultStmtType:string,
    showVisByDefault:bool,

    viewOptions:viewOptions,
    readOnly:bool,
    parenAc:bool,
    toggleParenAc: unit=>unit,

    stmt:userStmt, 
    onLabelEditRequested:unit=>unit, 
    onLabelEditDone:string=>unit, 
    onLabelEditCancel:string=>unit,
    onTypEditRequested:unit=>unit, 
    onTypEditDone:(userStmtType,bool)=>unit,
    onContEditRequested:unit=>unit, 
    onContEditDone:string=>unit, 
    onContEditCancel:string=>unit,
    onSyntaxTreeUpdatedWithoutContentChange:stmtCont=>unit,
    onJstfEditRequested:unit=>unit, 
    onJstfEditDone:string=>unit, 
    onJstfEditCancel:string=>unit,

    checkboxDisabled:bool,
    checkboxChecked:bool,
    checkboxOnChange:(~checked:bool,~shift:bool)=>unit,

    onGenerateProof:unit=>unit,
    onDebug:unit=>unit,
    onOpenSubstitutionDialog:option<React.ref<unit=>unit>>,
    addStmtAbove:string=>unit,
    addStmtBelow:string=>unit,
    setShowTabs:bool=>unit,
    openFrameExplorer:string=>unit,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.settings.customTransforms == b.settings.customTransforms
    && a.preCtxVer == b.preCtxVer
    && a.varsText == b.varsText

    && a.viewOptions.showCheckbox == b.viewOptions.showCheckbox
    && a.viewOptions.showLabel == b.viewOptions.showLabel
    && a.viewOptions.showType == b.viewOptions.showType
    && a.viewOptions.showJstf == b.viewOptions.showJstf
    && a.viewOptions.inlineMode == b.viewOptions.inlineMode
    && a.viewOptions.smallBtns == b.viewOptions.smallBtns
    && a.readOnly == b.readOnly
    && a.parenAc == b.parenAc

    && a.stmt.label == b.stmt.label
    && a.stmt.labelEditMode == b.stmt.labelEditMode
    && a.stmt.typ == b.stmt.typ
    && a.stmt.isGoal == b.stmt.isGoal
    && a.stmt.isBkm == b.stmt.isBkm
    && a.stmt.typEditMode == b.stmt.typEditMode
    && a.stmt.cont === b.stmt.cont
    && a.stmt.contEditMode == b.stmt.contEditMode
    && a.stmt.jstfText == b.stmt.jstfText
    && a.stmt.jstfEditMode == b.stmt.jstfEditMode

    && a.stmt.stmtErr == b.stmt.stmtErr
    && a.stmt.syntaxErr == b.stmt.syntaxErr
    && a.stmt.unifErr == b.stmt.unifErr

    && a.checkboxDisabled == b.checkboxDisabled
    && a.checkboxChecked == b.checkboxChecked


    && a.stmt.src == b.stmt.src
    && a.stmt.proofStatus === b.stmt.proofStatus
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    settings,
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
    onSyntaxTreeUpdatedWithoutContentChange,
    onJstfEditRequested,
    onJstfEditDone,
    onJstfEditCancel,
    onGenerateProof,
    onDebug,
    onOpenSubstitutionDialog,
    checkboxDisabled,
    checkboxChecked,
    checkboxOnChange,
    typeColors,
    preCtxColors,
    wrkCtxColors,
    viewOptions,
    readOnly,
    parenAc,
    toggleParenAc,
    editStmtsByLeftClick,
    longClickEnabled,
    longClickDelayMs,
    defaultStmtType,
    showVisByDefault,
    addStmtAbove,
    addStmtBelow,
    setShowTabs,
    openFrameExplorer,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState())
    let labelRef = React.useRef(Nullable.null)
    let jstfRef = React.useRef(Nullable.null)
    let stmtTextFieldRef = React.useRef(Nullable.null)

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
            setState(st => {
                st->setNewText(
                    switch stmt.typ {
                        | E => defaultJstfForHyp
                        | P => stmt.jstfText
                    }
                )
            })
        }
        None
    }, [stmt.labelEditMode, stmt.typEditMode, stmt.contEditMode, stmt.jstfEditMode])

    React.useEffect1(() => {
        switch syntaxTreeError {
            | None => ()
            | Some(msg) => {
                setSyntaxTreeError(_ => None)
                openInfoDialog( ~modalRef, ~text=msg )
            }
        }
        None
    }, [syntaxTreeError])

    React.useEffect1(() => {
        switch stmtTextFieldRef.current->Nullable.toOption {
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
                switch stmtTextFieldRef.current->Nullable.toOption {
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
                    | Text({text, syms}) => {
                        switch textToSyntaxTree( 
                            ~wrkCtx, ~syms=[syms->Array.map(s => s.sym)->Array.sliceToEnd(_, ~start=1)],
                            ~syntaxTypes, ~frms, 
                            ~frameRestrict=settings.allowedFrms.inSyntax,
                            ~parenCnt,
                            ~lastSyntaxType=getLastSyntaxType(),
                            ~onLastSyntaxTypeChange=setLastSyntaxType,
                        ) {
                            | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                            | Ok(syntaxTrees) => {
                                switch (syntaxTrees->Array.getUnsafe(0)) {
                                    | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                                    | Ok(syntaxTree) => {
                                        let stmtContTreeData = {
                                            text,
                                            exprTyp:(syms->Array.getUnsafe(0)).sym,
                                            root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors, ~wrkCtxColors ), 
                                            clickedNodeId:getNodeIdBySymIdx(~tree=syntaxTree, ~symIdx=clickedIdx)
                                                                ->Belt.Option.map(id => (id,Date.make())),
                                            expLvl:0,
                                        }
                                        onSyntaxTreeUpdatedWithoutContentChange(Tree(stmtContTreeData->incExpLvlIfConstClicked))
                                    }
                                }
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
                    10
                )->ignore
            }
        }
        None
    }, [syntaxTreeWasRequested])

    let actToggleInfoExpanded = () => {
        setState(st => {
            let st = if (st.infoExpanded) {
                setVisExpanded(st, false)
            } else {
                setVisExpanded(st, showVisByDefault)
            }
            setInfoExpanded(st, !st.infoExpanded)
        })
    }

    let actToggleVisExpanded = () => {
        setState(st => setVisExpanded(st, !st.visExpanded))
    }

    let actExpandProof = expanded => {
        setState(st => setInfoExpanded(st, expanded))
    }

    let actNewTextUpdated = newText => {
        setState(setNewText(_, newText))
    }

    let actJstfEditRequested = () => {
        onJstfEditRequested()
    }

    let before = (str:string, pos:int):string => str->String.substring(~start=0,~end=pos)
    let after = (str:string, pos:int):string => str->String.substringToEnd(~start=pos+1)

    let getLastSymbol = (str:string):string => {
        switch str->String.lastIndexOf(" ") {
            | -1 => str
            | idx => str->String.substringToEnd(~start=idx+1)
        }
    }

    let actStmtContentUpdated = (newText:string,selectionStart:int):unit => {
        let prevLen = state.newText->String.length
        let newLen = newText->String.length
        let newText = 
            if (parenAc) {
                if (prevLen + 1 == newLen && selectionStart > 1 && " " == newText->String.charAt(selectionStart - 1)) {
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
        let (newTyp,newIsGoal) = userStmtTypeAndIsGoalFromStr(newTypStr)
        onTypEditDone(newTyp,newIsGoal)
    }

    let actTypEditCancel = () => {
        onTypEditDone(stmt.typ, stmt.isGoal)
    }
    
    let actContEditDone = () => {
        onContEditDone(state.newText->String.trim)
    }
    
    let actContEditCancel = () => {
        onContEditCancel(state.newText->String.trim)
    }
    
    let actJstfEditDone = () => {
        actExpandProof(true)
        onJstfEditDone(state.newText->String.trim)
    }
    
    let actJstfEditCancel = () => {
        onJstfEditCancel(state.newText->String.trim)
    }
    
    let actJstfDeleted = () => {
        onJstfEditDone("")
    }

    let actUpdateSyntaxTreeWithoutContentChange = (update:stmtContTreeData=>stmtContTreeData):unit => {
        switch stmt.cont {
            | Text(_) => ()
            | Tree(treeData) => {
                onSyntaxTreeUpdatedWithoutContentChange(Tree(treeData->update))
            }
        }
    }

    let actTreeNodeClicked = (nodeId) => {
        actUpdateSyntaxTreeWithoutContentChange(treeData => {
            {...treeData, clickedNodeId:Some((nodeId,Date.make())), expLvl:0}->incExpLvlIfConstClicked
        })
    }

    let actUnselect = () => {
        actUpdateSyntaxTreeWithoutContentChange(treeData => {...treeData, clickedNodeId:None})
    }

    let actExpandSelection = () => {
        actUpdateSyntaxTreeWithoutContentChange(updateExpLevel(_,true))
    }

    let actShrinkSelection = () => {
        actUpdateSyntaxTreeWithoutContentChange(updateExpLevel(_,false))
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
        switch getSelectedText(stmt.cont) {
            | None => ()
            | Some(selectedText) => {
                copyToClipboard(selectedText)->promiseMap(_ => {
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
                })->ignore
            }
        }
    }

    // getSelectedRange() - returns what part of a statement is selected as Some((first, last)); range ends just before last
    let getSelectedRange = ():option<(int,int)> => {
        switch stmt.cont {
            | Text(_) => None
            | Tree({exprTyp, root}) => {
                let (_,selectedIds) = getIdsOfSelectedNodes(stmt.cont)
                let idxFrom = ref(None)
                let idxTo = ref(None)
                Expln_utils_data.traverseTree(
                    (ref(false),ref(exprTyp->String.length)),
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
                                charsPassed := charsPassed.contents + 1 + sym->String.length
                                selectionIsOn := symbolIsHighlighted
                            }
                        }
                        idxTo.contents
                    }
                )->ignore
                switch (idxFrom.contents, idxTo.contents) {
                    | (Some(idxFrom), None) => Some((idxFrom, stmt.cont->contToStr->String.length))
                    | (Some(idxFrom), Some(idxTo)) => Some((idxFrom, idxTo))
                    | _ => None
                }
            }
        }
    }

    let replaceSelectionWithNewText = (newText:string):option<string> => {
        switch getSelectedRange() {
            | None => None
            | Some((low, high)) =>
                let currentText = stmt.cont->contToStr
                Some(
                    currentText->String.slice(~start=0, ~end=low)
                        ++ newText
                        ++ currentText->String.sliceToEnd(~start=high)
                )
        }
    }

    let actPasteFromClipboard = () => {
        readFromClipboard()->promiseMap(clipboardContents => {
            clipboardContents->replaceSelectionWithNewText->Belt_Option.forEach(newStmtContent => {
                // Propagate changes to MM_cmp_editor
                onContEditDone(newStmtContent)
            })
        })->ignore
    }

    let actEditSelection = () => {
        setSelectionRange(_ => getSelectedRange())
        onContEditRequested()
    }

    let actOpenFrameExplorer = label => {
        setShowTabs(true)
        openFrameExplorer(label)
    }

    let applyResultOfTransform = (replaceSelection, resultOfTransform, action) => {
        if (replaceSelection) {
            replaceSelectionWithNewText(resultOfTransform)
        } else {
            Some(resultOfTransform)
        }->Belt_Option.forEach(action)
    }

    let actOpenFragmentTransform = () => {
        wrkCtx->Belt_Option.forEach(wrkCtx => {
            let transformsText = if (settings.useDefaultTransforms) {
                if (settings.useCustomTransforms) {
                    [MM_frag_transform_default_script.fragmentTransformsDefaultScript, settings.customTransforms]
                } else {
                    [MM_frag_transform_default_script.fragmentTransformsDefaultScript]
                }
            } else {
                if (settings.useCustomTransforms) {
                    [settings.customTransforms]
                } else {
                    []
                }
            }
            openModal(modalRef, () => React.null)->promiseMap(modalId => {
                updateModal(modalRef, modalId, () => {
                    let closeDialog = ()=>closeModal(modalRef, modalId)
                    <MM_cmp_frag_transform
                        step=stmt
                        ctxConstIntToSymExn={ctxIntToSymExn(wrkCtx, _)}
                        transformsText
                        onCancel=closeDialog
                        onInsertAbove={(replaceSelection, transformedSelectionText) => {
                            applyResultOfTransform(replaceSelection, transformedSelectionText, addStmtAbove)
                            closeDialog()
                        }}
                        onInsertBelow={(replaceSelection, transformedSelectionText) => {
                            applyResultOfTransform(replaceSelection, transformedSelectionText, addStmtBelow)
                            closeDialog()
                        }}
                        onUpdateCurrent={(replaceSelection, transformedSelectionText) => {
                            applyResultOfTransform(replaceSelection, transformedSelectionText, onContEditDone)
                            closeDialog()
                        }}
                    />
                })
            })->ignore
        })
    }

    let actOpenSubstitutionDialog = () => {
        switch onOpenSubstitutionDialog {
            | None => ()
            | Some(onOpenSubstitutionDialog) => onOpenSubstitutionDialog.current()
        }
    }

    let rndLabel = () => {
        if (stmt.labelEditMode) {
            <Col 
                spacing=0.
                style=ReactDOM.Style.make(
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ()
                )
            >
                <TextField
                    size=#small
                    style=ReactDOM.Style.make(~width="150px", ())
                    autoFocus=true
                    value=state.newText
                    onChange=evt2str(str => actNewTextUpdated(str->removeSymbolsNotAllowedInLabel))
                    onKeyDown=kbrdHnd2(
                        kbrdClbkMake(~key=keyEnter, ~act=actLabelEditDone),
                        kbrdClbkMake(~key=keyEsc, ~act=actLabelEditCancel),
                    )
                    title="Enter to save, Esc to cancel"
                />
                <Row>
                    {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->String.trim != "",  
                        ~onClick=actLabelEditDone, ~title="Save, Enter")}
                    {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,
                        ~onClick=actLabelEditCancel, ~title="Cancel, Esc", ~color=None)}
                </Row>
            </Col>
        } else {
            let chgLabelShortcutName = if (longClickEnabled) {"Long click (Alt + Left-click)"} else {"Alt + Left-click"}
            let showJstfShortcutName = if (longClickEnabled) {"Short click (Left-click)"} else {"Left-click"}
            <LongClickSpan
                onClick=clickHnd2(
                    clickClbkMake(~alt=true, ~act=onLabelEditRequested),
                    clickClbkMake(~act=actToggleInfoExpanded),
                )
                longClickEnabled
                longClickDelayMs
                onShortClick = {
                    (clickAttrs:option<UseLongClick.clickAttrs>) => {
                        switch clickAttrs {
                            | Some({alt:true}) => onLabelEditRequested()
                            | _ => actToggleInfoExpanded()
                        }
                    }
                }
                onLongClick=onLabelEditRequested
                style=ReactDOM.Style.make(
                    ~cursor=?{if (readOnly) {None} else {Some("pointer")}}, 
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ~display="inline-block",
                    ()
                )
                title=?{
                    if (readOnly) {
                        None
                    } else {
                        Some(
                            chgLabelShortcutName ++ " to change. " 
                                ++ "Alt is sometimes labelled Opt. " 
                                ++ showJstfShortcutName ++ " to show/hide the justification for provable."
                        )
                    }
                }
            >
                {React.string(stmt.label)}
            </LongClickSpan>
        }
    }

    let rndSelectionButtons = () => {
        let style = if (viewOptions.smallBtns) {
            Some(ReactDOM.Style.make(~padding="0px", ~minWidth="30px", ~minHeight="26px", ()))
        } else {
            None
        }
        let clickedTimeStr = switch stmt.cont {
            | Tree({clickedNodeId:Some(_,time)}) => time->Date.toISOString
            | _ => "1"
        }
        <Row alignItems=#center style=ReactDOM.Style.make(~marginTop="3px", ())>
            <ButtonGroup variant=#outlined size=#small >
                <Button title="Expand selection, W" onClick={_=>actExpandSelection()} ?style> <MM_Icons.ZoomOutMap/> </Button>
                <Button title="Shrink selection, S" onClick={_=>actShrinkSelection()} ?style> <MM_Icons.ZoomInMap/> </Button>
                {
                    if (readOnly) {React.null} else {
                        <Button title="Add new step above" onClick={_=>actAddStmtAbove()} ?style> 
                            <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(-90deg)", ()) />
                        </Button>
                    }
                }
                {
                    if (readOnly) {React.null} else {
                        <Button title="Add new step below" onClick={_=>actAddStmtBelow()} ?style> 
                            <MM_Icons.Logout style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />
                        </Button>
                    }
                }
                {
                    if (readOnly) {React.null} else {
                        <Button 
                            title="Transform, Q" 
                            onClick={_=>actOpenFragmentTransform()} 
                            ?style
                        > 
                            <MM_Icons.ShapeLineSharp/>
                        </Button>
                    }
                }
                {
                    if (readOnly) {React.null} else {
                        <Button 
                            title="Apply a substitution to all steps, R" 
                            onClick={_=>actOpenSubstitutionDialog()} 
                            ?style
                        > 
                            <MM_Icons.TextRotationNone/>
                        </Button>
                    }
                }
                <Button title="Copy to the clipboard, A" onClick={_=>actCopyToClipboard()} ?style> <MM_Icons.ContentCopy/> </Button>
                {
                    if (readOnly) {React.null} else {
                        <Button title="Paste from the clipboard to the selection, D" onClick={_=>actPasteFromClipboard()} ?style>
                            <MM_Icons.ContentPaste/>
                        </Button>
                    }
                }
                {
                    if (readOnly) {React.null} else {
                        <Button title="Edit, E" onClick={_=>actEditSelection()} ?style> <MM_Icons.Edit/> </Button>
                    }
                }
                <Button title="Unselect, Esc" onClick={_=>actUnselect()} ?style> <MM_Icons.CancelOutlined/> </Button>
            </ButtonGroup>
            {
                if (copiedToClipboard->Belt.Option.isSome) {
                    React.string("Copied to the clipboard.")
                } else {React.null}
            }
            {
                rndHiddenTextField(
                    ~key=clickedTimeStr,
                    ~onKeyDown=kbrdHnds([
                        kbrdClbkMake(~key="w", ~act=actExpandSelection),
                        kbrdClbkMake(~key="s", ~act=actShrinkSelection),
                        kbrdClbkMake(~key="q", ~act=actOpenFragmentTransform),
                        kbrdClbkMake(~key="r", ~act=actOpenSubstitutionDialog),
                        kbrdClbkMake(~key="e", ~act=actEditSelection),
                        kbrdClbkMake(~key="a", ~act=actCopyToClipboard),
                        kbrdClbkMake(~key="d", ~act=actPasteFromClipboard),
                        kbrdClbkMake(~key=keyEsc, ~act=actUnselect),
                    ])
                )
            }
        </Row>
    }

    let rndCont = ():reElem => {
        if (stmt.contEditMode) {
            let windowWidth = window["innerWidth"]
            let textFieldWidth = if (viewOptions.inlineMode) {
                windowWidth - 40
            } else {
                let checkBoxWidth = if (!viewOptions.showCheckbox) {0} else { 48 } 
                let labelWidth = if (!viewOptions.showLabel) {0} else { switch labelRef.current->Nullable.toOption {
                    | None => 0
                    | Some(domElem) => ReactDOM.domElementToObj(domElem)["offsetWidth"] + 10
                } } 
                let typWidth = if (!viewOptions.showType) {0} else { 28 } 
                let jstfWidth = if (!viewOptions.showJstf) {0} else { switch jstfRef.current->Nullable.toOption {
                    | None => 0
                    | Some(domElem) => ReactDOM.domElementToObj(domElem)["offsetWidth"] + 10
                } }
                Math.Int.max(
                    200,
                    windowWidth - checkBoxWidth - labelWidth - typWidth - jstfWidth - 40
                )
            }
            <Col 
                spacing=0.
                style=ReactDOM.Style.make(
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ()
                )
            >
                <TextField
                    inputRef=ReactDOM.Ref.domRef(stmtTextFieldRef)
                    size=#small
                    style=ReactDOM.Style.make(
                        ~width = (textFieldWidth)->Belt_Int.toString ++ "px", 
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
                    onKeyDown=kbrdHnd3(
                        kbrdClbkMake(~key=keyEnter, ~act=actContEditDone),
                        kbrdClbkMake(~key=keyEsc, ~act=actContEditCancel),
                        kbrdClbkMake(~alt=true, ~key="p", ~act=toggleParenAc),
                    )
                    title="Enter to save, Shift+Enter to start a new line, Esc to cancel"
                />
                <Row>
                    {
                        if (parenAc) {
                            rndIconButton(~icon=<MM_Icons.Code/>, 
                                ~onClick=toggleParenAc, 
                                ~title="Parentheses autocomplete is On; press Alt+P to turn it Off"
                            )
                        } else {
                            rndIconButton(~icon=<MM_Icons.CodeOff/>, 
                                ~onClick=toggleParenAc, 
                                ~title="Parentheses autocomplete is Off; press Alt+P to turn it On", 
                                ~color=None
                            )
                        }
                    }
                    {rndIconButton(~icon=<MM_Icons.Save/>, ~active= state.newText->String.trim != "",  
                        ~onClick=actContEditDone, ~title="Save, Enter")}
                    {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,  
                        ~onClick=actContEditCancel, ~title="Cancel, Esc", ~color=None)}
                </Row>
            </Col>
        } else {
            let textIsSelected = switch stmt.cont {
                | Text(_) => false
                | Tree({clickedNodeId}) => clickedNodeId->Belt.Option.isSome
            }
            let howToSelect =
                if (editStmtsByLeftClick) {
                    if (longClickEnabled) {
                        "<long-click> (Alt+<left-click>) to select"
                    } else {
                        "Alt+<left-click> to select"
                    }
                } else {
                    if (longClickEnabled) {
                        "<short-click> to select"
                    } else {
                        "<left-click> to select"
                    }
                }
            let title =
                if (editStmtsByLeftClick) {
                    if (longClickEnabled) {
                        "<short-click> to change, " ++ howToSelect
                    } else {
                        "<left-click> to change, " ++ howToSelect
                    }
                } else {
                    if (longClickEnabled) {
                        "<long-click> (Alt+<left-click>) to change, " ++ howToSelect
                    } else {
                        "Alt+<left-click> to change, " ++ howToSelect
                    }
                }
            let elems = [
                <Paper 
                    style=ReactDOM.Style.make(
                        ~padding="1px 3px",
                        ~backgroundColor=
                            if (stmt.stmtErr->Belt_Option.isSome || stmt.syntaxErr->Belt_Option.isSome) {
                                "rgb(255,230,230)"
                            } else {"rgb(255,255,235)"}, 
                        ~fontFamily="monospace",
                        ~fontSize="1.3em",
                        ()
                    ) 
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
                            ~longClickDelayMs,
                            ~renderSelection=true,
                            ~cursor = if (editStmtsByLeftClick) {"auto"} else {"pointer"},
                            ~title={if (readOnly) {howToSelect} else {title}}
                        )
                    }
                </Paper>
            ]
            if (syntaxTreeWasRequested->Belt.Option.isSome) {
                elems->Array.push(
                    <span> {"Building a syntax tree..."->React.string} </span>
                )
            }
            if (textIsSelected) {
                elems->Array.push(
                    rndSelectionButtons()
                )
            }

            <Col 
                spacing=0.
                style=ReactDOM.Style.make(
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ()
                )
            >
                {elems->React.array}
            </Col>
        }
    }

    let rndTyp = () => {
        if (stmt.typEditMode) {
            let typStrLowerCase = switch stmt.typ {
                | E => "e"
                | P => if (stmt.isGoal) {"g"} else {"p"}
            }
            <Col spacing=0.>
                <FormControl
                    size=#small
                    style=ReactDOM.Style.make(
                        ~marginLeft=stmtPartMarginLeft,
                        ~marginTop=stmtPartMarginTop,
                        ()
                    )
                >
                    <Select
                        value={typStrLowerCase}
                        onChange=evt2str(actTypEditDone)
                    >
                        <MenuItem value="e">{React.string("H - (Essential) Hypothesis")}</MenuItem>
                        <MenuItem value="p">{React.string("P - Provable Statement")}</MenuItem>
                        <MenuItem value="g">{React.string("G - Goal Statement")}</MenuItem>
                    </Select>
                </FormControl>
                {
                    rndIconButton(
                        ~icon=<MM_Icons.CancelOutlined/>, ~onClick=actTypEditCancel, ~title="Cancel", ~color=None
                    )
                }
            </Col>
        } else {
            let typStr = switch stmt.typ {
                | E => "H"
                | P => if (stmt.isGoal) {"G"} else {"P"}
            }
            let chgTypShortcutName = if (longClickEnabled) {"Long click (Alt + Left-click)"} else {"Alt + Left-click"}
            let showJstfShortcutName = if (longClickEnabled) {"Short click (Left-click)"} else {"Left-click"}
            <LongClickSpan
                onClick=clickHnd2(
                    clickClbkMake(~alt=true, ~act=onTypEditRequested),
                    clickClbkMake(~act=actToggleInfoExpanded),
                )
                longClickEnabled
                longClickDelayMs
                onShortClick = {
                    (clickAttrs:option<UseLongClick.clickAttrs>) => {
                        switch clickAttrs {
                            | Some({alt:true}) => onTypEditRequested()
                            | _ => actToggleInfoExpanded()
                        }
                    }
                }
                onLongClick=onTypEditRequested
                style=ReactDOM.Style.make(
                    ~cursor=?{if (readOnly) {None} else {Some("pointer")}}, 
                    ~fontWeight="bold", 
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ~display="inline-block",
                    ~backgroundColor=?(if (stmt.isBkm) {Some("rgb(65 65 65)")} else {None}),
                    ~color= if (stmt.isBkm) {"white"} else {"black"},
                    ~borderRadius="4px",
                    ()
                )
                title=?{
                    if (readOnly) {
                        None
                    } else {
                        Some(
                            chgTypShortcutName 
                                ++ " to change step type between P (provable), G (goal) and H (hypothesis). " 
                                ++ "Alt is sometimes labelled Opt. " 
                                ++ showJstfShortcutName ++ " to show/hide the justification for provable."
                        )
                    }
                }
            >
                {React.string(typStr)}
            </LongClickSpan>
        }
    }

    let visualizationIsAvailable = wrkCtx->Belt.Option.isSome
        && stmt.proofTreeDto->Belt.Option.isSome
        && stmt.src->Belt.Option.isSome

    let getFrmLabelBkgColor = (label:string):option<string> => {
        switch frms->frmsGetByLabel(label) {
            | None => None
            | Some(frm) => MM_react_common.getFrmLabelBkgColor(frm.frame, settings)
        }
    }

    let rndJstf = (~isInline:bool, ~textFieldWidth:string):reElem => {
        if (stmt.jstfEditMode) {
            <Col 
                spacing=0.
                style=ReactDOM.Style.make(
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ()
                )
            >
                <TextField
                    size=#small
                    label="Justification"
                    style=ReactDOM.Style.make(~width=textFieldWidth, ())
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
                    onKeyDown=kbrdHnd2(
                        kbrdClbkMake(~key=keyEnter, ~act=actJstfEditDone),
                        kbrdClbkMake(~key=keyEsc, ~act=actJstfEditCancel),
                    )
                    title="Enter to save, Esc to cancel"
                />
                <Row>
                    {rndIconButton(~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actJstfEditDone,
                        ~title="Save, Enter")}
                    {rndIconButton(~icon=<MM_Icons.CancelOutlined/>,
                        ~onClick=actJstfEditCancel, ~title="Cancel, Esc", ~color=None)}
                    {rndIconButton(~icon=<MM_Icons.DeleteForever/>,
                                ~onClick=actJstfDeleted, ~title="Clear", ~color=Some("red"))}
                </Row>
            </Col>
        } else {
            let jstfTextStr = if (stmt.typ == E) { "HYP" } else { stmt.jstfText }
            let jstfText = if (stmt.typ == E) { jstfTextStr->React.string } else {
                switch parseJstf(jstfTextStr) {
                    | Error(_) | Ok(None) => jstfTextStr->React.string
                    | Ok(Some({args, label})) => {
                        <span>
                            {React.string(args->Array.joinUnsafe(" ") ++ " : ")}
                            <span 
                                className="underline-on-hover"
                                style=ReactDOM.Style.make(
                                    ~cursor="pointer", 
                                    ~backgroundColor=?getFrmLabelBkgColor(label),
                                    ~borderRadius="3px",
                                    ()
                                )
                                onClick=clickHnd2(
                                    clickClbkMake(~alt=true, ~act=actJstfEditRequested),
                                    clickClbkMake(~act=()=>actOpenFrameExplorer(label)),
                                )
                            >
                                {label->React.string}
                            </span>
                        </span>
                    }
                }
            }
            let padding = if (jstfTextStr->String.trim == "") { "11px 16px" } else { "1px" }
            let title =
                if (longClickEnabled) {
                    "<long-click> (Alt+<left-click>) to change; click on the label to open a proof explorer tab"
                } else {
                    "Alt+<left-click> to change; click on the label to open a proof explorer tab"
                }
            <Row
                spacing=0.
                style=ReactDOM.Style.make(
                    ~marginLeft=stmtPartMarginLeft, 
                    ~marginTop=stmtPartMarginTop, 
                    ()
                )
                alignItems=#center
            >
                <LongClickPaper
                    longClickEnabled
                    longClickDelayMs
                    onShortClick={
                        (clickAttrs:option<UseLongClick.clickAttrs>) => {
                            switch clickAttrs {
                                | Some({alt:true}) => actJstfEditRequested()
                                | _ => ()
                            }
                        }
                    }
                    onLongClick=actJstfEditRequested
                    ref_=ReactDOM.Ref.domRef(jstfRef) 
                    onClick=clickHnd(~alt=true, ~act=actJstfEditRequested)
                    style=ReactDOM.Style.make( 
                        ~padding, 
                        ~overflowWrap="normal", 
                        ~whiteSpace="nowrap", 
                        ()
                    )
                    title=?{if (readOnly) {None} else {Some(title)}}
                >
                    jstfText
                </LongClickPaper>
                {
                    if (isInline) {
                        <span style=ReactDOM.Style.make(~display="none", ())/>
                    } else {
                        let btns = []
                        if (jstfTextStr->String.trim != "") {
                            btns->Array.push(
                                rndIconButton(~icon=<MM_Icons.DeleteForever/>, ~key="d",
                                    ~onClick=actJstfDeleted, ~title="Delete justification", ~color=None
                                )
                            )
                        }
                        btns->Array.push(
                            rndIconButton(~icon=<MM_Icons.VisibilityOff/>, ~key="h",
                                ~onClick=actToggleInfoExpanded, ~title="Hide justification", ~color=None
                            )
                        )
                        if (visualizationIsAvailable) {
                            btns->Array.push(
                                rndIconButton(
                                    ~icon=<MM_Icons.AccountTree style=ReactDOM.Style.make(~transform="rotate(90deg)", ()) />, 
                                    ~key="v",
                                    ~onClick=actToggleVisExpanded, ~title="Show/Hide visualization", ~color=None
                                )
                            )
                        }
                        <Row style=ReactDOM.Style.make(~marginLeft="10px", ())>
                            { btns->React.array }
                        </Row>
                    }
                }
            </Row>
        }
    }

    let rndJstfVisualization = ():option<reElem> => {
        if (visualizationIsAvailable) {
            Some(
                <VisualizedJstf
                    wrkCtx={wrkCtx->Belt_Option.getExn}
                    proofTreeDto={stmt.proofTreeDto->Belt_Option.getExn}
                    jstfText=stmt.jstfText
                    src={stmt.src->Belt_Option.getExn}
                    typeColors
                    preCtxColors
                    wrkCtxColors
                />
            )
        } else {
            None
        }
    }

    let rndInfoBody = ():option<reElem> => {
        if (stmt.typ != P) {
            None
        } else {
            let jstf = if (!viewOptions.showJstf && (state.infoExpanded || stmt.jstfEditMode)) {
                Some(rndJstf(~isInline=false, ~textFieldWidth="600px"))
            } else {
                None
            }
            let jstfVis = if (state.infoExpanded && (state.visExpanded || viewOptions.showJstf)) {
                rndJstfVisualization()
            } else {
                None
            }

            if (jstf->Belt_Option.isNone && jstfVis->Belt_Option.isNone) {
                None
            } else {
                Some(
                    <Col spacing=0.>
                        {jstf->Belt_Option.getWithDefault(React.null)}
                        {jstfVis->Belt_Option.getWithDefault(React.null)}
                    </Col>
                )
            }
        }
    }

    let rndContAndInfoBody = () => {
        switch rndInfoBody() {
            | None => rndCont()
            | Some(infoBody) => {
                <Col spacing=0.>
                    {rndCont()}
                    infoBody
                </Col>
            }
        }
    }

    let rndProofStatusInner = () => {
        let readyTooltip =
            if (longClickEnabled) {
                "A proof is ready, <long-click> (Alt+<left-click>) to show a completed proof"
            } else {
                "A proof is ready, Alt+<left-click> to show a completed proof"
            }
        rndProofStatus(
            ~proofStatus=stmt.proofStatus, 
            ~longClickEnabled,
            ~longClickDelayMs,
            ~readyTooltip,
            ~waitingTooltip="Justification for this step is correct",
            ~noJstfTooltip="Justification cannot be determined automatically. Click to debug.",
            ~jstfIsIncorrectTooltip="Justification is incorrect. Click to debug.",
            ~onReadyIconClicked=actToggleInfoExpanded,
            ~onReadyIconAltClicked=onGenerateProof,
            ~onWaitingIconClicked=actToggleInfoExpanded,
            ~onErrorIconClicked=onDebug,
            ~onNoJstfIconClicked=onDebug
        )
    }

    let rndProofStatusTd = (tdStyle) => {
        if (stmt.proofStatus->Belt.Option.isSome) {
            <td style=tdStyle> { rndProofStatusInner() } </td>
        } else {
            React.null
        }
    }

    let rndProofStatusRow = () => {
        if (stmt.proofStatus->Belt.Option.isSome) {
            rndProofStatusInner()
        } else {
            <span style=ReactDOM.Style.make(~display="none", ())/>
        }
    }

    let rndCheckbox = () => {
        <Checkbox
            style=ReactDOM.Style.make(
                ~marginLeft=stmtPartMarginLeft,
                ~marginTop=stmtPartMarginTop,
                ~padding="0px",
                ()
            )
            disabled=checkboxDisabled
            checked=checkboxChecked
            onChange={evt => {
                checkboxOnChange(
                    ~checked=ReactEvent.Form.target(evt)["checked"],
                    ~shift=ReactEvent.Form.nativeEvent(evt)["shiftKey"],
                )
            }}
        />
    }

    let rndCheckboxTd = (tdStyle) => {
        if (viewOptions.showCheckbox) {
            <td style=tdStyle> {rndCheckbox()} </td>
        } else {
            React.null
        }
    }

    let rndCheckboxRow = () => {
        if (viewOptions.showCheckbox) {
            rndCheckbox()
        } else {
            <span style=ReactDOM.Style.make(~display="none", ())/>
        }
    }

    let rndLabelTd = (tdStyle) => {
        if (viewOptions.showLabel) {
            <td style=tdStyle> {rndLabel()} </td>
        } else {
            React.null
        }
    }

    let rndLabelRow = () => {
        if (viewOptions.showLabel) {
            rndLabel()
        } else {
            <span style=ReactDOM.Style.make(~display="none", ())/>
        }
    }

    let rndTypTd = (tdStyle) => {
        if (viewOptions.showType) {
            <td style=tdStyle> {rndTyp()} </td>
        } else {
            React.null
        }
    }

    let rndTypRow = () => {
        if (viewOptions.showType) {
            rndTyp()
        } else {
            <span style=ReactDOM.Style.make(~display="none", ())/>
        }
    }

    let rndJstfTd = (tdStyle) => {
        if (viewOptions.showJstf) {
            <td style=tdStyle> {rndJstf(~isInline=true, ~textFieldWidth="150px")} </td>
        } else {
            React.null
        }
    }

    let rndJstfRow = () => {
        if (viewOptions.showJstf) {
            rndJstf(~isInline=true, ~textFieldWidth="150px")
        } else {
            <span style=ReactDOM.Style.make(~display="none", ())/>
        }
    }

    if (viewOptions.inlineMode) {
        <Row spacing=0. alignItems=#"flex-start">
            { rndCheckboxRow() }
            { rndProofStatusRow() }
            { rndLabelRow() }
            { rndTypRow() }
            { rndJstfRow() }
            { rndContAndInfoBody() }
        </Row>
    } else {
        let tdStyle = ReactDOM.Style.make(~padding="0px", ~borderCollapse="collapse", ())
        <table 
            style=ReactDOM.Style.make(
                ~cursor=if (syntaxTreeWasRequested->Belt.Option.isSome) {"wait"} else {""},
                ~padding="0px",
                ~borderCollapse="collapse",
                ()
            )>
            <tbody>
                <tr style=ReactDOM.Style.make(~verticalAlign="top", ())>
                    { rndCheckboxTd(tdStyle) }
                    { rndProofStatusTd(tdStyle) }
                    { rndLabelTd(tdStyle) }
                    { rndTypTd(tdStyle) }
                    { rndJstfTd(tdStyle) }
                    <td style=tdStyle> {rndContAndInfoBody()} </td>
                </tr>
            </tbody>
        </table>
    }
}, propsAreSame)
