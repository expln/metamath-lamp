open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open MM_provers
open MM_proof_tree
open MM_proof_tree_dto
open Expln_React_Modal
open Local_storage_utils

@val external window: {..} = "window"
@val external setTimeout: (unit => unit, int) => unit = "setTimeout"

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

let leftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk, ifNot: ReactEvent.Mouse.t => unit) => {
    if (mouseEvt->ReactEvent.Mouse.button == 0 
            && !(mouseEvt->ReactEvent.Mouse.altKey) 
            && !(mouseEvt->ReactEvent.Mouse.ctrlKey)) {
        clbk()
    } else {
        ifNot(mouseEvt)
    }
}

let altLeftClickHnd = (mouseEvt:ReactEvent.Mouse.t, clbk, ifNot: ReactEvent.Mouse.t => unit) => {
    if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.altKey) {
        clbk()
    } else {
        ifNot(mouseEvt)
    }
}

let altLeftClickHnd2 = (clbk:unit=>unit):(ReactEvent.Mouse.t => unit) => {
    mouseEvt => {
        if (mouseEvt->ReactEvent.Mouse.button == 0 && mouseEvt->ReactEvent.Mouse.altKey) {
            clbk()
        }
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
        Error(`Cannot build a syntax tree without a list of syntax types.`)
    } else {
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
        let expr = syms->Js_array2.map(stmtSym => stmtSym.sym)->ctxSymsToIntsExn(wrkCtx, _)
        let stmtsToProve = syntaxTypes->Js_array2.map(typ => [typ]->Js_array2.concat(expr->Js_array2.sliceFrom(1)))
        let maxIdx = stmtsToProve->Js_array2.length-1
        let idx = ref(0)
        let proofTreeRef = ref(None)
        while (idx.contents <= maxIdx && proofTreeRef.contents->Belt_Option.isNone) {
            let stmtToProve = stmtsToProve[idx.contents]
            let proofTree = proveFloatings(
                ~wrkCtx,
                ~frms,
                ~floatingsToProve=[stmtToProve],
                ~parenCnt,
                ()
            )
            if (proofTree->ptGetNode(stmtToProve)->pnGetProof->Belt_Option.isSome) {
                proofTreeRef := Some(proofTree)
                switch (lastSyntaxType, wrkCtx->ctxIntToSym(syntaxTypes[idx.contents])) {
                    | (None, Some(provedSyntaxTypeStr)) => onLastSyntaxTypeChange(provedSyntaxTypeStr)
                    | (Some(lastSyntaxTypeStr), Some(provedSyntaxTypeStr)) => {
                        if (lastSyntaxTypeStr != provedSyntaxTypeStr) {
                            onLastSyntaxTypeChange(provedSyntaxTypeStr)
                        }
                    }
                    | _ => ()
                }
            } else {
                idx := idx.contents + 1
            }
        }
        switch proofTreeRef.contents {
            | None => Error(`Could not prove this statement is of any of the types: ${wrkCtx->ctxIntsToStrExn(syntaxTypes)}`)
            | Some(proofTree) => {
                let provedStmt = stmtsToProve[idx.contents]
                let proofTreeDto = proofTree->proofTreeToDto([provedStmt])
                switch proofTreeDto.nodes->Js_array2.find(node => node.expr->exprEq(provedStmt)) {
                    | None => Error(`Could not find proof for: ${wrkCtx->ctxIntsToStrExn(provedStmt)}`)
                    | Some(proofNode) => {
                        let proofTable = createProofTable(~tree=proofTreeDto, ~root=proofNode, ())
                        switch buildSyntaxTree(wrkCtx, proofTable, proofTable->Js_array2.length-1) {
                            | Error(msg) => Error(msg)
                            | Ok(syntaxTree) => Ok(syntaxTree)
                        }
                    }
                }
            }
        }
    }
}

let rndSymbol = (
    ~isFirst:bool,
    ~key:string,
    ~sym:string,
    ~color:option<string>,
    ~onClick:option<ReactEvent.Mouse.t=>unit>=?,
    ~spaceBackgroundColor:option<string>=?,
    ~symbolBackgroundColor:option<string>=?,
    ()
):reElem => {
    <React.Fragment key>
        {
            if (isFirst) {
                React.null
            } else {
                <span 
                    ?onClick 
                    style=ReactDOM.Style.make(
                        ~backgroundColor=?spaceBackgroundColor,
                        ()
                    )
                > 
                    {" "->React.string} 
                </span>
            }
        }
        {
            let (color,fontWeight) = switch color {
                | None => ("black","normal")
                | Some(color) => (color,"bold")
            }
            <span ?onClick style=ReactDOM.Style.make( ~color, ~fontWeight, ~backgroundColor=?symbolBackgroundColor, () ) >
                {sym->React.string}
            </span>
        }
    </React.Fragment>
}

let getIdsOfAllChildSymbols = (tree:syntaxTreeNode):Belt_SetInt.t => {
    let res = []
    Expln_utils_data.traverseTree(
        (),
        Subtree(tree),
        (_, node) => {
            switch node {
                | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                | Symbol(_) => None
            }
        },
        ~process = (_, node) => {
            switch node {
                | Subtree(_) => ()
                | Symbol({id}) => res->Js.Array2.push(id)->ignore
            }
            None
        },
        ()
    )->ignore
    Belt_SetInt.fromArray(res)
}

let getIdsOfSelectedNodes = (stmtCont:stmtCont):(int,Belt_SetInt.t) => {
    switch stmtCont {
        | Text(_) => (-1,Belt_SetInt.empty)
        | Tree({root, clickedNodeId, expLvl}) => {
            switch clickedNodeId {
                | None => (-1,Belt_SetInt.empty)
                | Some(nodeId) => {
                    switch root->getNodeById(nodeId) {
                        | None => (-1,Belt_SetInt.empty)
                        | Some(Subtree(_)) => (-1,Belt_SetInt.empty) //this should never happen because a Subtree cannot be clicked
                        | Some(Symbol({parent, isVar})) => {
                            if (expLvl == 0) {
                                if (isVar) {
                                    (nodeId,Belt_SetInt.fromArray([nodeId]))
                                } else {
                                    (nodeId,getIdsOfAllChildSymbols(parent))
                                }
                            } else {
                                let curParent = ref(Some(parent))
                                let curLvl = ref(expLvl)
                                while (curLvl.contents > 0 && curParent.contents->Belt_Option.isSome) {
                                    curLvl := curLvl.contents - 1
                                    curParent := (curParent.contents->Belt_Option.getExn).parent
                                }
                                switch curParent.contents {
                                    | Some(parent) => (nodeId,getIdsOfAllChildSymbols(parent))
                                    | None => (nodeId,getIdsOfAllChildSymbols(root))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let rndContText = (
    ~stmtCont:stmtCont,
    ~onTextClick:option<int=>unit>=?,
    ~onTreeClick:option<int=>unit>=?,
    ()
) => {
    switch stmtCont {
        | Text(syms) => {
            let onClick = idx => onTextClick->Belt_Option.map(onTextClick => altLeftClickHnd2(() => onTextClick(idx)))
            syms->Js.Array2.mapi((stmtSym,i) => {
                rndSymbol(
                    ~isFirst = i==0,
                    ~key=i->Belt.Int.toString,
                    ~sym=stmtSym.sym,
                    ~color=stmtSym.color,
                    ~onClick=?onClick(i),
                    ()
                )
            })->React.array
        }
        | Tree({exprTyp, root}) => {
            let onClick = id => onTreeClick->Belt_Option.map(onTreeClick => altLeftClickHnd2(() => onTreeClick(id)))
            let (clickedId,selectedIds) = getIdsOfSelectedNodes(stmtCont)
            let elems = []
            elems->Js.Array2.push(
                rndSymbol(
                    ~isFirst=true,
                    ~key="expr-type",
                    ~sym=exprTyp,
                    ~color=None,
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
                                    ~onClick=?onClick(id),
                                    ~spaceBackgroundColor=?{ 
                                        if (symbolIsHighlighted && selectionIsOn.contents) {
                                            Some("#ADD6FF")
                                        } else {
                                            None
                                        } 
                                    },
                                    ~symbolBackgroundColor=?{ 
                                        if (symbolIsHighlighted) {
                                            if (id == clickedId) {
                                                Some("#99bce0")
                                            } else {
                                                Some("#ADD6FF")
                                            }
                                        } else {
                                            None
                                        } 
                                    },
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
    switch proofStatus {
        | None => React.null
        | Some(status) => {
            switch status {
                | Ready =>
                    <span 
                        title=?readyTooltip
                        style=ReactDOM.Style.make(
                            ~color="green", ~fontWeight="bold", 
                            ~cursor=if (onReadyIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onReadyIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >{React.string("\u2713")}</span>
                | Waiting =>
                    <span 
                        title=?waitingTooltip
                        style=ReactDOM.Style.make(~color="orange", ~fontWeight="bold", ())
                    >
                        {React.string("\u223F")}
                    </span>
                | NoJstf =>
                    <span 
                        title=?noJstfTooltip
                        style=ReactDOM.Style.make(
                            ~color="red", ~fontWeight="bold", 
                            ~cursor=if (onNoJstfIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onNoJstfIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >
                        {React.string("?")}
                    </span>
                | JstfIsIncorrect =>
                    <span 
                        title=?jstfIsIncorrectTooltip
                        style=ReactDOM.Style.make(
                            ~color="red", ~fontWeight="bold", 
                            ~cursor=if (onErrorIconClicked->Belt_Option.isSome) {"pointer"} else {"default"}, 
                            ()
                        )
                        onClick={_=>onErrorIconClicked->Belt_Option.forEach(clbk => clbk())}
                    >
                        {React.string("\u2717")}
                    </span>
            }
        }
    }
}

module VisualizedJstf = {
    type props = {
        wrkCtx:option<mmContext>,
        stmt:userStmt,
        typeColors:Belt_HashMapString.t<string>,
        preCtxColors:Belt_HashMapString.t<string>,
        wrkCtxColors:Belt_HashMapString.t<string>,
    }
    let make = React.memoCustomCompareProps( @react.component (props:props) => {
        switch (props.wrkCtx, props.stmt.proofTreeDto, props.stmt.jstf) {
            | (Some(ctx), Some(proofTreeDto), Some(jstf)) => {
                switch props.stmt.src {
                    | None | Some(VarType) | Some(Hypothesis(_)) | Some(AssertionWithErr(_)) => React.null
                    | Some(Assertion({args, label})) => {
                        switch ctx->getFrame(label) {
                            | None => React.null
                            | Some(frame) => {
                                let asrt = ctx->frmIntsToSymsExn(frame, frame.asrt)
                                let hyps = []
                                let subs = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                let frmColors = Belt_HashMapString.make(~hintSize = frame.hyps->Js.Array2.length)
                                frame.hyps->Js.Array2.forEachi((hyp,i) => {
                                    if (hyp.typ == E) {
                                        hyps->Js.Array2.push(ctx->frmIntsToSymsExn(frame, hyp.expr))->ignore
                                    } else {
                                        let frmSym = ctx->frmIntToSymExn(frame, hyp.expr[1])
                                        subs->Belt_HashMapString.set(
                                            frmSym,
                                            ctx->ctxIntsToSymsExn( 
                                                proofTreeDto.nodes[args[i]].expr->Js_array2.sliceFrom(1) 
                                            )
                                        )
                                        let typeSym = ctx->ctxIntToSymExn(hyp.expr[0])
                                        switch props.typeColors->Belt_HashMapString.get(typeSym) {
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
                                    ctxColors1=Some(props.preCtxColors)
                                    ctxColors2=Some(props.wrkCtxColors)
                                    subs
                                />
                            }
                        }
                    }
                }
            }
            | _ => React.null
        }
    }, (_,_) => true )
}

let getColorForSymbol = (
    ~sym:string,
    ~preCtxColors:Belt_HashMapString.t<string>,
    ~wrkCtxColors:Belt_HashMapString.t<string>,
):option<string> => {
    switch preCtxColors->Belt_HashMapString.get(sym) {
        | Some(color) => Some(color)
        | None => wrkCtxColors->Belt_HashMapString.get(sym)
    }
}

let rec addColorsToSyntaxTree = (
    ~tree:syntaxTreeNode,
    ~preCtxColors:Belt_HashMapString.t<string>,
    ~wrkCtxColors:Belt_HashMapString.t<string>,
):syntaxTreeNode => {
    {
        ...tree,
        children: tree.children->Js.Array2.map(child => {
            switch child {
                | Subtree(syntaxTreeNode) => {
                    Subtree(addColorsToSyntaxTree(~tree=syntaxTreeNode, ~preCtxColors, ~wrkCtxColors))
                }
                | Symbol(symData) => {
                    Symbol({ ...symData, color:getColorForSymbol(~sym=symData.sym, ~preCtxColors, ~wrkCtxColors)})
                }
            }
        })
    }
}

let getNodeIdBySymIdx = (
    ~symIdx:int,
    ~tree:syntaxTreeNode,
):option<int> => {
    let (_, idOpt) = Expln_utils_data.traverseTree(
        ref(0),
        Subtree(tree),
        (_, node) => {
            switch node {
                | Subtree(syntaxTreeNode) => Some(syntaxTreeNode.children)
                | Symbol(_) => None
            }
        },
        ~process = (cnt, node) => {
            switch node {
                | Subtree(_) => None
                | Symbol({id}) => {
                    cnt := cnt.contents + 1
                    if (cnt.contents == symIdx) {
                        Some(id)
                    } else {
                        None
                    }
                }
            }
        },
        ()
    )
    idOpt
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~wrkCtx:option<mmContext>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~syntaxTypes:array<int>,
    ~stmt:userStmt, 
    ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit, ~onLabelEditCancel:string=>unit,
    ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~onContEditRequested:unit=>unit, ~onContEditDone:string=>unit, ~onContEditCancel:string=>unit,
    ~onSyntaxTreeUpdated:stmtCont=>unit,
    ~onJstfEditRequested:unit=>unit, ~onJstfEditDone:string=>unit, ~onJstfEditCancel:string=>unit,
    ~onGenerateProof:unit=>unit,
    ~onDebug:unit=>unit,
    ~typeColors:Belt_HashMapString.t<string>,
    ~preCtxColors:Belt_HashMapString.t<string>,
    ~wrkCtxColors:Belt_HashMapString.t<string>,
    ~visualizationIsOn:bool,
) => {
    let (state, setState) = React.useState(_ => makeInitialState())
    let labelRef = React.useRef(Js.Nullable.null)

    let (syntaxTreeWasRequested, setSyntaxTreeWasRequested) = React.useState(() => None)
    let (syntaxTreeError, setSyntaxTreeError) = React.useState(() => None)

    React.useEffect1(() => {
        if (stmt.labelEditMode) {
            setState(setNewText(_,stmt.label))
        } else if (stmt.typEditMode) {
            setState(setNewText(_,stmt.typ->userStmtTypeToStr))
        } else if (stmt.contEditMode) {
            setState(setNewText(_,contToStr(stmt.cont)))
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

    let actBuildSyntaxTree = (clickedIdx:int):unit => {
        let lastSyntaxTypeLocStorKey = "editor-last-syntax-type"
        switch wrkCtx {
            | None => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because there was an error setting MM context.`))
            | Some(wrkCtx) => {
                switch stmt.cont {
                    | Tree(_) => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because stmtCont is a tree.`))
                    | Text(syms) => {
                        switch textToSyntaxTree( 
                            ~wrkCtx, ~syms, ~syntaxTypes, ~frms, ~parenCnt, 
                            ~lastSyntaxType=locStorReadString(lastSyntaxTypeLocStorKey),
                            ~onLastSyntaxTypeChange = locStorWriteString(lastSyntaxTypeLocStorKey, _),
                        ) {
                            | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                            | Ok(syntaxTree) => {
                                onSyntaxTreeUpdated(Tree({
                                    exprTyp:syms[0].sym, 
                                    root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors, ~wrkCtxColors ), 
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
                    10
                )
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
        actUpdateSyntaxTree(treeData => {...treeData, expLvl: treeData.expLvl + 1})
    }

    let actShrinkSelection = () => {
        actUpdateSyntaxTree(treeData => {...treeData, expLvl: Js_math.max_int(treeData.expLvl - 1, 0)})
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
                onClick=leftClickHnd(_, onLabelEditRequested, _ => ()) 
                title="<left-click> to change"
                style=ReactDOM.Style.make(~overflowWrap="normal", ~whiteSpace="nowrap", ())
            >
                {React.string(stmt.label)}
            </span>
        }
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
                    size=#small
                    style=ReactDOM.Style.make(
                        ~width = (windowWidth - 200 - labelWidth)->Belt_Int.toString ++ "px", 
                        ()
                    )
                    inputProps={
                        "style": {"fontFamily": "monospace"}
                    }
                    autoFocus=true
                    multiline=true
                    value=state.newText
                    onChange=evt2str(actNewTextUpdated)
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
                    onClick=leftClickHnd(_, onContEditRequested, _ => ()) 
                    style=ReactDOM.Style.make(
                        ~padding="1px 10px", 
                        ~backgroundColor="rgb(255,255,235)", 
                        ~fontFamily="monospace",
                        ~fontSize="1.3em",
                        ()
                    ) 
                    title="<left-click> to change"
                >
                    {
                        rndContText(
                            ~stmtCont=stmt.cont, 
                            ~onTextClick=idx=>setSyntaxTreeWasRequested(_ => Some(idx)), 
                            ~onTreeClick=actTreeNodeClicked, 
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
                    <ButtonGroup variant=#contained size=#small color="grey" >
                        <Button title="Expand selection" onClick={_=>actExpandSelection()}> <MM_Icons.ZoomOutMap/> </Button>
                        <Button title="Shrink selection" onClick={_=>actShrinkSelection()}> <MM_Icons.ZoomInMap/> </Button>
                        <Button title="Unselect" onClick={_=>actUnselect()}> <MM_Icons.CancelOutlined/> </Button>
                    </ButtonGroup>
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
            <span 
                onClick=altLeftClickHnd(_, onTypEditRequested, _ => actToggleInfoExpanded()) 
                style=ReactDOM.Style.make(~cursor="pointer", ~fontWeight="bold", ())
                title="Alt+<left-click> to change statement type. Left-click to show/hide the justification for provable."
            >
                {React.string(typStr->Js_string2.toUpperCase)}
            </span>
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
                    onClick=leftClickHnd(_, onJstfEditRequested, _ => ()) 
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
        if (visualizationIsOn) {
                switch stmt.src {
                    | None => React.null
                    | Some(_) => {
                        <VisualizedJstf
                            wrkCtx
                            stmt
                            typeColors
                            preCtxColors
                            wrkCtxColors
                        />
                    }
                }
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
}