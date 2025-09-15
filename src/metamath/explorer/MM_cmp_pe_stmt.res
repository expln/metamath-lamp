open Expln_React_Mui
open Expln_utils_promise
open MM_wrk_editor
open MM_wrk_settings
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open Expln_React_Modal
open MM_cmp_user_stmt

type state = {
    cont:stmtCont,
}

let makeInitialState = (~ctx:mmContext, ~stmt:expr, ~symColors:Belt_HashMapString.t<string>) => {
    let syms = ctx->ctxIntsToSymsExn(stmt)
    {
        cont:Text({
            text: syms->Array.joinUnsafe(" "),
            syms: syms->Array.map(sym => {
                {sym, color:symColors->Belt_HashMapString.get(sym)}
            })
        }),
    }
}

type props = {
    modalRef:modalRef,

    ctx:mmContext,
    syntaxTypes:array<int>,
    frms: frms,
    frameRestrict:frameRestrict,
    parenCnt: parenCnt,

    stmt:expr,
    symColors:Belt_HashMapString.t<string>,
    symRename:option<Belt_HashMapString.t<string>>,
    symsToHighlight:option<array<int>>,
    editStmtsByLeftClick:bool,
    openExplorer:option<(~initPatternFilterStr:string=?, ~initDependsOnFilter:string=?)=>unit>,
}

let propsAreSame = (a:props,b:props):bool => {
    a.ctx === b.ctx
    && a.stmt === b.stmt
    && a.symColors === b.symColors
    && a.symRename === b.symRename
    && a.symsToHighlight === b.symsToHighlight
    && a.editStmtsByLeftClick === b.editStmtsByLeftClick
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    ctx,
    syntaxTypes,
    frms,
    frameRestrict,
    parenCnt,
    stmt,
    symColors,
    symRename,
    symsToHighlight,
    editStmtsByLeftClick,
    openExplorer,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~ctx, ~stmt, ~symColors))
    let (syntaxTreeWasRequested, setSyntaxTreeWasRequested) = React.useState(() => None)
    let (syntaxTreeError, setSyntaxTreeError) = React.useState(() => None)
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    React.useEffect5(() => {
        setState(_ => makeInitialState(~ctx, ~stmt, ~symColors))
        None
    }, (ctx, stmt, symColors, symRename, editStmtsByLeftClick))

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

    let actUpdateStmt = (newCont:stmtCont):unit => {
        setState(_ => {cont:newCont})
    }

    let actUpdateSyntaxTree = (update:stmtContTreeData=>stmtContTreeData):unit => {
        switch state.cont {
            | Text(_) => ()
            | Tree(treeData) => {
                actUpdateStmt(Tree(treeData->update))
            }
        }
    }

    let actTreeNodeClicked = (nodeId) => {
        actUpdateSyntaxTree(treeData => {
            {...treeData, clickedNodeId:Some((nodeId,Date.make())), expLvl:0}->incExpLvlIfConstClicked
        })
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

    let actCopyToClipboard = () => {
        switch getSelectedText(state.cont) {
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

    let actSearchSelectedInNewExplorer = () => {
        switch getSelectedText(state.cont) {
            | None => ()
            | Some(selectedText) => openExplorer->Option.forEach(fn=>fn(~initPatternFilterStr="$+ " ++ selectedText))
        }
    }

    let actBuildSyntaxTree = (clickedIdx:int):unit => {
        switch state.cont {
            | Tree(_) => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because stmtCont is a tree.`))
            | Text({text,syms}) => {
                switch textToSyntaxTree( 
                    ~wrkCtx=ctx, ~syms=[syms->Array.map(s => s.sym)->Array.sliceToEnd(_, ~start=1)],
                    ~syntaxTypes, ~frms, ~frameRestrict, ~parenCnt,
                    ~lastSyntaxType=getLastSyntaxType(),
                    ~onLastSyntaxTypeChange=setLastSyntaxType,
                ) {
                    | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                    | Ok(syntaxTrees) => {
                        switch syntaxTrees->Array.getUnsafe(0) {
                            | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                            | Ok(syntaxTree) => {
                                let stmtContTreeData = {
                                    text,
                                    exprTyp:(syms->Array.getUnsafe(0)).sym,
                                    root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors=symColors ), 
                                    clickedNodeId:getNodeIdBySymIdx(~tree=syntaxTree, ~symIdx=clickedIdx)
                                                        ->Belt.Option.map(id => (id,Date.make())),
                                    expLvl:0,
                                }
                                actUpdateStmt(Tree(stmtContTreeData->incExpLvlIfConstClicked))
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

    let rndSelectionButtons = () => {
        let clickedTimeStr = switch state.cont {
            | Tree({clickedNodeId:Some(_,time)}) => time->Date.toISOString
            | _ => "1"
        }
        <Row alignItems=#center>
            <ButtonGroup variant=#outlined size=#small >
                <Button title="Expand selection, W" onClick={_=>actExpandSelection()}> <MM_Icons.ZoomOutMap/> </Button>
                <Button title="Shrink selection, S" onClick={_=>actShrinkSelection()}> <MM_Icons.ZoomInMap/> </Button>
                <Button title="Copy to the clipboard" onClick={_=>actCopyToClipboard()}> <MM_Icons.ContentCopy/> </Button>
                {
                    switch openExplorer {
                        | Some(_) => {
                            <Button title="Search in a new Explorer tab" 
                                onClick={_=>actSearchSelectedInNewExplorer()}
                            > 
                                <MM_Icons.Search/>
                            </Button>
                        }
                        | None => React.null
                    }
                }
                <Button title="Unselect, Esc" onClick={_=>actUnselect()}> <MM_Icons.CancelOutlined/> </Button>
            </ButtonGroup>
            {
                if (copiedToClipboard->Belt.Option.isSome) {
                    React.string("Copied to the clipboard.")
                } else {React.null}
            }
            {
                rndHiddenTextField(
                    ~key=clickedTimeStr,
                    ~onKeyDown=kbrdHnd3(
                        kbrdClbkMake(~key="w", ~act=actExpandSelection),
                        kbrdClbkMake(~key="s", ~act=actShrinkSelection),
                        kbrdClbkMake(~key=keyEsc, ~act=actUnselect),
                    )
                )
            }
        </Row>
    }

    let rndStmt = () => {
        let textIsSelected = switch state.cont {
            | Text(_) => false
            | Tree({clickedNodeId}) => clickedNodeId->Belt.Option.isSome
        }
        let elems = [
            <span 
                style=ReactDOM.Style.make(
                    ~fontFamily="monospace",
                    ~fontSize="1.3em",
                    ()
                )
            >
                {
                    rndContText(
                        ~stmtCont=state.cont, 
                        ~onTextLeftClick=idx=>setSyntaxTreeWasRequested(_ => Some(idx)),
                        ~onTextAltLeftClick=idx=>setSyntaxTreeWasRequested(_ => Some(idx)),
                        ~onTreeLeftClick=actTreeNodeClicked,
                        ~onTreeAltLeftClick=actTreeNodeClicked,
                        ~cursor="pointer",
                        ~renderSelection=true,
                        ~symRename?,
                        ~symsToHighlight?,
                    )
                }
            </span>
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
        <Col>
            {elems->React.array}
        </Col>
    }

    rndStmt()

}, propsAreSame)