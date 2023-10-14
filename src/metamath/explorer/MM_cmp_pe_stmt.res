open Expln_React_Mui
open Expln_utils_promise
open MM_wrk_editor
open MM_wrk_settings
open MM_react_common
open MM_context
open MM_substitution
open MM_parenCounter
open Expln_React_Modal
open Common
open MM_cmp_user_stmt

type state = {
    cont:stmtCont,
}

let makeInitialState = (~ctx:mmContext, ~stmt:expr, ~symColors:Belt_HashMapString.t<string>) => {
    let syms = ctx->ctxIntsToSymsExn(stmt)
    {
        cont:Text({
            text: syms->Js.Array2.joinWith(" "),
            syms: syms->Js_array2.map(sym => {
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
    editStmtsByLeftClick:bool,
    openExplorer:(~initPatternFilterStr:string)=>unit,
}

let propsAreSame = (a:props,b:props):bool => {
    a.ctx === b.ctx
    && a.stmt === b.stmt
    && a.symColors === b.symColors
    && a.symRename === b.symRename
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
                openInfoDialog( ~modalRef, ~text=msg, () )
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
            {...treeData, clickedNodeId:Some((nodeId,Js_date.make())), expLvl:0}->incExpLvlIfConstClicked
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
            | Some(selectedText) => {
                let searchPattern = ctx->ctxStrToIntsExn(selectedText)->Js.Array2.map(s => {
                    if (s < 0) {
                        s
                    } else {
                        ctx->getTypeOfVarExn(s)
                    }
                })->(ctxIntsToStrExn(ctx, _))
                openExplorer(~initPatternFilterStr=searchPattern)
            }
        }
    }

    let actBuildSyntaxTree = (clickedIdx:int):unit => {
        switch state.cont {
            | Tree(_) => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because stmtCont is a tree.`))
            | Text({text,syms}) => {
                switch textToSyntaxTree( 
                    ~wrkCtx=ctx, ~syms=[syms->Js_array2.map(s => s.sym)->Js_array2.sliceFrom(_, 1)], 
                    ~syntaxTypes, ~frms, ~frameRestrict, ~parenCnt,
                    ~lastSyntaxType=getLastSyntaxType(),
                    ~onLastSyntaxTypeChange=setLastSyntaxType,
                ) {
                    | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                    | Ok(syntaxTrees) => {
                        switch syntaxTrees[0] {
                            | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                            | Ok(syntaxTree) => {
                                let stmtContTreeData = {
                                    text,
                                    exprTyp:syms[0].sym, 
                                    root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors=symColors, () ), 
                                    clickedNodeId:getNodeIdBySymIdx(~tree=syntaxTree, ~symIdx=clickedIdx)
                                                        ->Belt.Option.map(id => (id,Js_date.make())),
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
        <Row alignItems=#center>
            <ButtonGroup variant=#outlined size=#small >
                <Button title="Expand selection" onClick={_=>actExpandSelection()}> <MM_Icons.ZoomOutMap/> </Button>
                <Button title="Shrink selection" onClick={_=>actShrinkSelection()}> <MM_Icons.ZoomInMap/> </Button>
                <Button title="Copy to the clipboard" onClick={_=>actCopyToClipboard()}> <MM_Icons.ContentCopy/> </Button>
                <Button title="Search in a new Explorer tab" 
                    onClick={_=>actSearchSelectedInNewExplorer()}
                > 
                    <MM_Icons.Search/>
                </Button>
                <Button title="Unselect" onClick={_=>actUnselect()}> <MM_Icons.CancelOutlined/> </Button>
            </ButtonGroup>
            {
                if (copiedToClipboard->Belt.Option.isSome) {
                    React.string("Copied to the clipboard.")
                } else {React.null}
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
                        ()
                    )
                }
            </span>
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

    rndStmt()

}, propsAreSame)