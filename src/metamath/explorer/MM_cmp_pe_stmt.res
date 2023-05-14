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

type state = {
    cont:stmtCont,
}

let makeInitialState = (~ctx:mmContext, ~stmt:expr, ~symColors:Belt_HashMapString.t<string>) => {
    {
        cont:Text(
            ctx->ctxIntsToSymsExn(stmt)->Js_array2.map(sym => {
                {sym, color:symColors->Belt_HashMapString.get(sym)}
            })
        ),
    }
}

type props = {
    modalRef:modalRef,

    ctx:mmContext,
    syntaxTypes:array<int>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,

    stmt:expr,
    symColors:Belt_HashMapString.t<string>,
    symRename:option<Belt_HashMapString.t<string>>,
    editStmtsByLeftClick:bool,
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
    parenCnt,
    stmt,
    symColors,
    symRename,
    editStmtsByLeftClick,
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
        setState(st => {...st, cont:newCont})
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
        actUpdateSyntaxTree(treeData => {...treeData, clickedNodeId:Some(nodeId), expLvl:0})
    }

    let actUnselect = () => {
        actUpdateSyntaxTree(treeData => {...treeData, clickedNodeId:None})
    }

    let actExpandSelection = () => {
        actUpdateSyntaxTree(updateExpLavel(_,true))
    }

    let actShrinkSelection = () => {
        actUpdateSyntaxTree(updateExpLavel(_,false))
    }

    let actCopyToClipboard = () => {
        switch getSelectedSymbols(state.cont) {
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

    let actBuildSyntaxTree = (clickedIdx:int):unit => {
        switch state.cont {
            | Tree(_) => setSyntaxTreeError(_ => Some(`Cannot build a syntax tree because stmtCont is a tree.`))
            | Text(syms) => {
                switch textToSyntaxTree( 
                    ~wrkCtx=ctx, ~syms, ~syntaxTypes, ~frms, ~parenCnt, 
                    ~lastSyntaxType=getLastSyntaxType(),
                    ~onLastSyntaxTypeChange=setLastSyntaxType,
                ) {
                    | Error(msg) => setSyntaxTreeError(_ => Some(msg))
                    | Ok(syntaxTree) => {
                        actUpdateStmt(Tree({
                            exprTyp:syms[0].sym, 
                            root:addColorsToSyntaxTree( ~tree=syntaxTree, ~preCtxColors=symColors, () ), 
                            clickedNodeId:getNodeIdBySymIdx(~tree=syntaxTree, ~symIdx=clickedIdx),
                            expLvl:0,
                        }))
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

    let rndSelectionButtons = () => {
        <Row alignItems=#center>
            <ButtonGroup variant=#outlined size=#small >
                <Button title="Expand selection" onClick={_=>actExpandSelection()}> <MM_Icons.ZoomOutMap/> </Button>
                <Button title="Shrink selection" onClick={_=>actShrinkSelection()}> <MM_Icons.ZoomInMap/> </Button>
                <Button title="Copy to the clipboard" onClick={_=>actCopyToClipboard()}> <MM_Icons.ContentCopy/> </Button>
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
                    ~padding="1px 10px", 
                    ~fontFamily="monospace",
                    ~fontSize="1.3em",
                    ()
                )
            >
                {
                    rndContText(
                        ~stmtCont=state.cont, 
                        ~onTextClick=idx=>setSyntaxTreeWasRequested(_ => Some(idx)),
                        ~onTreeClick=actTreeNodeClicked,
                        ~renderSelection=true,
                        ~editStmtsByLeftClick,
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