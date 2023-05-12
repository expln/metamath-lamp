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

type state = {
    frmCtx:mmContext,
    symColors:Belt_HashMapString.t<string>,
    eHyps:array<expr>,
    asrt:expr,
    descrIsExpanded:bool
}

let makeInitialState = (~preCtx:mmContext, ~frame:frame, ~typeColors:Belt_HashMapString.t<string>) => {
    let frmCtx = createContext(~parent=preCtx, ())
    let symRename = ref(None)
    let frmVarIntToCtxInt = []
    let symColors = Belt_HashMapString.make(~hintSize=frame.numOfVars)

    let createLocalCtxVar = (~frmVarName, ~ctxVarTypInt):int => {
        let [ctxVarName] = generateNewVarNames( 
            ~ctx=frmCtx, 
            ~types=[ctxVarTypInt], 
            ~typeToPrefix=Belt_MapString.empty, 
            ()
        )
        let [ctxVarLabel] = generateNewLabels(
            ~ctx=frmCtx, 
            ~prefix="loc-var-", 
            ~amount=1,
            ()
        )
        frmCtx->applySingleStmt(Var({symbols:[ctxVarName]}))
        frmCtx->applySingleStmt(Floating({label:ctxVarLabel, expr:[frmCtx->ctxIntToSymExn(ctxVarTypInt), ctxVarName]}))
        switch symRename.contents {
            | None => {
                symRename := Belt_HashMapString.make(~hintSize=frame.numOfVars)
                symRename.contents->Belt_HashMapString.set(ctxVarName, frmVarName)
            }
            | Some(symRename) => {
                symRename->Belt_HashMapString.set(ctxVarName, frmVarName)
            }
        }
        frmCtx->ctxSymToIntExn(ctxVarName)
    }

    for frmVarInt in 0 to frame.numOfVars-1 {
        let frmVarName = frame.frameVarToSymb[frmVarInt]
        switch frmCtx->getTokenType(frmVarName) {
            | Some(V) => {
                let ctxVarInt = frmCtx->ctxSymToIntExn(frmVarName)
                let ctxVarTypInt = frmCtx->getTypeOfVarExn(ctxVarInt)
                let ctxVarName = if (frame.varTypes[frmVarInt] == ctxVarTypInt) {
                    frmVarIntToCtxInt->Js.Array2.push(ctxVarInt)
                    frmVarName
                } else {
                    let ctxNewVarInt = createLocalCtxVar(~frmVarName, ~ctxVarTypInt)
                    frmVarIntToCtxInt->Js.Array2.push(ctxNewVarInt)
                    frmCtx->ctxIntToSymExn(ctxNewVarInt)
                }
                let frmVarTypSym = frmCtx->ctxIntToSymExn(ctxVarTypInt)
                typeColors->Belt_HashMapString.get(frmVarTypSym)->Belt.Option.forEach(color => {
                    symColors->Belt_HashMapString.set()
                })
            }
            | _ => {

            }
        }
    }
    {
        cont:Text(
            ctx->frmIntsToSymsExn(frame,stmt)->Js_array2.map(sym => {
                {sym, color:symColors->Belt_HashMapString.get(sym)}
            })
        ),
        showButtons:false,
        permSels: [],
    }
}

let setContent = (st,cont):state => {
    {
        ...st,
        cont
    }
}

let setShowButtons = (st,showButtons):state => {
    {
        ...st,
        showButtons
    }
}

type props = {
    modalRef:modalRef,
    settingsVer:int,
    preCtxVer:int,
    frmCtx:mmContext,
    frame:frame,
    stmt:expr,
    symColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.preCtxVer == b.preCtxVer
    && a.frame === b.frame
    && a.stmt === b.stmt
    && a.editStmtsByLeftClick === b.editStmtsByLeftClick
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    settingsVer,
    preCtxVer,
    frmCtx,
    frame,
    stmt,
    symColors,
    editStmtsByLeftClick,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~ctx=frmCtx, ~frame, ~stmt, ~symColors))

    let rndStmt = () => {
        let elems = [
            <Paper 
                // onClick={
                    // if (editStmtsByLeftClick) {
                    //     leftClickHnd(onContEditRequested)
                    // } else {
                    //     altLeftClickHnd(onContEditRequested)
                    // }
                // }
                style=ReactDOM.Style.make(
                    ~padding="1px 10px", 
                    ~fontFamily="monospace",
                    ~fontSize="1.3em",
                    ()
                ) 
                // title={
                //     if (editStmtsByLeftClick) {
                //         "<left-click> to change, Alt+<left-click> to select"
                //     } else {
                //         "Alt + <left-click> to change, <left-click> to select"
                //     }
                // }
            >
                {
                    rndContText(
                        ~stmtCont=state.cont, 
                        // ~onTextClick=idx=>setSyntaxTreeWasRequested(_ => Some(idx)),
                        // ~onTreeClick=actTreeNodeClicked,
                        ~renderSelection=true,
                        ~editStmtsByLeftClick,
                        ()
                    )
                }
            </Paper>
        ]
        <Col>
            {elems->React.array}
        </Col>
    }

    rndStmt()

}, propsAreSame)