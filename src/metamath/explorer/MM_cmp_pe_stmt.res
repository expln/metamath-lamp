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
    cont:stmtCont,
    showButtons:bool,
    permSels: array<(Belt_HashSetInt.t, string)>,
}

let makeInitialState = (~ctx:mmContext, ~stmt:expr, ~symColors:Belt_HashMapString.t<string>) => {
    {
        cont:Text(
            ctx->ctxIntsToSymsExn(stmt)->Js_array2.map(sym => {
                {sym, color:symColors->Belt_HashMapString.get(sym)}
            })
        ),
        showButtons:false,
        permSels: [],
    }
}

type props = {
    modalRef:modalRef,
    settingsVer:int,
    ctxVer:int,
    ctx:mmContext,
    stmt:expr,
    symColors:Belt_HashMapString.t<string>,
    symRename:option<Belt_HashMapString.t<string>>,
    editStmtsByLeftClick:bool,
}

let propsAreSame = (a:props,b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.ctxVer == b.ctxVer
}

let make = React.memoCustomCompareProps( ({
    modalRef,
    settingsVer,
    ctxVer,
    ctx,
    stmt,
    symColors,
    symRename,
    editStmtsByLeftClick,
}:props) =>  {
    let (state, setState) = React.useState(_ => makeInitialState(~ctx, ~stmt, ~symColors))

    React.useEffect2(() => {
        setState(_ => makeInitialState(~ctx, ~stmt, ~symColors))
        None
    }, (settingsVer, ctxVer))

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
                        ~symRename?,
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