open MM_context
open MM_wrk_editor
open MM_wrk_settings
open MM_parenCounter

type userStmtLocStor = {
    label: string,
    typ: string,
    cont: string,
    jstfText: string,
}   

type editorStateLocStor = {
    varsText: string,
    disjText: string,
    stmts: array<userStmtLocStor>,
}

let userStmtLocStorToUserStmt = (userStmtLocStor:userStmtLocStor):userStmt => {
    {
        id: "",

        label: userStmtLocStor.label,
        labelEditMode: false,
        typ: userStmtTypeFromStr(userStmtLocStor.typ),
        typEditMode: false,
        cont: strToCont(userStmtLocStor.cont, ()),
        contEditMode: false,

        jstfText: userStmtLocStor.jstfText,
        jstfEditMode: false,

        stmtErr: None,

        expr: None,
        jstf: None,
        proof: None,
        proofStatus: None,
        unifErr: None,
    }
}

let createInitialEditorState = (
    ~settingsV:int, 
    ~settings:settings, 
    ~preCtxV:int, 
    ~preCtx:mmContext, 
    ~stateLocStor:option<editorStateLocStor>
) => {
    let st = {
        settingsV,
        settings,
        typeColors: Belt_HashMapString.make(~hintSize=0),

        preCtxV,
        preCtx,
        frms: Belt_MapString.empty,
        parenCnt: parenCntMake([], ()),
        preCtxColors: Belt_HashMapString.make(~hintSize=0),

        varsText: stateLocStor->Belt.Option.map(obj => obj.varsText)->Belt.Option.getWithDefault(""),
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: stateLocStor->Belt.Option.map(obj => obj.disjText)->Belt.Option.getWithDefault(""),
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: stateLocStor
            ->Belt.Option.map(stateLocStor => stateLocStor.stmts->Js_array2.length)
            ->Belt.Option.getWithDefault(0),
        stmts: 
            stateLocStor
                ->Belt.Option.map(obj => obj.stmts->Js_array2.mapi((stmtLocStor,i) => {
                    {
                        ...userStmtLocStorToUserStmt(stmtLocStor),
                        id: i->Belt_Int.toString
                    }
                }))
                ->Belt.Option.getWithDefault([]),
        checkedStmtIds: [],

        unifyAllIsRequiredCnt: 0,
    }
    let st = st->setSettings(settingsV, settings)
    let st = st->setPreCtx(preCtxV, preCtx)
    st
}

let editorStateToEditorStateLocStor = (state:editorState):editorStateLocStor => {
    {
        varsText: state.varsText,
        disjText: state.disjText,
        stmts: state.stmts->Js_array2.map(stmt => {
            {
                label: stmt.label,
                typ: (stmt.typ->userStmtTypeToStr),
                cont: contToStr(stmt.cont),
                jstfText: stmt.jstfText,
            }
        }),
    }
}

let readEditorStateFromJsonStr = (jsonStr:string):option<editorStateLocStor> => {
    open Expln_utils_jsonParse
    let parseResult = parseJson(jsonStr, asObj(_, d=>{
        {
            varsText: d->str("varsText", ~default=()=>"", ()),
            disjText: d->str("disjText", ~default=()=>"", ()),
            stmts: d->arr("stmts", asObj(_, d=>{
                {
                    label: d->str("label", ()),
                    typ: d->str("typ", ()),
                    cont: d->str("cont", ()),
                    jstfText: d->str("jstfText", ())
                }
            }, ()), ())
        }
    }, ()), ())
    switch parseResult {
        | Error(_) => None
        | Ok(res) => Some(res)
    }
}
