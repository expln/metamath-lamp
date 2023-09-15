open MM_wrk_editor
open MM_parenCounter
open MM_wrk_pre_ctx_data

type userStmtLocStor = {
    label: string,
    typ: string,
    isGoal: bool,
    cont: string,
    jstfText: string,
}   

type editorStateLocStor = {
    srcs: array<mmCtxSrcDto>,
    descr: string,
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
        isGoal: userStmtLocStor.isGoal,
        cont: strToCont(userStmtLocStor.cont, ()),
        contEditMode: false,
        isDuplicated: false,

        jstfText: userStmtLocStor.jstfText,
        jstfEditMode: false,

        stmtErr: None,

        expr: None,
        jstf: None,
        proofTreeDto: None,
        src: None,
        proof: None,
        proofStatus: None,
        unifErr: None,
        syntaxErr: None,
    }
}

let createInitialEditorState = (
    ~preCtxData:preCtxData, 
    ~stateLocStor:option<editorStateLocStor>,
) => {
    let st = {
        settingsV:preCtxData.settingsV.ver,
        settings:preCtxData.settingsV.val,
        typeColors: Belt_HashMapString.make(~hintSize=0),

        srcs:preCtxData.srcs,
        preCtxV:preCtxData.ctxV.ver,
        preCtx:preCtxData.ctxV.val,
        frms: Belt_MapString.empty,
        parenCnt: parenCntMake(~parenMin=0),
        preCtxColors: Belt_HashMapString.make(~hintSize=0),
        allTypes: [],
        syntaxTypes: [],
        parensMap:Belt_HashMapString.make(~hintSize=0),

        descr: stateLocStor->Belt.Option.map(obj => obj.descr)->Belt.Option.getWithDefault(""),
        descrEditMode: false,

        varsText: stateLocStor->Belt.Option.map(obj => obj.varsText)->Belt.Option.getWithDefault(""),
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: stateLocStor->Belt.Option.map(obj => obj.disjText)->Belt.Option.getWithDefault(""),
        disjEditMode: false,
        disjErr: None,

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
        continueMergingStmts: 0,
    }
    let st = st->setPreCtxData(preCtxData)
    st
}

let editorStateToEditorStateLocStor = (state:editorState):editorStateLocStor => {
    {
        srcs: state.srcs->Js.Array2.map(src => {...src, ast:None, allLabels:[]}),
        descr:state.descr,
        varsText: state.varsText,
        disjText: state.disjText,
        stmts: state.stmts->Js_array2.map(stmt => {
            {
                label: stmt.label,
                typ: (stmt.typ->userStmtTypeToStr),
                isGoal: stmt.isGoal,
                cont: contToStr(stmt.cont),
                jstfText: stmt.jstfText,
            }
        }),
    }
}

let readEditorStateFromJsonStr = (jsonStr:string):result<editorStateLocStor,string> => {
    open Expln_utils_jsonParse
    parseJson(jsonStr, asObj(_, d=>{
        {
            srcs: d->arr("srcs", asObj(_, d=>{
                {
                    typ: d->str("typ", ()),
                    fileName: d->str("fileName", ()),
                    url: d->str("url", ()),
                    readInstr: d->str("readInstr", ()),
                    label: d->str("label", ()),
                    resetNestingLevel: d->bool("resetNestingLevel", ~default=()=>true, ()),
                    ast: None,
                    allLabels: [],
                }
            }, ()), ~default=()=>[], ()),
            descr: d->str("descr", ~default=()=>"", ()),
            varsText: d->str("varsText", ~default=()=>"", ()),
            disjText: d->str("disjText", ~default=()=>"", ()),
            stmts: d->arr("stmts", asObj(_, d=>{
                {
                    label: d->str("label", ()),
                    typ: d->str("typ", ()),
                    isGoal: d->bool("isGoal", ~default=()=>false, ()),
                    cont: d->str("cont", ()),
                    jstfText: d->str("jstfText", ())
                }
            }, ()), ())
        }
    }, ()), ())
}
