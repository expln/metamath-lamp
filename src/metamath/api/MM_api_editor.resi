open MM_wrk_editor
open MM_syntax_tree
open MM_apply_asrt_matcher_type

type proverParams = {
    delayBeforeStartMs:int,
    stmtId: stmtId,
    debugLevel:int,
    bottomUpProverParams: MM_bottom_up_prover_params.bottomUpProverParams,
    /* 
    selectFirstFoundProof == true: if a proof is found, add it to the editor. In any case close the dialog 
        (even if a proof is not found).
    selectFirstFoundProof == false: if a proof found, do not add it to the editor. Leave the dialog open in any case
        (even if a proof is found).
    selectFirstFoundProof == null: if a proof is found, add it to the editor and close the dialog;
        if a proof is not found, leave the dialog open.
    */
    selectFirstFoundProof:option<bool>,
}

let deleteEditor: int => unit
let setLastOpenedEditorId: int => unit

let updateEditorData: (
    ~editorId:int,
    ~unifMetavarPrefix:string,
    ~state:editorState,
    ~setState:(editorState=>result<(editorState,JSON.t),string>)=>promise<result<JSON.t,string>>,
    ~setEditorContIsHidden:bool=>promise<unit>,
    ~canStartProvingBottomUp:bool,
    ~startProvingBottomUp:proverParams=>promise<result<bool,string>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~getAsrtSyntaxTrees:()=>promise<Belt_HashMapString.t<syntaxTreeNode>>,
    ~addAsrtByLabel: string=>promise<result<unit,string>>,
) => unit

let stmtToJson: (userStmt, option<int=>string>) => JSON.t

type apiApplyAsrtResultHypMatcher = {
    label: option<string>,
    idx: option<int>,
    pat: string,
}

type apiApplyAsrtResultMatcher = {
    res: option<string>,
    hyps: array<apiApplyAsrtResultHypMatcher>,
}

let optArrayToMatchers: (
    ~state:editorState,
    ~matches:option<array<apiApplyAsrtResultMatcher>>,
) => result<option<array<applyAsrtResultMatcher>>,string>