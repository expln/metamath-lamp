open MM_wrk_editor
open MM_syntax_tree
open MM_apply_asrt_matcher

type proverParams = {
    delayBeforeStartMs:int,
    stmtId: stmtId,
    debugLevel:int,
    bottomUpProverParams: MM_provers.bottomUpProverParams,
    selectFirstFoundProof:bool,
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
    ~startProvingBottomUp:proverParams=>promise<option<bool>>,
    ~canStartUnifyAll:bool,
    ~startUnifyAll:unit=>promise<unit>,
    ~buildSyntaxTrees:array<string>=>result<array<result<syntaxTreeNode,string>>,string>,
    ~getAsrtSyntaxTrees:()=>promise<Belt_HashMapString.t<syntaxTreeNode>>,
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