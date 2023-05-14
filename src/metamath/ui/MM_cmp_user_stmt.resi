open MM_wrk_editor
open MM_context
open Expln_React_Modal
open MM_substitution
open MM_parenCounter
open MM_syntax_tree

let rndProofStatus: (
    ~proofStatus:option<proofStatus>,
    ~readyTooltip:string=?,
    ~waitingTooltip:string=?,
    ~noJstfTooltip:string=?,
    ~jstfIsIncorrectTooltip:string=?,
    ~onReadyIconClicked:unit=>unit =?,
    ~onErrorIconClicked:unit=>unit =?,
    ~onNoJstfIconClicked:unit=>unit =?,
    ()
) => React.element

type props = {
    modalRef:modalRef,

    settingsVer:int,
    preCtxVer:int,
    varsText:string,
    wrkCtx:option<mmContext>,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,
    parensMap:Belt_HashMapString.t<string>,
    typeColors:Belt_HashMapString.t<string>,
    preCtxColors:Belt_HashMapString.t<string>,
    wrkCtxColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    defaultStmtType:string,

    visualizationIsOn:bool,

    stmt:userStmt, 
    onLabelEditRequested:unit=>unit, 
    onLabelEditDone:string=>unit, 
    onLabelEditCancel:string=>unit,
    onTypEditRequested:unit=>unit, 
    onTypEditDone:userStmtType=>unit,
    onContEditRequested:unit=>unit, 
    onContEditDone:string=>unit, 
    onContEditCancel:string=>unit,
    onSyntaxTreeUpdated:stmtCont=>unit,
    onJstfEditRequested:unit=>unit, 
    onJstfEditDone:string=>unit, 
    onJstfEditCancel:string=>unit,

    onGenerateProof:unit=>unit,
    onDebug:unit=>unit,
    addStmtAbove:string=>unit,
    addStmtBelow:string=>unit,
}

let make: props => React.element

let rndContText: (
    ~stmtCont:stmtCont,
    ~symRename:Belt_HashMapString.t<string>=?,
    ~onTextClick:int=>unit=?,
    ~onTreeClick:int=>unit=?,
    ~editStmtsByLeftClick:bool=?,
    ~renderSelection:bool=?,
    ()
) => React.element

let textToSyntaxTree: (
    ~wrkCtx:mmContext,
    ~syms:array<stmtSym>,
    ~syntaxTypes:array<int>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~lastSyntaxType:option<string>,
    ~onLastSyntaxTypeChange:string => unit,
) => result<syntaxTreeNode,string>

let getLastSyntaxType: unit => option<string>

let setLastSyntaxType: string => unit