open MM_wrk_editor
open MM_context
open Expln_React_Modal
open MM_react_common
open MM_substitution
open MM_parenCounter

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

@react.component
let make: (
    ~modalRef:modalRef,
    ~wrkCtx:option<mmContext>,
    ~frms: Belt_MapString.t<frmSubsData>,
    ~parenCnt: parenCnt,
    ~syntaxTypes:array<int>,
    ~parensMap:Belt_HashMapString.t<string>,
    ~stmt:userStmt, 
    ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit, ~onLabelEditCancel:string=>unit,
    ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~onContEditRequested:unit=>unit, ~onContEditDone:string=>unit, ~onContEditCancel:string=>unit,
    ~onSyntaxTreeUpdated:stmtCont=>unit,
    ~onJstfEditRequested:unit=>unit, ~onJstfEditDone:string=>unit, ~onJstfEditCancel:string=>unit,
    ~onGenerateProof:unit=>unit,
    ~onDebug:unit=>unit,
    ~typeColors:Belt_HashMapString.t<string>,
    ~preCtxColors:Belt_HashMapString.t<string>,
    ~wrkCtxColors:Belt_HashMapString.t<string>,
    ~visualizationIsOn:bool,
) => React.element

let rndContText: (
    ~stmtCont:stmtCont,
    ~onTextClick:int=>unit=?,
    ~onTreeClick:int=>unit=?,
    ()
) => React.element
