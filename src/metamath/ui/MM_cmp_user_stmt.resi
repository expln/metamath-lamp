open MM_syntax_tree
open Expln_React_common
open Expln_React_Mui
open MM_wrk_editor

let rndProofStatus: (
    ~proofStatus:option<proofStatus>,
    ~readyTooltip:string=?,
    ~waitingTooltip:string=?,
    ~noJstfTooltip:string=?,
    ~jstfIsIncorrectTooltip:string=?,
    ~onReadyIconClicked:unit=>unit =?,
    ()
) => React.element

@react.component
let make: (
    ~stmt:userStmt, 
    ~onLabelEditRequested:unit=>unit, ~onLabelEditDone:string=>unit, ~onLabelEditCancel:string=>unit,
    ~onTypEditRequested:unit=>unit, ~onTypEditDone:userStmtType=>unit,
    ~onContEditRequested:unit=>unit, ~onContEditDone:string=>unit, ~onContEditCancel:string=>unit,
    ~onJstfEditRequested:unit=>unit, ~onJstfEditDone:string=>unit, ~onJstfEditCancel:string=>unit,
    ~onGenerateProof:unit=>unit,
) => React.element

let rndContText: stmtCont => React.element
