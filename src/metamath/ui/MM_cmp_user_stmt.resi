open MM_wrk_editor
open MM_wrk_settings
open MM_context
open Expln_React_Modal
open MM_substitution
open MM_parenCounter

let rndProofStatus: (
    ~proofStatus:option<proofStatus>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~readyTooltip:string=?,
    ~waitingTooltip:string=?,
    ~noJstfTooltip:string=?,
    ~jstfIsIncorrectTooltip:string=?,
    ~onReadyIconClicked:unit=>unit =?,
    ~onReadyIconAltClicked:unit=>unit=?,
    ~onWaitingIconClicked:unit=>unit=?,
    ~onErrorIconClicked:unit=>unit =?,
    ~onNoJstfIconClicked:unit=>unit =?
) => React.element

type viewOptions = {
    showCheckbox:bool,
    showLabel:bool,
    showType:bool,
    showJstf:bool,
    inlineMode:bool,
    smallBtns:bool,
}

type props = {
    modalRef:modalRef,

    settingsVer:int,
    settings:settings,
    preCtxVer:int,
    varsText:string,
    wrkCtx:option<mmContext>,
    frms: frms,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,
    parensMap:Belt_HashMapString.t<string>,
    typeColors:Belt_HashMapString.t<string>,
    preCtxColors:Belt_HashMapString.t<string>,
    wrkCtxColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,
    longClickEnabled:bool,
    longClickDelayMs:int,
    defaultStmtType:string,
    showVisByDefault:bool,

    viewOptions:viewOptions,
    readOnly:bool,
    parenAc:bool,
    toggleParenAc:unit=>unit,

    stmt:userStmt, 
    onLabelEditRequested:unit=>unit, 
    onLabelEditDone:string=>unit, 
    onLabelEditCancel:string=>unit,
    onTypEditRequested:unit=>unit, 
    onTypEditDone:(userStmtType,bool)=>unit,
    onContEditRequested:unit=>unit, 
    onContEditDone:string=>unit, 
    onContEditCancel:string=>unit,
    onSyntaxTreeUpdatedWithoutContentChange:stmtCont=>unit,
    onJstfEditRequested:unit=>unit, 
    onJstfEditDone:string=>unit, 
    onJstfEditCancel:string=>unit,

    checkboxDisabled:bool,
    checkboxChecked:bool,
    checkboxOnChange:(~checked:bool,~shift:bool)=>unit,

    onGenerateProof:unit=>unit,
    onDebug:unit=>unit,
    addStmtAbove:string=>unit,
    addStmtBelow:string=>unit,
    setShowTabs:bool=>unit,
    openFrameExplorer:string=>unit,
}

let make: props => React.element

let rndContText: (
    ~stmtCont:stmtCont,
    ~symRename:Belt_HashMapString.t<string>=?,
    ~onTextLeftClick:int=>unit=?,
    ~onTextAltLeftClick:int=>unit=?,
    ~onTreeLeftClick:int=>unit=?,
    ~onTreeAltLeftClick:int=>unit=?,
    ~longClickEnabled:bool=?,
    ~longClickDelayMs:int=?,
    ~cursor:string=?,
    ~renderSelection:bool=?,
    ~title:string=?
) => React.element

let getLastSyntaxType: unit => option<string>

let setLastSyntaxType: string => unit