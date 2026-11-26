// The format is based on getEditorStateForLlm() from src/metamath/api/macros/llm_api_set_mm.js.

type stmtErr = {
    msg: Nullable.t<string>,
}

type step = {
    label: string,
    stmt: string,
    isHyp: bool,
    isGoal: bool,
    isBkm: bool,
    status: Nullable.t<string>,
    jstfText: Nullable.t<string>,
    stmtErr: Nullable.t<stmtErr>,
    syntaxErr: Nullable.t<string>,
    unifErr: Nullable.t<string>,
}

type editorState = {
    tabId: string,
    scope: string,
    scopeErr: Nullable.t<string>,
    descr: Nullable.t<string>,
    varsText: Nullable.t<string>,
    varsErr: Nullable.t<string>,
    disjText: Nullable.t<string>,
    disjErr: Nullable.t<string>,
    selectedSteps: array<string>,
    steps: array<step>,
}

let isNonEmptyStr = (value:Nullable.t<string>):bool => {
    value->Nullable.toOption->Option.mapOr(false, str => str->String.trim->String.length > 0)
}

let getStepErrors = (step:step):array<string> => {
    [
        step.stmtErr->Nullable.toOption->Option.flatMap(stmtErr => stmtErr.msg->Nullable.toOption),
        step.syntaxErr->Nullable.toOption,
        step.unifErr->Nullable.toOption,
    ]->Array.filterMap(err => err)->Array.filter(err => err->String.trim != "")
}

let stepIsRelevant = (step:step):bool => {
    step.isBkm
        || step.isHyp
        || step.isGoal
        || switch step.status->Nullable.toOption {
            | Some("?") | Some("~") | Some("x") => true
            | _ => false
        }
        || getStepErrors(step)->Array.length > 0
}

let getStepType = (step:step):string => {
    if (step.isGoal) {
        "G"
    } else if (step.isHyp) {
        "H"
    } else if (step.isBkm) {
        "P"
    } else {
        "p"
    }
}

let getStepStatus = (step:step):string => {
    if (step.isHyp) {
        "."
    } else {
        step.status->Nullable.getOr(".")
    }
}

/*
* Returns "    <prefix>: <text>" where continuation lines of a multiline text are aligned with its first line.
* */
let indentedField = (prefix:string, text:string):string => {
    let head = `    ${prefix}: `
    let pad = " "->String.repeat(head->String.length)
    text->String.trim
        ->String.split("\n")
        ->Array.mapWithIndex((line, i) => (i == 0 ? head : pad) ++ line)
        ->Array.join("\n")
}

let stepToText = (step:step, ~maxLabelLength:int):string => {
    let lines = [
        `${getStepType(step)} ${getStepStatus(step)} ${step.label->String.padEnd(maxLabelLength, " ")} ${step.stmt}`
    ]
    if (!step.isHyp && isNonEmptyStr(step.jstfText)) {
        lines->Array.push(indentedField("jstf", step.jstfText->Nullable.getOr("")))
    }
    getStepErrors(step)->Array.forEach(err => lines->Array.push(indentedField("error", err)))
    lines->Array.join("\n")
}

let pushSection = (
    res:array<string>,
    title:string,
    text:Nullable.t<string>,
    ~err:Nullable.t<string>=Nullable.undefined
):unit => {
    if (isNonEmptyStr(text) || isNonEmptyStr(err)) {
        res->Array.push(`${title}:`)
        if (isNonEmptyStr(text)) {
            res->Array.push(text->Nullable.getOr("")->String.trim)
        }
        if (isNonEmptyStr(err)) {
            res->Array.push(indentedField("error", err->Nullable.getOr("")))
        }
        res->Array.push("")
    }
}

type editorStateToTextParams = {
    state: editorState,
    onlyRelevantSteps?: bool,
    notes?: array<string>,
}

let editorStateToText = ({state, ?onlyRelevantSteps, ?notes}:editorStateToTextParams):string => {
    let allSteps = state.steps
    let steps = onlyRelevantSteps->Option.getOr(false) ? allSteps->Array.filter(stepIsRelevant) : allSteps
    let maxLabelLength = steps->Array.reduce(0, (max, step) => Math.Int.max(max, step.label->String.length))
    let res = ["--- editor state begin ---"]
    res->Array.push(`Editor tab id: ${state.tabId}`)
    res->Array.push(`Scope: ${state.scope}`)
    if (isNonEmptyStr(state.scopeErr)) {
        res->Array.push(`Scope error: ${state.scopeErr->Nullable.getOr("")}`)
    }
    notes->Option.getOr([])->Array.forEach(note => res->Array.push(`Note: ${note}`))
    res->Array.push("")
    res->pushSection("Description", state.descr)
    res->pushSection("Variables", state.varsText, ~err=state.varsErr)
    res->pushSection("Disjoints", state.disjText, ~err=state.disjErr)
    if (state.selectedSteps->Array.length > 0) {
        res->Array.push(`Selected steps: ${state.selectedSteps->Array.join(" ")}`)
        res->Array.push("")
    }
    let numOfSteps = steps->Array.length->Int.toString
    let numOfAllSteps = allSteps->Array.length->Int.toString
    if (steps->Array.length < allSteps->Array.length) {
        res->Array.push(`Steps (${numOfSteps} of ${numOfAllSteps} shown, proved non-bookmarked steps are hidden):`)
    } else {
        res->Array.push(`Steps (${numOfAllSteps}):`)
    }
    steps->Array.forEach(step => res->Array.push(stepToText(step, ~maxLabelLength)))
    res->Array.push("--- editor state end ---")
    res->Array.join("\n")
}
