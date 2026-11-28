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

type foundAssertion = {
    label: string,
    isAxiom: bool,
    isDisc: bool,
    isDepr: bool,
    disj: array<array<string>>,
    hyps: array<string>,
    asrt: string,
}

let assertionToText = ({label, isAxiom, isDisc, isDepr, disj, hyps, asrt}:foundAssertion):string => {
    let marks = [
        isAxiom ? Some("axiom") : None,
        isDisc ? Some("discouraged") : None,
        isDepr ? Some("deprecated") : None,
    ]->Array.filterMap(mark => mark)
    let lines = [marks->Array.length == 0 ? label : `${label} (${marks->Array.join(", ")})`]
    if (disj->Array.length > 0) {
        lines->Array.push(`  disj: ${disj->Array.map(grp => grp->Array.join(" "))->Array.join(" , ")}`)
    }
    hyps->Array.forEach(hyp => lines->Array.push(`  hyp: ${hyp}`))
    lines->Array.push(`  asrt: ${asrt}`)
    lines->Array.join("\n")
}

type ctxFileInfo = {
    fileName: string,
    numOfAsrts: int,
    numOfAxioms: int,
}

type ctxSrcInfo = {
    @as("type") type_: string,
    url: string,
    fileName: string,
    alias: Nullable.t<string>,
    readInstr: string,
    label: Nullable.t<string>,
    numOfAsrts: int,
    numOfAxioms: int,
    files: array<ctxFileInfo>,
}

type loadedContextInfo = {
    numOfAsrts: int,
    numOfAxioms: int,
    srcs: array<ctxSrcInfo>,
}

type loadedContextInfoToTextParams = {
    info: loadedContextInfo,
    urlToLocalPath: string => option<string>,
}

let asrtCountsToText = (~numOfAsrts:int, ~numOfAxioms:int):string => {
    `${numOfAsrts->Int.toString} assertions (${numOfAxioms->Int.toString} axioms)`
}

/*
* Converts one source of the loaded context to text.
* idx - the index of the source in the order of loading, starting from 0.
* */
let ctxSrcToText = (src:ctxSrcInfo, ~idx:int, ~urlToLocalPath:string => option<string>):string => {
    let name = if (src.type_ == "local") {
        `${src.fileName} (a local file selected in metamath-lamp, its full path is unknown)`
    } else {
        switch urlToLocalPath(src.url) {
            | None => src.url
            | Some(localPath) => `${localPath} (served by this MCP server as ${src.url})`
        }
    }
    let label = src.label->Nullable.getOr("")
    let lines = [`${(idx + 1)->Int.toString}. ${name}`]
    src.alias->Nullable.toOption->Option.forEach(alias => lines->Array.push(`   alias: ${alias}`))
    lines->Array.push(
        switch src.readInstr {
            | "stopBefore" => `   read: up to '${label}', excluding it`
            | "stopAfter" => `   read: up to '${label}', including it`
            | _ => "   read: the whole file"
        }
    )
    lines->Array.push(`   loaded: ${asrtCountsToText(~numOfAsrts=src.numOfAsrts, ~numOfAxioms=src.numOfAxioms)}`)
    if (src.files->Array.length > 0) {
        lines->Array.push("   files (the first one is the file itself, others are included via $[ ... $]):")
        src.files->Array.forEach(file => {
            let counts = asrtCountsToText(~numOfAsrts=file.numOfAsrts, ~numOfAxioms=file.numOfAxioms)
            lines->Array.push(`     - ${file.fileName}: ${counts}`)
        })
    }
    lines->Array.join("\n")
}

let loadedContextInfoToText = ({info, urlToLocalPath}:loadedContextInfoToTextParams):string => {
    if (info.srcs->Array.length == 0) {
        "No context is loaded. Use load_context to load one."
    } else {
        [
            `Loaded: ${asrtCountsToText(~numOfAsrts=info.numOfAsrts, ~numOfAxioms=info.numOfAxioms)}.`,
            "Sources, in the order of loading:",
        ]->Array.concat(
            info.srcs->Array.mapWithIndex((src, idx) => ctxSrcToText(src, ~idx, ~urlToLocalPath))
        )->Array.join("\n")
    }
}

type getInsertIdxParams = {
    steps: array<step>,
    beforeLabel?: string,
    afterLabel?: string,
}

type insertIdxResult = {
    idx?: int,
    err?: string,
}

let getInsertIdx = ({steps, ?beforeLabel, ?afterLabel}:getInsertIdxParams):insertIdxResult => {
    let findIdx = label => steps->Array.findIndex(step => step.label == label)
    switch (beforeLabel, afterLabel) {
        | (Some(beforeLabel), _) => {
            let idx = findIdx(beforeLabel)
            idx < 0 ? {err: `No step with label '${beforeLabel}' exists.`} : {idx: idx}
        }
        | (None, Some(afterLabel)) => {
            let idx = findIdx(afterLabel)
            idx < 0 ? {err: `No step with label '${afterLabel}' exists.`} : {idx: idx + 1}
        }
        | (None, None) => {
            let goalIdx = steps->Array.findIndex(step => step.isGoal)
            {idx: goalIdx < 0 ? steps->Array.length : goalIdx}
        }
    }
}
