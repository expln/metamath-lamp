open MM_parser
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_wrk_settings
open MM_react_common
open MM_cmp_type_settings

type settingsState = {
    nextId: int,

    parens: string,
    parensErr: option<string>,

    typeSettings: array<typeSettingsState>,
}

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

let createDefaultSettings = () => {
    {
        nextId: 4,
        parens: "( ) [ ] { }",
        parensErr: None,
        typeSettings: [
            {
                id: "0",
                typ: "wff",
                color: "#4363d8",
                prefix: "&W",
                err: None,
            },
            {
                id: "1",
                typ: "term",
                color: "#000000",
                prefix: "&T",
                err: None,
            },
            {
                id: "2",
                typ: "setvar",
                color: "#e6194B",
                prefix: "&S",
                err: None,
            },
            {
                id: "3",
                typ: "class",
                color: "#f032e6",
                prefix: "&C",
                err: None,
            },
        ],
    }
}

let strContainsWhitespaceRegex = %re("/\s+/")
let validateAndCorrectTypeSetting = (ts:typeSettingsState):typeSettingsState => {
    let newId = switch Belt_Int.fromString(ts.id) {
        | None => "0"
        | Some(_) => ts.id
    }
    let newTyp = ts.typ->Js_string2.trim
    let typHasWhitespace = newTyp->Js_string2.match_(strContainsWhitespaceRegex)->Belt.Option.isSome
    let typIsEmpty = newTyp->Js_string2.length == 0
    let newColor = if (!(allColors->Js_array2.includes(ts.color))) {
        allColors[0]
    } else {
        ts.color
    }
    let newPrefix = ts.prefix->Js_string2.trim
    let prefixHasWhitespace = newPrefix->Js_string2.match_(strContainsWhitespaceRegex)->Belt.Option.isSome
    let prefixIsEmpty = newPrefix->Js_string2.length == 0
    let err = if (typHasWhitespace) {
        Some("Type should not contain whitespaces.")
    } else if (typIsEmpty) {
        Some("Type should not be empty.")
    } else if (prefixHasWhitespace) {
        Some("Work variable prefix should not contain whitespaces.")
    } else if (prefixIsEmpty) {
        Some("Work variable prefix should not be empty.")
    } else {
        None
    }

    {
        ...ts,
        typ: newTyp,
        color: newColor,
        prefix: newPrefix,
        err
    }
}

let validateAndCorrectState = (st:settingsState):settingsState => {
    let validatedTypeSettings = st.typeSettings->Js_array2.map(validateAndCorrectTypeSetting)
    let distinctIds = Belt_SetInt.fromArray(
        validatedTypeSettings->Js_array2.map(ts => ts.id->Belt_Int.fromString->Belt.Option.getWithDefault(0))
    )
    let validatedTypeSettings = if (distinctIds->Belt_SetInt.size != validatedTypeSettings->Js_array2.length) {
        let maxId = distinctIds->Belt_SetInt.maximum->Belt.Option.getWithDefault(0)
        validatedTypeSettings->Js_array2.mapi((ts,i) => {...ts, id:(maxId+i)->Belt_Int.toString})
    } else {
        validatedTypeSettings
    }
    let maxTypSettId = validatedTypeSettings->Js_array2.reduce(
        (maxId,ts) => {
            let id = switch ts.id->Belt_Int.fromString {
                | None => raise(MmException({msg:`Cannot convert string to int for typeSettingsState.id`}))
                | Some(id) => id
            }
            if (id <= maxId) { maxId } else { id }
        },
        0
    )
    let newNextId = if (maxTypSettId < st.nextId) {st.nextId} else {maxTypSettId + 1}
    let newParens = st.parens->Js_string2.trim
    let parensErr = if (mod(newParens->getSpaceSeparatedValuesAsArray->Js.Array2.length, 2) == 0) {
        None
    } else {
        Some("Number of parentheses must be even.")
    }

    {
        ...st,
        nextId: newNextId,
        parens: newParens,
        parensErr,
        typeSettings: validatedTypeSettings,
    }
}

let stateToSettings = (st:settingsState):settings => {
    {
        parens: st.parens,
        typeSettings: st.typeSettings->Js_array2.map(typSett => {
            typ: typSett.typ,
            color: typSett.color,
            prefix: typSett.prefix,
        }),
    }
}

let settingsToState = (ls:settings):settingsState => {
    let res = {
        nextId: 0,
        parens: ls.parens,
        parensErr: None,
        typeSettings: ls.typeSettings->Js_array2.map(lts => {
            id: "0",
            typ: lts.typ,
            color: lts.color,
            prefix: lts.prefix,
            err: None,
        }),
    }
    validateAndCorrectState(res)
}

let settingsLocStorKey = "settings"

let saveStateToLocStor = (st:settingsState):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(settingsLocStorKey, Expln_utils_common.stringify(
        stateToSettings(st)
    ))
}

let readStateFromLocStor = ():settingsState => {
    let defaultSettings = createDefaultSettings()
    switch Dom_storage2.localStorage->Dom_storage2.getItem(settingsLocStorKey) {
        | None => defaultSettings
        | Some(settingsLocStorStr) => {
            open Expln_utils_jsonParse
            let parseResult:result<settingsState,string> = parseJson(settingsLocStorStr, asObj(_, d=>{
                {
                    nextId: 0,
                    parens: d->str("parens", ~default=()=>defaultSettings.parens, ()),
                    parensErr: None,
                    typeSettings: d->arr("typeSettings", asObj(_, d=>{
                        id: "0",
                        typ: d->str("typ", ()),
                        color: d->str("color", ()),
                        prefix: d->str("prefix", ()),
                        err: None,
                    }, ()), ~default=()=>defaultSettings.typeSettings, ()),
                }
            }, ()), ~default=()=>defaultSettings, ())
            switch parseResult {
                | Error(msg) => raise(MmException({msg:`Cannot read settings from the local storage: ${msg}`}))
                | Ok(state) => {
                    validateAndCorrectState(state)
                }
            }
        }
    }
}

let settingsReadFromLocStor = () => readStateFromLocStor()->stateToSettings

let isValid = st => {
    st.parensErr->Belt_Option.isNone
        && st.typeSettings->Js_array2.every(ts => ts.err->Belt_Option.isNone)
}

let eqTypeSetting = (ts1:typeSettingsState, ts2:typeSettingsState):bool => {
    ts1.typ == ts2.typ
        && ts1.color == ts2.color
        && ts1.prefix == ts2.prefix
}

let eqState = (st1, st2) => {
    st1.parens == st2.parens
        && st1.typeSettings->Js_array2.length == st2.typeSettings->Js_array2.length
        && st1.typeSettings->Js_array2.everyi((ts1,i) => eqTypeSetting(ts1, st2.typeSettings[i]))
}

let updateParens = (st,parens) => {
    {
        ...st,
        parens,
        parensErr: None,
    }
}

let updateTypeSetting = (st,id,update:typeSettingsState=>typeSettingsState) => {
    {
        ...st,
        typeSettings: st.typeSettings->Js_array2.map(ts => if (ts.id == id) { update(ts) } else { ts })
    }
}

let updateType = (st,id,typ) => {
    updateTypeSetting(st, id, ts => {...ts, typ, err:None})
}

let updateColor = (st,id,color) => {
    updateTypeSetting(st, id, ts => {...ts, color, err:None})
}

let updatePrefix = (st,id,prefix) => {
    updateTypeSetting(st, id, ts => {...ts, prefix, err:None})
}

let addTypeSetting = st => {
    let newId = st.nextId->Belt_Int.toString
    {
        ...st,
        nextId: st.nextId + 1,
        typeSettings: st.typeSettings->Js_array2.concat([{
            id: newId,
            typ: "",
            color: allColors[0],
            prefix: "",
            err: None,
        }]),
    }
}

let deleteTypeSetting = (st, id) => {
    {
        ...st,
        typeSettings: st.typeSettings->Js_array2.filter(ts => ts.id != id)
    }
}

@react.component
let make = (~modalRef:modalRef, ~ctx:mmContext, ~initialSettings:settings, ~onChange: settings => unit) => {
    let (state, setState) = React.useState(_ => initialSettings->settingsToState)
    let (prevState, setPrevState) = React.useState(_ => state)

    let applyChanges = () => {
        let st = validateAndCorrectState(state)
        setState(_ => st)
        if (st->isValid) {
            saveStateToLocStor(st)
            setPrevState(_ => st)
            onChange(st->stateToSettings)
        }
    }
    
    let discardChanges = () => {
        setState(_ => prevState)
    }

    let actParensChange = parens => {
        setState(updateParens(_, parens))
    }

    let actTypeSettingAdd = () => {
        setState(addTypeSetting)
    }

    let actTypeSettingDelete = id => {
        setState(deleteTypeSetting(_, id))
    }

    let actTypeChange = (id,typ) => {
        setState(updateType(_, id, typ))
    }

    let actColorChange = (id,color) => {
        setState(updateColor(_, id, color))
    }

    let actPrefixChange = (id,prefix) => {
        setState(updatePrefix(_, id, prefix))
    }

    let makeActTerminate = (modalId:option<modalId>):option<unit=>unit> => {
        modalId->Belt.Option.map(modalId => () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        })
    }

    let rndFindParensProgress = (pct, modalIdOpt) => {
        rndProgress(~text=`Searching parentheses`, ~pct, ~onTerminate=?makeActTerminate(modalIdOpt), ())
    }

    let syncParens = () => {
        openModal(modalRef, _ => rndFindParensProgress(0., None))->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => rndFindParensProgress(0., Some(modalId)))
            MM_wrk_FindParens.beginFindParens(
                ~ctx,
                ~onProgress = pct => updateModal(modalRef, modalId, () => rndFindParensProgress(pct, Some(modalId))),
                ~onDone = parens => {
                    actParensChange(parens)
                    closeModal(modalRef, modalId)
                }
            )
        })->ignore
    }

    <Col spacing=1. style=ReactDOM.Style.make(~margin="30px", ())>
        <Row alignItems=#center>
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="400px", ())
                label="Parentheses" 
                value=state.parens 
                onChange=evt2str(actParensChange)
                error={state.parensErr->Belt_Option.isSome}
                title="Parentheses are used to speed up finding of substitutions"
            />
            <span title="Determine parentheses from the loaded MM file">
                <IconButton onClick={_ => syncParens()}>
                    <MM_Icons.Sync/>
                </IconButton>
            </span>
        </Row>
        {
            switch state.parensErr {
                | None => React.null
                | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
            }
        }
        <Divider/>
        <MM_cmp_type_settings
            typeSettings=state.typeSettings
            availableColors=allColors
            onAdd=actTypeSettingAdd
            onTypeChange=actTypeChange
            onColorChange=actColorChange
            onPrefixChange=actPrefixChange
            onDelete=actTypeSettingDelete
        />
        <Divider/>
        {
            if (!eqState(prevState, state)) {
                <Row spacing=3. >
                    <Button disabled={!isValid(state)} onClick={_=>applyChanges()} variant=#contained>
                        {React.string("Apply changes")}
                    </Button>
                    <Button onClick={_ => discardChanges()}>
                        {React.string("Discard changes")}
                    </Button>
                </Row>
            } else {
                React.null
            }
        }
    </Col>
}