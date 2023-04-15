open MM_parser
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_wrk_settings
open MM_react_common
open MM_cmp_type_settings
open MM_cmp_web_src_settings
open Common

type settingsState = {
    parens: string,
    parensErr: option<string>,

    asrtsToSkip: array<string>,
    asrtsToSkipRegex: string,

    typeNextId: int,
    typeSettings: array<typeSettingsState>,

    webSrcNextId: int,
    webSrcSettings: array<webSrcSettingsState>,

}

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

let asrtsToSkipRegexDefault = "New usage of \"([^\"]+)\" is discouraged"

let createDefaultSettings = () => {
    {
        parens: "( ) [ ] { }",
        parensErr: None,
        asrtsToSkip: [],
        asrtsToSkipRegex: asrtsToSkipRegexDefault,
        typeNextId: 4,
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
        webSrcNextId: 1,
        webSrcSettings: [
            {
                id: "0",
                alias: "us.metamath.org:set.mm",
                url: "https://us.metamath.org/metamath/set.mm",
                trusted: false,
                err: None
            }
        ]
    }
}

let validateAndCorrectWebSrcSetting = (src:webSrcSettingsState):webSrcSettingsState => {
    let newAlias = src.alias->Js.String2.trim
    let newUrl = src.url->Js.String2.trim
    let err = if (newUrl->Js_string2.length == 0) {
        Some("URL should not be empty.")
    } else {
        None
    }

    {
        ...src,
        alias: newAlias,
        url: newUrl,
        err
    }
}

let validateAndCorrectParens = (st:settingsState):settingsState => {
    let newParens = st.parens->Js_string2.trim
    let parensErr = if (mod(newParens->getSpaceSeparatedValuesAsArray->Js.Array2.length, 2) == 0) {
        None
    } else {
        Some("Number of parentheses must be even.")
    }

    {
        ...st,
        parens: newParens,
        parensErr,
    }
}

let validateAndCorrectTypeSettings = (st:settingsState):settingsState => {
    let strContainsWhitespaceRegex = %re("/\s+/")
    let validateAndCorrectTypeSetting = (ts:typeSettingsState):typeSettingsState => {
        let newId = ts.id->Belt_Int.fromString->Belt.Option.getWithDefault(0)->Belt_Int.toString
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
            id: newId,
            typ: newTyp,
            color: newColor,
            prefix: newPrefix,
            err
        }
    }

    let validatedTypeSettings = st.typeSettings->Js_array2.map(validateAndCorrectTypeSetting)
    let distinctTypeIds = Belt_SetInt.fromArray(
        validatedTypeSettings->Js_array2.map(ts => ts.id->Belt_Int.fromString->Belt.Option.getExn)
    )
    let validatedTypeSettings = if (distinctTypeIds->Belt_SetInt.size == validatedTypeSettings->Js_array2.length) {
        validatedTypeSettings
    } else {
        let maxId = distinctTypeIds->Belt_SetInt.maximum->Belt.Option.getWithDefault(0)
        validatedTypeSettings->Js_array2.mapi((ts,i) => {...ts, id:(maxId+i+1)->Belt_Int.toString})
    }
    let maxTypSettId = validatedTypeSettings->Js_array2.reduce(
        (maxId,ts) => {
            let id = ts.id->Belt_Int.fromString->Belt.Option.getExn
            if (id <= maxId) { maxId } else { id }
        },
        0
    )
    let newNextId = if (maxTypSettId < st.typeNextId) {st.typeNextId} else {maxTypSettId + 1}

    {
        ...st,
        typeNextId: newNextId,
        typeSettings: validatedTypeSettings,
    }
}

let validateAndCorrectWebSrcSettings = (st:settingsState):settingsState => {
    let validateAndCorrectWebSrcSetting = (src:webSrcSettingsState):webSrcSettingsState => {
        let newId = src.id->Belt_Int.fromString->Belt.Option.getWithDefault(0)->Belt_Int.toString
        let newAlias = src.alias->Js.String2.trim
        let newUrl = src.url->Js.String2.trim
        let err = if (newUrl->Js_string2.length == 0) {
            Some("URL should not be empty.")
        } else {
            None
        }

        {
            id: newId,
            alias: newAlias,
            url: newUrl,
            trusted: src.trusted,
            err
        }
    }

    let validatedWebSrcSettings = st.webSrcSettings->Js_array2.map(validateAndCorrectWebSrcSetting)
    let distinctIds = Belt_SetInt.fromArray(
        validatedWebSrcSettings->Js_array2.map(src => src.id->Belt_Int.fromString->Belt.Option.getExn)
    )
    let validatedWebSrcSettings = if (distinctIds->Belt_SetInt.size == validatedWebSrcSettings->Js_array2.length) {
        validatedWebSrcSettings
    } else {
        let maxId = distinctIds->Belt_SetInt.maximum->Belt.Option.getWithDefault(0)
        validatedWebSrcSettings->Js_array2.mapi((src,i) => {...src, id:(maxId+i+1)->Belt_Int.toString})
    }
    let maxId = validatedWebSrcSettings->Js_array2.reduce(
        (maxId,src) => {
            let id = src.id->Belt_Int.fromString->Belt.Option.getExn
            if (id <= maxId) { maxId } else { id }
        },
        0
    )
    let newNextId = if (maxId < st.webSrcNextId) {st.webSrcNextId} else {maxId + 1}

    {
        ...st,
        webSrcNextId: newNextId,
        webSrcSettings: validatedWebSrcSettings,
    }
}

let validateAndCorrectState = (st:settingsState):settingsState => {
    let st = validateAndCorrectParens(st)
    let st = validateAndCorrectTypeSettings(st)
    let st = validateAndCorrectWebSrcSettings(st)
    st
}

let stateToSettings = (st:settingsState):settings => {
    {
        parens: st.parens,
        asrtsToSkip: st.asrtsToSkip,
        asrtsToSkipRegex: st.asrtsToSkipRegex,
        typeSettings: st.typeSettings->Js_array2.map(typSett => {
            typ: typSett.typ,
            color: typSett.color,
            prefix: typSett.prefix,
        }),
        webSrcSettings: st.webSrcSettings->Js_array2.map(s => {
            alias: s.alias,
            url: s.url,
            trusted: s.trusted,
        }),
    }
}

let settingsToState = (ls:settings):settingsState => {
    let res = {
        parens: ls.parens,
        parensErr: None,
        asrtsToSkip: ls.asrtsToSkip,
        asrtsToSkipRegex: ls.asrtsToSkipRegex,
        typeNextId: 0,
        typeSettings: ls.typeSettings->Js_array2.map(lts => {
            id: "0",
            typ: lts.typ,
            color: lts.color,
            prefix: lts.prefix,
            err: None,
        }),
        webSrcNextId: 0,
        webSrcSettings: ls.webSrcSettings->Js_array2.map(s => {
            id: "0",
            alias: s.alias,
            url: s.url,
            trusted: s.trusted,
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
                    parens: d->str("parens", ~default=()=>defaultSettings.parens, ()),
                    parensErr: None,
                    asrtsToSkip: d->arr("asrtsToSkip", asStr(_, ()), ~default=()=>[], ()),
                    asrtsToSkipRegex: d->str("asrtsToSkipRegex", ~default=()=>asrtsToSkipRegexDefault, ()),
                    typeNextId: 0,
                    typeSettings: d->arr("typeSettings", asObj(_, d=>{
                        id: "0",
                        typ: d->str("typ", ()),
                        color: d->str("color", ()),
                        prefix: d->str("prefix", ()),
                        err: None,
                    }, ()), ~default=()=>defaultSettings.typeSettings, ()),
                    webSrcNextId: 0,
                    webSrcSettings: d->arr("webSrcSettings", asObj(_, d=>{
                        id: "0",
                        alias: d->str("alias", ()),
                        url: d->str("url", ()),
                        trusted: d->bool("trusted", ()),
                        err: None,
                    }, ()), ~default=()=>defaultSettings.webSrcSettings, ()),
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
        && st.webSrcSettings->Js_array2.every(s => s.err->Belt_Option.isNone)
}

let eqTypeSetting = (ts1:typeSettingsState, ts2:typeSettingsState):bool => {
    ts1.typ == ts2.typ
        && ts1.color == ts2.color
        && ts1.prefix == ts2.prefix
}

let eqWebSrcSetting = (ts1:webSrcSettingsState, ts2:webSrcSettingsState):bool => {
    ts1.alias == ts2.alias
        && ts1.url == ts2.url
        && ts1.trusted == ts2.trusted
}

let eqState = (st1, st2) => {
    st1.parens == st2.parens
        && st1.asrtsToSkip == st2.asrtsToSkip
        && st1.asrtsToSkipRegex == st2.asrtsToSkipRegex
        && st1.typeSettings->Js_array2.length == st2.typeSettings->Js_array2.length
        && st1.typeSettings->Js_array2.everyi((ts1,i) => eqTypeSetting(ts1, st2.typeSettings[i]))
        && st1.webSrcSettings->Js_array2.length == st2.webSrcSettings->Js_array2.length
        && st1.webSrcSettings->Js_array2.everyi((ts1,i) => eqWebSrcSetting(ts1, st2.webSrcSettings[i]))
}

let updateParens = (st,parens) => {
    {
        ...st,
        parens,
        parensErr: None,
    }
}

let setAsrtsToSkip = (st, asrtsToSkip) => {...st, asrtsToSkip}
let setAsrtsToSkipRegex = (st, asrtsToSkipRegex) => {...st, asrtsToSkipRegex}

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
    let newId = st.typeNextId->Belt_Int.toString
    {
        ...st,
        typeNextId: st.typeNextId + 1,
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

let updateWebSrcSetting = (st,id,update:webSrcSettingsState=>webSrcSettingsState) => {
    {
        ...st,
        webSrcSettings: st.webSrcSettings->Js_array2.map(s => if (s.id == id) { update(s) } else { s })
    }
}

let updateAlias = (st,id,alias) => {
    updateWebSrcSetting(st, id, s => {...s, alias, err:None})
}

let updateUrl = (st,id,url) => {
    updateWebSrcSetting(st, id, s => {...s, url, err:None})
}

let updateTrusted = (st,id,trusted) => {
    updateWebSrcSetting(st, id, s => {...s, trusted, err:None})
}

let addWebSrcSetting = st => {
    let newId = st.webSrcNextId->Belt_Int.toString
    {
        ...st,
        webSrcNextId: st.webSrcNextId + 1,
        webSrcSettings: st.webSrcSettings->Js_array2.concat([{
            id: newId,
            alias: "",
            url: "",
            trusted: false,
            err: None,
        }]),
    }
}

let deleteWebSrcSetting = (st, id) => {
    {
        ...st,
        webSrcSettings: st.webSrcSettings->Js_array2.filter(s => s.id != id)
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

    let actAsrtsToSkipChange = (res:MM_cmp_asrts_to_skip.asrtsToSkipResult) => {
        setState(st => {
            let st = st->setAsrtsToSkip(res.asrtsToSkip)
            let st = st->setAsrtsToSkipRegex(res.regex)
            st
        })
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

    let actWebSrcSettingAdd = () => {
        setState(addWebSrcSetting)
    }

    let actWebSrcSettingDelete = id => {
        setState(deleteWebSrcSetting(_, id))
    }

    let actAliasChange = (id,alias) => {
        setState(updateAlias(_, id, alias))
    }

    let actUrlChange = (id,url) => {
        setState(updateUrl(_, id, url))
    }

    let actTrustedChange = (id,trusted) => {
        setState(updateTrusted(_, id, trusted))
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

    let actOpenAsrtsToSkipDialog = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <MM_cmp_asrts_to_skip 
                        initText={state.asrtsToSkip->Js.Array2.joinWith("\n")}
                        initRegex=state.asrtsToSkipRegex
                        onSave={res => {
                            closeModal(modalRef, modalId)
                            actAsrtsToSkipChange(res)
                        }}
                        onCancel={()=> {
                            closeModal(modalRef, modalId)
                        }}
                    />
                </Paper>
            })
        })->ignore
    }

    let rndAsrtsToSkip = () => {
        let asrtsSelected = state.asrtsToSkip->Js_array2.length->Belt.Int.toString
        <Row style=ReactDOM.Style.make(~marginTop="5px", ~marginBottom="5px", ())>
            <span>
                {`Assertions to skip: ${asrtsSelected} assertions selected.`->React.string}
            </span>
            <span
                onClick={_=> { actOpenAsrtsToSkipDialog() }}
                style=ReactDOM.Style.make(~cursor="pointer", ~color="blue", ())
            >
                {React.string("edit")}
            </span>
        </Row>
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
        {rndAsrtsToSkip()}
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
        <MM_cmp_web_src_settings
            webSrcSettings=state.webSrcSettings
            onAdd=actWebSrcSettingAdd
            onAliasChange=actAliasChange
            onUrlChange=actUrlChange
            onTrustedChange=actTrustedChange
            onDelete=actWebSrcSettingDelete
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