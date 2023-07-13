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

    editStmtsByLeftClick:bool,
    initStmtIsGoal:bool,
    defaultStmtLabel:string,
    defaultStmtType:string,
    checkSyntax: bool,
    stickGoalToBottom:bool,

    typeNextId: int,
    typeSettings: array<typeSettingsState>,

    webSrcNextId: int,
    webSrcSettings: array<webSrcSettingsState>,

    longClickEnabled:bool,
    longClickDelayMsStr:string,

    hideContextSelector:bool,
    showVisByDefault:bool,
    editorHistMaxLengthStr:string,
}

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

let asrtsToSkipRegexDefault = "New usage of \"([^\"]+)\" is discouraged"

let createDefaultWebSrcSettingState = (alias:string,url:string):webSrcSettingsState => {
    {
        id: "0",
        alias,
        url,
        trusted: false,
        err: None
    }
}

let longClickDelayMsDefault = 500
let longClickDelayMsMin = UseLongClick.repeatDelayMs->Belt_Float.toInt + 100
let longClickDelayMsMax = 3000
let editorHistMaxLengthDefault = 20
let editorHistMaxLengthMin = 0
let editorHistMaxLengthMax = 1000

let setmm = createDefaultWebSrcSettingState("set.mm:latest","https://us.metamath.org/metamath/set.mm")
let isetmm = createDefaultWebSrcSettingState("iset.mm:latest","https://us.metamath.org/metamath/iset.mm")
let defaultAliases = [setmm.alias, isetmm.alias]

let createDefaultSettings = () => {
    {
        parens: "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( )) [b /b",
        parensErr: None,
        asrtsToSkip: [],
        asrtsToSkipRegex: asrtsToSkipRegexDefault,
        editStmtsByLeftClick: false,
        initStmtIsGoal:true,
        defaultStmtLabel:"qed",
        defaultStmtType:"|-",
        checkSyntax: true,
        stickGoalToBottom:true,
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
        webSrcSettings: [ setmm, isetmm ],
        longClickEnabled:true,
        longClickDelayMsStr:longClickDelayMsDefault->Belt.Int.toString,
        hideContextSelector:false,
        showVisByDefault:false,
        editorHistMaxLengthStr:editorHistMaxLengthDefault->Belt.Int.toString,
    }
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

let validateDefaultStmtLabel = (label:string):string => {
    label->Js.String2.replaceByRe(%re("/[^A-Za-z0-9._-]/g"), "")
}

let validateAndCorrectDefaultStmtType = (st:settingsState):settingsState => {
    {
        ...st,
        defaultStmtType: st.defaultStmtType->Js_string2.trim,
    }
}

let validateAndCorrectDefaultStmtLabel = (st:settingsState):settingsState => {
    {
        ...st,
        defaultStmtLabel: st.defaultStmtLabel->validateDefaultStmtLabel,
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
    
let restoreDefaultsForWebSrc = (state:settingsState, alias: string, url: string):settingsState => {
    let state = if (state.webSrcSettings->Js.Array2.find(ws => ws.alias == alias)->Belt.Option.isSome) {
        state
    } else {
        let newId = state.webSrcNextId->Belt_Int.toString
        let state = state->addWebSrcSetting
        {
            ...state,
            webSrcSettings: state.webSrcSettings->Js.Array2.map(ws => {
                if (ws.id == newId) {
                    {
                        ...ws,
                        alias,
                    }
                } else {
                    ws
                }
            })
        }
    }
    let state = {
        ...state,
        webSrcSettings: state.webSrcSettings->Js.Array2.map(ws => {
            if (ws.alias == alias) {
                {
                    ...ws,
                    url,
                }
            } else {
                ws
            }
        })
    }
    {
        ...state,
        webSrcSettings: state.webSrcSettings->Js.Array2.sortInPlaceWith((s1,s2) => {
            let i1 = if defaultAliases->Js.Array2.includes(s1.alias) {0} else {1}
            let i2 = if defaultAliases->Js.Array2.includes(s2.alias) {0} else {1}
            i1 - i2
        })
    }
}

let restoreDefaultWebSrcSettings = (state: settingsState):settingsState => {
    let defaultSettings = createDefaultSettings()
    defaultSettings.webSrcSettings->Js.Array2.reduce(
        (state, default) => restoreDefaultsForWebSrc(state, default.alias, default.url),
        state
    )
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

    let st = {
        ...st,
        webSrcNextId: newNextId,
        webSrcSettings: validatedWebSrcSettings,
    }
    st->restoreDefaultWebSrcSettings
}

let validateLongClickDelayMs = (ms:int):result<int,string> => {
    if (ms < longClickDelayMsMin) {
        Ok(longClickDelayMsMin)
    } else if (longClickDelayMsMax < ms) {
        Ok(longClickDelayMsMax)
    } else {
        Ok(ms)
    }
}

let validateAndCorrectLongClickSettings = (st:settingsState):settingsState => {
    {
        ...st,
        longClickDelayMsStr:
            switch st.longClickDelayMsStr->Belt_Int.fromString {
                | None => longClickDelayMsDefault->Belt.Int.toString
                | Some(ms) => {
                    switch validateLongClickDelayMs(ms) {
                        | Ok(ms) => ms->Belt.Int.toString
                        | Error(_) => longClickDelayMsDefault->Belt.Int.toString
                    }
                }
            }
    }
}

let validateEditorHistoryMaxLength = (length:int):result<int,string> => {
    if (length < editorHistMaxLengthMin) {
        Ok(editorHistMaxLengthMin)
    } else if (editorHistMaxLengthMax < length) {
        Ok(editorHistMaxLengthMax)
    } else {
        Ok(length)
    }
}

let validateAndCorrectEditorHistoryMaxLengthSetting = (st:settingsState):settingsState => {
    {
        ...st,
        editorHistMaxLengthStr:
            switch st.editorHistMaxLengthStr->Belt_Int.fromString {
                | None => editorHistMaxLengthDefault->Belt.Int.toString
                | Some(length) => {
                    switch validateEditorHistoryMaxLength(length) {
                        | Ok(length) => length->Belt.Int.toString
                        | Error(_) => editorHistMaxLengthDefault->Belt.Int.toString
                    }
                }
            }
    }
}

let validateAndCorrectState = (st:settingsState):settingsState => {
    let st = validateAndCorrectParens(st)
    let st = validateAndCorrectDefaultStmtType(st)
    let st = validateAndCorrectTypeSettings(st)
    let st = validateAndCorrectWebSrcSettings(st)
    let st = validateAndCorrectLongClickSettings(st)
    let st = validateAndCorrectDefaultStmtLabel(st)
    let st = validateAndCorrectEditorHistoryMaxLengthSetting(st)
    st
}

let stateToSettings = (st:settingsState):settings => {
    {
        parens: st.parens,
        asrtsToSkip: st.asrtsToSkip,
        asrtsToSkipRegex: st.asrtsToSkipRegex,
        editStmtsByLeftClick:st.editStmtsByLeftClick,
        initStmtIsGoal:st.initStmtIsGoal,
        defaultStmtLabel:st.defaultStmtLabel,
        defaultStmtType:st.defaultStmtType,
        checkSyntax:st.checkSyntax,
        stickGoalToBottom:st.stickGoalToBottom,
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
        longClickEnabled: st.longClickEnabled,
        longClickDelayMs: 
            st.longClickDelayMsStr->Belt_Int.fromString->Belt.Option.getWithDefault(longClickDelayMsDefault),
        hideContextSelector: st.hideContextSelector,
        showVisByDefault: st.showVisByDefault,
        editorHistMaxLength: 
            st.editorHistMaxLengthStr->Belt_Int.fromString->Belt.Option.getWithDefault(editorHistMaxLengthDefault),
    }
}

let settingsToState = (ls:settings):settingsState => {
    let res = {
        parens: ls.parens,
        parensErr: None,
        asrtsToSkip: ls.asrtsToSkip,
        asrtsToSkipRegex: ls.asrtsToSkipRegex,
        editStmtsByLeftClick:ls.editStmtsByLeftClick,
        initStmtIsGoal:ls.initStmtIsGoal,
        defaultStmtLabel:ls.defaultStmtLabel,
        defaultStmtType:ls.defaultStmtType,
        checkSyntax:ls.checkSyntax,
        stickGoalToBottom:ls.stickGoalToBottom,
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
        longClickEnabled: ls.longClickEnabled,
        longClickDelayMsStr: ls.longClickDelayMs->Belt.Int.toString,
        hideContextSelector: ls.hideContextSelector,
        showVisByDefault: ls.showVisByDefault,
        editorHistMaxLengthStr: ls.editorHistMaxLength->Belt.Int.toString,
    }
    validateAndCorrectState(res)
}

let settingsLocStorKey = "settings"

let settingsSaveToLocStor = (settings:settings):unit => {
    Dom_storage2.localStorage->Dom_storage2.setItem(
        settingsLocStorKey, 
        Expln_utils_common.stringify(settings)
    )
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
                    asrtsToSkip: d->arr("asrtsToSkip", asStr(_, ()), ~default=()=>defaultSettings.asrtsToSkip, ()),
                    asrtsToSkipRegex: d->str("asrtsToSkipRegex", ~default=()=>defaultSettings.asrtsToSkipRegex, ()),
                    editStmtsByLeftClick: d->bool(
                        "editStmtsByLeftClick", ~default=()=>defaultSettings.editStmtsByLeftClick, ()
                    ),
                    initStmtIsGoal: d->bool( "initStmtIsGoal", ~default=()=>defaultSettings.initStmtIsGoal, () ),
                    defaultStmtLabel: d->str("defaultStmtLabel", 
                        ~default=()=>defaultSettings.defaultStmtLabel, 
                        ~validator = str => Ok(validateDefaultStmtLabel(str)),
                        ()
                    ),
                    defaultStmtType: d->str("defaultStmtType", ~default=()=>defaultSettings.defaultStmtType, ()),
                    checkSyntax: d->bool( "checkSyntax", ~default=()=>defaultSettings.checkSyntax, () ),
                    stickGoalToBottom: d->bool( "stickGoalToBottom", ~default=()=>defaultSettings.stickGoalToBottom,()),
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
                    longClickEnabled: d->bool( "longClickEnabled", ~default=()=>defaultSettings.longClickEnabled, () ),
                    longClickDelayMsStr: d->int( "longClickDelayMs", 
                        ~default = () => longClickDelayMsDefault,
                        ~validator = validateLongClickDelayMs,
                        () 
                    )->Belt_Int.toString,
                    hideContextSelector: d->bool( "hideContextSelector", 
                        ~default=()=>defaultSettings.hideContextSelector, 
                        () 
                    ),
                    showVisByDefault: d->bool( "showVisByDefault", 
                        ~default=()=>defaultSettings.showVisByDefault, 
                        () 
                    ),
                    editorHistMaxLengthStr: d->int( "editorHistMaxLength", 
                        ~default = () => editorHistMaxLengthDefault,
                        ~validator = validateEditorHistoryMaxLength,
                        () 
                    )->Belt_Int.toString,
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
        && st1.editStmtsByLeftClick == st2.editStmtsByLeftClick
        && st1.initStmtIsGoal == st2.initStmtIsGoal
        && st1.defaultStmtLabel == st2.defaultStmtLabel
        && st1.defaultStmtType == st2.defaultStmtType
        && st1.checkSyntax == st2.checkSyntax
        && st1.stickGoalToBottom == st2.stickGoalToBottom
        && st1.typeSettings->Js_array2.length == st2.typeSettings->Js_array2.length
        && st1.typeSettings->Js_array2.everyi((ts1,i) => eqTypeSetting(ts1, st2.typeSettings[i]))
        && st1.webSrcSettings->Js_array2.length == st2.webSrcSettings->Js_array2.length
        && st1.webSrcSettings->Js_array2.everyi((ts1,i) => eqWebSrcSetting(ts1, st2.webSrcSettings[i]))
        && st1.longClickEnabled == st2.longClickEnabled
        && st1.longClickDelayMsStr == st2.longClickDelayMsStr
        && st1.hideContextSelector == st2.hideContextSelector
        && st1.showVisByDefault == st2.showVisByDefault
        && st1.editorHistMaxLengthStr== st2.editorHistMaxLengthStr
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

let updateEditStmtsByLeftClick = (st, editStmtsByLeftClick) => {...st, editStmtsByLeftClick}
let updateInitStmtIsGoal = (st, initStmtIsGoal) => {...st, initStmtIsGoal}
let updateDefaultStmtLabel = (st, defaultStmtLabel) => {...st, defaultStmtLabel}
let updateDefaultStmtType = (st, defaultStmtType) => {...st, defaultStmtType}
let updateCheckSyntax = (st, checkSyntax) => {...st, checkSyntax}
let updateStickGoalToBottom = (st, stickGoalToBottom) => {...st, stickGoalToBottom}
let updateHideContextSelector = (st, hideContextSelector) => {...st, hideContextSelector}
let updateShowVisByDefault = (st, showVisByDefault) => {...st, showVisByDefault}

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

let deleteWebSrcSetting = (st, id) => {
    {
        ...st,
        webSrcSettings: st.webSrcSettings->Js_array2.filter(s => s.id != id)
    }
}

let updateLongClickEnabled = (st, longClickEnabled) => {
    { ...st, longClickEnabled: longClickEnabled }
}

let updateLongClickDelayMsStr = (st, longClickDelayMsStr) => {
    { ...st, longClickDelayMsStr: longClickDelayMsStr }
}

let updateEditorHistMaxLengthStr = (st, editorHistMaxLengthStr) => {
    { ...st, editorHistMaxLengthStr: editorHistMaxLengthStr }
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~ctx:mmContext, 
    ~settingsVer:int, 
    ~settings:settings, 
    ~onChange: settings => unit
) => {
    let (state, setState) = React.useState(_ => settings->settingsToState)
    let (prevState, setPrevState) = React.useState(_ => state)

    React.useEffect1(() => {
        let newState = settings->settingsToState
        setState(_ => newState)
        setPrevState(_ => newState)
        None
    }, [settingsVer])

    let actApplyChanges = () => {
        let st = validateAndCorrectState(state)
        setState(_ => st)
        if (st->isValid) {
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

    let actEditStmtsByLeftClickChange = editStmtsByLeftClick => {
        setState(updateEditStmtsByLeftClick(_, editStmtsByLeftClick))
    }

    let actInitStmtIsGoalChange = initStmtIsGoal => {
        setState(updateInitStmtIsGoal(_, initStmtIsGoal))
    }

    let actDefaultStmtLabelChange = defaultStmtLabel => {
        setState(updateDefaultStmtLabel(_, defaultStmtLabel))
    }

    let actDefaultStmtTypeChange = defaultStmtType => {
        setState(updateDefaultStmtType(_, defaultStmtType))
    }

    let actLongClickDelayMsStrChange = longClickDelayMsStr => {
        setState(updateLongClickDelayMsStr(_, longClickDelayMsStr))
    }

    let actEditorHistMaxLengthStrChange = editorHistMaxLengthStr => {
        setState(updateEditorHistMaxLengthStr(_, editorHistMaxLengthStr))
    }

    let actCheckSyntaxChange = checkSyntax => {
        setState(updateCheckSyntax(_, checkSyntax))
    }

    let actHideContextSelectorChange = hideContextSelector => {
        setState(updateHideContextSelector(_, hideContextSelector))
    }

    let actShowVisByDefaultChange = showVisByDefault => {
        setState(updateShowVisByDefault(_, showVisByDefault))
    }

    let actStickGoalToBottomChange = stickGoalToBottom => {
        setState(updateStickGoalToBottom(_, stickGoalToBottom))
    }

    let actLongClickEnabledChange = (longClickEnabled) => {
        setState(updateLongClickEnabled(_, longClickEnabled))
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

    let restoreDefaultsForType = (state:settingsState, typ:string, color:string, prefix:string):settingsState => {
        let state = if (state.typeSettings->Js.Array2.find(ts => ts.typ == typ)->Belt.Option.isSome) {
            state
        } else {
            let newId = state.typeNextId->Belt_Int.toString
            let state = state->addTypeSetting
            {
                ...state,
                typeSettings: state.typeSettings->Js.Array2.map(ts => {
                    if (ts.id == newId) {
                        {
                            ...ts,
                            typ,
                        }
                    } else {
                        ts
                    }
                })
            }
        }
        {
            ...state,
            typeSettings: state.typeSettings->Js.Array2.map(ts => {
                if (ts.typ == typ) {
                    {
                        ...ts,
                        color,
                        prefix
                    }
                } else {
                    ts
                }
            })
        }
    }

    let actRestoreDefaultTypeSettings = () => {
        setState(state => {
            let defaultSettings = createDefaultSettings()
            defaultSettings.typeSettings->Js.Array2.reduce(
                (state, default) => restoreDefaultsForType(state, default.typ, default.color, default.prefix),
                state
            )
        })
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

    let rndParens = () => {
        let elems = []
        elems->Js_array2.push(
            <Row alignItems=#center key="parens-text-field">
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
        )->ignore
        switch state.parensErr {
            | None => ()
            | Some(msg) => {
                elems->Js_array2.push(
                    <pre style=ReactDOM.Style.make(~color="red", ()) key="parens-error">{React.string(msg)}</pre>
                )->ignore
            }
        }
        elems->React.array
    }

    let rndAsrtsToSkip = () => {
        let asrtsSelected = state.asrtsToSkip->Js_array2.length->Belt.Int.toString
        <Row>
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

    let rndLongClickSettings = () => {
        <Row>
            <FormControlLabel
                control={
                    <Checkbox
                        checked=state.longClickEnabled
                        onChange=evt2bool(actLongClickEnabledChange)
                    />
                }
                label="Enable long-click"
            />
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="150px", ())
                label="Long click delay, ms" 
                value={state.longClickDelayMsStr} 
                onChange=evt2str(actLongClickDelayMsStrChange)
                title="How many milliseconds to wait for a click to become \"long\""
                disabled={!state.longClickEnabled}
            />
        </Row>
    }

    let rndApplyChangesBtn = () => {
        let disabled = !isValid(state) || eqState(prevState, state) 
        <Row spacing=3. >
            <Button disabled onClick={_=>actApplyChanges()} variant=#contained>
                {React.string("Apply changes")}
            </Button>
            <Button disabled onClick={_ => discardChanges()}>
                {React.string("Discard changes")}
            </Button>
        </Row>
    }

    <Col spacing=2. style=ReactDOM.Style.make(~margin="30px", ())>
        {rndApplyChangesBtn()}
        <Divider/>
        {rndParens()}
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.checkSyntax
                    onChange=evt2bool(actCheckSyntaxChange)
                />
            }
            label="Check syntax"
        />
        {rndAsrtsToSkip()}
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.initStmtIsGoal
                    onChange=evt2bool(actInitStmtIsGoalChange)
                />
            }
            label="Mark the first provable step as a goal"
        />
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.stickGoalToBottom
                    onChange=evt2bool(actStickGoalToBottomChange)
                />
            }
            label="Stick the goal step to the bottom"
        />
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            label="Initial step label" 
            value=state.defaultStmtLabel
            onChange=evt2str(actDefaultStmtLabelChange)
            title="This text is used as a label for the first provable step. If empty - a label will be generated automatically."
        />
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            label="Default statement type" 
            value=state.defaultStmtType 
            onChange=evt2str(actDefaultStmtTypeChange)
            title="This text is used as the initial statement type for new statements"
        />
        <MM_cmp_edit_stmts_setting
            editStmtsByLeftClick=state.editStmtsByLeftClick
            onChange=actEditStmtsByLeftClickChange
            longClickEnabled=state.longClickEnabled
        />
        {rndLongClickSettings()}
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.hideContextSelector
                    onChange=evt2bool(actHideContextSelectorChange)
                />
            }
            label="Hide context header"
        />
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.showVisByDefault
                    onChange=evt2bool(actShowVisByDefaultChange)
                />
            }
            label="Show visualizations by default"
        />
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            label="Max length of editor history" 
            value=state.editorHistMaxLengthStr
            onChange=evt2str(actEditorHistMaxLengthStrChange)
            title="How many previous editor states to store."
        />
        <Divider/>
        <MM_cmp_type_settings
            typeSettings=state.typeSettings
            availableColors=allColors
            onAdd=actTypeSettingAdd
            onTypeChange=actTypeChange
            onColorChange=actColorChange
            onPrefixChange=actPrefixChange
            onDelete=actTypeSettingDelete
            onRestoreDefaults=actRestoreDefaultTypeSettings
        />
        <Divider/>
        <MM_cmp_web_src_settings
            webSrcSettings=state.webSrcSettings
            onAdd=actWebSrcSettingAdd
            onAliasChange=actAliasChange
            onUrlChange=actUrlChange
            onTrustedChange=actTrustedChange
            onDelete=actWebSrcSettingDelete
            defaultIds={
                defaultAliases
                    ->Js.Array2.map(defaultAlias => 
                        state.webSrcSettings->Js.Array2.find(webSrc => webSrc.alias == defaultAlias)
                    )
                    ->Js_array2.filter(Belt_Option.isSome)
                    ->Js.Array2.map(webSrcOpt => (webSrcOpt->Belt_Option.getExn).id)
            }
        />
        <Divider/>
        {rndApplyChangesBtn()}
    </Col>
}
