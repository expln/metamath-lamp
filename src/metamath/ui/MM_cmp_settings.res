open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_wrk_settings
open MM_wrk_pre_ctx_data
open MM_react_common
open MM_cmp_type_settings
open MM_cmp_web_src_settings
open Common

type settingsState = {
    parens: string,
    parensErr: option<string>,

    asrtsToSkip: array<string>,

    descrRegexToDisc: string,
    descrRegexToDiscErr: option<string>,
    labelRegexToDisc: string,
    labelRegexToDiscErr: option<string>,

    descrRegexToDepr: string,
    descrRegexToDeprErr: option<string>,
    labelRegexToDepr: string,
    labelRegexToDeprErr: option<string>,

    discColor:string,
    deprColor:string,
    tranDeprColor:string,
    allowedFrms:allowedFrms,

    editStmtsByLeftClick:bool,
    initStmtIsGoal:bool,
    defaultStmtLabel:string,
    defaultStmtType:string,
    checkSyntax: bool,
    stickGoalToBottom:bool,
    autoMergeStmts:bool,
    autoUnifyAll:bool,

    typeNextId: int,
    typeSettings: array<typeSettingsState>,
    unifMetavarPrefix:string,
    sortDisjByType:string,

    webSrcNextId: int,
    webSrcSettings: array<webSrcSettingsState>,

    longClickEnabled:bool,
    longClickDelayMsStr:string,

    hideContextSelector:bool,
    showVisByDefault:bool,
    editorHistMaxLengthStr:string,

    useDefaultTransforms:bool,
    useCustomTransforms:bool,
    customTransforms:string,

    combCntMaxStr:string,
}

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

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
let combCntMaxDefault = 10000

let setmm = createDefaultWebSrcSettingState("set.mm:latest","https://us.metamath.org/metamath/set.mm")
let isetmm = createDefaultWebSrcSettingState("iset.mm:latest","https://us.metamath.org/metamath/iset.mm")
let defaultAliases = [setmm.alias, isetmm.alias]

let defaultDiscColor = "#fabed4"
let defaultDeprColor = "#ffe119"
let defaultTranDeprColor = "#fffac8"

let createDefaultSettings = ():settingsState => {
    {
        parens: "( ) [ ] { } [. ]. [_ ]_ <. >. <\" \"> << >> [s ]s (. ). (( )) [b /b",
        parensErr: None,
        asrtsToSkip: [],

        descrRegexToDisc: "\\(New usage is discouraged\\.\\)",
        descrRegexToDiscErr: None,
        labelRegexToDisc: "",
        labelRegexToDiscErr: None,
        descrRegexToDepr: "",
        descrRegexToDeprErr: None,
        labelRegexToDepr: "",
        labelRegexToDeprErr: None,

        discColor:defaultDiscColor,
        deprColor:defaultDeprColor,
        tranDeprColor:defaultTranDeprColor,
        allowedFrms: {
            inSyntax: {
                useDisc:false,
                useDepr:true,
                useTranDepr:true,
            },
            inEssen: {
                useDisc:false,
                useDepr:true,
                useTranDepr:true,
            },
        },

        editStmtsByLeftClick: false,
        initStmtIsGoal:true,
        defaultStmtLabel:"qed",
        defaultStmtType:"|-",
        unifMetavarPrefix:"&",
        sortDisjByType:"class wff",
        checkSyntax: true,
        stickGoalToBottom:true,
        autoMergeStmts:false,
        autoUnifyAll:true,
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
        useDefaultTransforms:true,
        useCustomTransforms:false,
        customTransforms:"",
        combCntMaxStr:combCntMaxDefault->Belt.Int.toString,
    }
}

let addWebSrcSetting = st => {
    let newId = st.webSrcNextId->Belt_Int.toString
    {
        ...st,
        webSrcNextId: st.webSrcNextId + 1,
        webSrcSettings: st.webSrcSettings->Array.concat([{
            id: newId,
            alias: "",
            url: "",
            trusted: false,
            err: None,
        }]),
    }
}

let validateAndCorrectParens = (st:settingsState):settingsState => {
    let newParens = st.parens->String.trim
    let parensErr = if (mod(newParens->getSpaceSeparatedValuesAsArray->Array.length, 2) == 0) {
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

let validateRegex = (regex:string):option<string> => {
    switch strToRegex(regex) {
        | Error(msg) => Some(msg)
        | Ok(_) => None
    }
}

let validateColor = (color:string):string => {
    if (allColors->Array.includes(color)) {
        color
    } else {
        allColors->Array.getUnsafe(0)
    }
}

let validateAndCorrectDiscAndDeprSettings = (st:settingsState):settingsState => {
    let st = {
        ...st,
        descrRegexToDisc: st.descrRegexToDisc->String.trim,
        labelRegexToDisc: st.labelRegexToDisc->String.trim,
        descrRegexToDepr: st.descrRegexToDepr->String.trim,
        labelRegexToDepr: st.labelRegexToDepr->String.trim,
    }
    let st = {
        ...st,
        descrRegexToDiscErr: st.descrRegexToDisc->validateRegex,
        labelRegexToDiscErr: st.labelRegexToDisc->validateRegex,
        descrRegexToDeprErr: st.descrRegexToDepr->validateRegex,
        labelRegexToDeprErr: st.labelRegexToDepr->validateRegex,

        discColor: st.discColor->validateColor,
        deprColor: st.deprColor->validateColor,
        tranDeprColor: st.tranDeprColor->validateColor,
    }
    st
}

let validateDefaultStmtLabel = (label:string):string => {
    label->String.replaceRegExp(%re("/[^A-Za-z0-9._-]/g"), "")
}

let validateAndCorrectDefaultStmtType = (st:settingsState):settingsState => {
    {
        ...st,
        defaultStmtType: st.defaultStmtType->String.trim,
    }
}

let validateAndCorrectUnifMetavarPrefix = (st:settingsState):settingsState => {
    {
        ...st,
        unifMetavarPrefix: st.unifMetavarPrefix->String.trim,
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
        let newTyp = ts.typ->String.trim
        let typHasWhitespace = newTyp->Js_string2.match_(strContainsWhitespaceRegex)->Belt.Option.isSome
        let typIsEmpty = newTyp->String.length == 0
        let newColor = if (!(allColors->Array.includes(ts.color))) {
            allColors->Array.getUnsafe(0)
        } else {
            ts.color
        }
        let newPrefix = ts.prefix->String.trim
        let prefixHasWhitespace = newPrefix->Js_string2.match_(strContainsWhitespaceRegex)->Belt.Option.isSome
        let prefixIsEmpty = newPrefix->String.length == 0
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

    let validatedTypeSettings = st.typeSettings->Array.map(validateAndCorrectTypeSetting)
    let distinctTypeIds = Belt_SetInt.fromArray(
        validatedTypeSettings->Array.map(ts => ts.id->Belt_Int.fromString->Belt.Option.getExn)
    )
    let validatedTypeSettings = if (distinctTypeIds->Belt_SetInt.size == validatedTypeSettings->Array.length) {
        validatedTypeSettings
    } else {
        let maxId = distinctTypeIds->Belt_SetInt.maximum->Belt.Option.getWithDefault(0)
        validatedTypeSettings->Array.mapWithIndex((ts,i) => {...ts, id:(maxId+i+1)->Belt_Int.toString})
    }
    let maxTypSettId = validatedTypeSettings->Array.reduce(
        0,
        (maxId,ts) => {
            let id = ts.id->Belt_Int.fromString->Belt.Option.getExn
            if (id <= maxId) { maxId } else { id }
        }
    )
    let newNextId = if (maxTypSettId < st.typeNextId) {st.typeNextId} else {maxTypSettId + 1}

    {
        ...st,
        typeNextId: newNextId,
        typeSettings: validatedTypeSettings,
    }
}
    
let restoreDefaultsForWebSrc = (state:settingsState, alias: string, url: string):settingsState => {
    let state = if (state.webSrcSettings->Array.find(ws => ws.alias == alias)->Belt.Option.isSome) {
        state
    } else {
        let newId = state.webSrcNextId->Belt_Int.toString
        let state = state->addWebSrcSetting
        {
            ...state,
            webSrcSettings: state.webSrcSettings->Array.map(ws => {
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
        webSrcSettings: state.webSrcSettings->Array.map(ws => {
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
        webSrcSettings: state.webSrcSettings->Expln_utils_common.sortInPlaceWith((s1,s2) => {
            let i1 = if defaultAliases->Array.includes(s1.alias) {0} else {1}
            let i2 = if defaultAliases->Array.includes(s2.alias) {0} else {1}
            Belt_Float.fromInt(i1 - i2)
        })
    }
}

let restoreDefaultWebSrcSettings = (state: settingsState):settingsState => {
    let defaultSettings = createDefaultSettings()
    defaultSettings.webSrcSettings->Array.reduce(
        state,
        (state, default) => restoreDefaultsForWebSrc(state, default.alias, default.url)
    )
}

let validateAndCorrectWebSrcSettings = (st:settingsState):settingsState => {
    let validateAndCorrectWebSrcSetting = (src:webSrcSettingsState):webSrcSettingsState => {
        let newId = src.id->Belt_Int.fromString->Belt.Option.getWithDefault(0)->Belt_Int.toString
        let newAlias = src.alias->String.trim
        let newUrl = src.url->String.trim
        let err = if (newUrl->String.length == 0) {
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

    let validatedWebSrcSettings = st.webSrcSettings->Array.map(validateAndCorrectWebSrcSetting)
    let distinctIds = Belt_SetInt.fromArray(
        validatedWebSrcSettings->Array.map(src => src.id->Belt_Int.fromString->Belt.Option.getExn)
    )
    let validatedWebSrcSettings = if (distinctIds->Belt_SetInt.size == validatedWebSrcSettings->Array.length) {
        validatedWebSrcSettings
    } else {
        let maxId = distinctIds->Belt_SetInt.maximum->Belt.Option.getWithDefault(0)
        validatedWebSrcSettings->Array.mapWithIndex((src,i) => {...src, id:(maxId+i+1)->Belt_Int.toString})
    }
    let maxId = validatedWebSrcSettings->Array.reduce(
        0,
        (maxId,src) => {
            let id = src.id->Belt_Int.fromString->Belt.Option.getExn
            if (id <= maxId) { maxId } else { id }
        }
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

let validateCombCntMax = (combCntMax:int):result<int,string> => {
    if (combCntMax < 1) {
        Ok(1)
    } else {
        Ok(combCntMax)
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

let validateAndCorrectCombCntMax = (st:settingsState):settingsState => {
    {
        ...st,
        combCntMaxStr:
            switch st.combCntMaxStr->Belt_Int.fromString {
                | None => combCntMaxDefault->Belt.Int.toString
                | Some(combCntMax) => {
                    switch validateCombCntMax(combCntMax) {
                        | Ok(combCntMax) => combCntMax->Belt.Int.toString
                        | Error(_) => combCntMaxDefault->Belt.Int.toString
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
    let st = validateAndCorrectDiscAndDeprSettings(st)
    let st = validateAndCorrectDefaultStmtType(st)
    let st = validateAndCorrectTypeSettings(st)
    let st = validateAndCorrectUnifMetavarPrefix(st)
    let st = validateAndCorrectWebSrcSettings(st)
    let st = validateAndCorrectLongClickSettings(st)
    let st = validateAndCorrectDefaultStmtLabel(st)
    let st = validateAndCorrectEditorHistoryMaxLengthSetting(st)
    let st = validateAndCorrectCombCntMax(st)
    st
}

let stateToSettings = (st:settingsState):settings => {
    {
        parens: st.parens,
        asrtsToSkip: st.asrtsToSkip,
        descrRegexToDisc: st.descrRegexToDisc,
        labelRegexToDisc: st.labelRegexToDisc,
        descrRegexToDepr: st.descrRegexToDepr,
        labelRegexToDepr: st.labelRegexToDepr,
        discColor: st.discColor,
        deprColor: st.deprColor,
        allowedFrms: st.allowedFrms,
        tranDeprColor: st.tranDeprColor,
        editStmtsByLeftClick:st.editStmtsByLeftClick,
        initStmtIsGoal:st.initStmtIsGoal,
        defaultStmtLabel:st.defaultStmtLabel,
        defaultStmtType:st.defaultStmtType,
        unifMetavarPrefix:st.unifMetavarPrefix,
        sortDisjByType:st.sortDisjByType,
        checkSyntax:st.checkSyntax,
        stickGoalToBottom:st.stickGoalToBottom,
        autoMergeStmts:st.autoMergeStmts,
        autoUnifyAll:st.autoUnifyAll,
        typeSettings: st.typeSettings->Array.map(typSett => {
            typ: typSett.typ,
            color: typSett.color,
            prefix: typSett.prefix,
        }),
        webSrcSettings: st.webSrcSettings->Array.map(s => {
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
        useDefaultTransforms: st.useDefaultTransforms,
        useCustomTransforms: st.useCustomTransforms,
        customTransforms: st.customTransforms,
        combCntMax: 
            st.combCntMaxStr->Belt_Int.fromString->Belt.Option.getWithDefault(combCntMaxDefault),
    }
}

let settingsToState = (ls:settings):settingsState => {
    let res = {
        parens: ls.parens,
        parensErr: None,
        asrtsToSkip: ls.asrtsToSkip,
        descrRegexToDisc: ls.descrRegexToDisc,
        descrRegexToDiscErr: None,
        labelRegexToDisc: ls.labelRegexToDisc,
        labelRegexToDiscErr: None,
        descrRegexToDepr: ls.descrRegexToDepr,
        descrRegexToDeprErr: None,
        labelRegexToDepr: ls.labelRegexToDepr,
        labelRegexToDeprErr: None,
        discColor: ls.discColor,
        deprColor: ls.deprColor,
        allowedFrms: ls.allowedFrms,
        tranDeprColor: ls.tranDeprColor,
        editStmtsByLeftClick:ls.editStmtsByLeftClick,
        initStmtIsGoal:ls.initStmtIsGoal,
        defaultStmtLabel:ls.defaultStmtLabel,
        defaultStmtType:ls.defaultStmtType,
        unifMetavarPrefix:ls.unifMetavarPrefix,
        sortDisjByType:ls.sortDisjByType,
        checkSyntax:ls.checkSyntax,
        stickGoalToBottom:ls.stickGoalToBottom,
        autoMergeStmts:ls.autoMergeStmts,
        autoUnifyAll:ls.autoUnifyAll,
        typeNextId: 0,
        typeSettings: ls.typeSettings->Array.map(lts => {
            id: "0",
            typ: lts.typ,
            color: lts.color,
            prefix: lts.prefix,
            err: None,
        }),
        webSrcNextId: 0,
        webSrcSettings: ls.webSrcSettings->Array.map(s => {
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
        useDefaultTransforms: ls.useDefaultTransforms,
        useCustomTransforms: ls.useCustomTransforms,
        customTransforms: ls.customTransforms,
        combCntMaxStr: ls.combCntMax->Belt.Int.toString,
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
                    parens: d->str("parens", ~default=()=>defaultSettings.parens),
                    parensErr: None,
                    asrtsToSkip: d->arr("asrtsToSkip", asStr(_), ~default=()=>defaultSettings.asrtsToSkip),
                    descrRegexToDisc: d->str("descrRegexToDisc", ~default=()=>defaultSettings.descrRegexToDisc),
                    descrRegexToDiscErr: None,
                    labelRegexToDisc: d->str("labelRegexToDisc", ~default=()=>defaultSettings.labelRegexToDisc),
                    labelRegexToDiscErr: None,
                    descrRegexToDepr: d->str("descrRegexToDepr", ~default=()=>defaultSettings.descrRegexToDepr),
                    descrRegexToDeprErr: None,
                    labelRegexToDepr: d->str("labelRegexToDepr", ~default=()=>defaultSettings.labelRegexToDepr),
                    labelRegexToDeprErr: None,
                    discColor: d->str("discColor", ~default=()=>defaultDiscColor),
                    deprColor: d->str("deprColor", ~default=()=>defaultDeprColor),
                    tranDeprColor: d->str("tranDeprColor", ~default=()=>defaultTranDeprColor),
                    allowedFrms: d->obj("allowedFrms", d=>{
                        {
                            inSyntax: d->obj("inSyntax", d=>{
                                useDisc: d->bool( "useDisc" ),
                                useDepr: d->bool( "useDepr" ),
                                useTranDepr: d->bool( "useTranDepr" ),
                            }),
                            inEssen: d->obj("inEssen", d=>{
                                useDisc: d->bool( "useDisc" ),
                                useDepr: d->bool( "useDepr" ),
                                useTranDepr: d->bool( "useTranDepr" ),
                            })
                        }
                    }, ~default=()=>defaultSettings.allowedFrms),
                    editStmtsByLeftClick: d->bool(
                        "editStmtsByLeftClick", ~default=()=>defaultSettings.editStmtsByLeftClick
                    ),
                    initStmtIsGoal: d->bool( "initStmtIsGoal", ~default=()=>defaultSettings.initStmtIsGoal ),
                    defaultStmtLabel: d->str("defaultStmtLabel", 
                        ~default=()=>defaultSettings.defaultStmtLabel, 
                        ~validator = str => Ok(validateDefaultStmtLabel(str))
                    ),
                    defaultStmtType: d->str("defaultStmtType", ~default=()=>defaultSettings.defaultStmtType),
                    unifMetavarPrefix: d->str("unifMetavarPrefix", ~default=()=>defaultSettings.unifMetavarPrefix),
                    sortDisjByType: d->str("sortDisjByType", ~default=()=>defaultSettings.sortDisjByType),
                    checkSyntax: d->bool( "checkSyntax", ~default=()=>defaultSettings.checkSyntax ),
                    stickGoalToBottom: d->bool( "stickGoalToBottom", ~default=()=>defaultSettings.stickGoalToBottom),
                    autoMergeStmts: d->bool( "autoMergeStmts", ~default=()=>defaultSettings.autoMergeStmts),
                    autoUnifyAll: d->bool( "autoUnifyAll", ~default=()=>defaultSettings.autoUnifyAll),
                    typeNextId: 0,
                    typeSettings: d->arr("typeSettings", asObj(_, d=>{
                        id: "0",
                        typ: d->str("typ"),
                        color: d->str("color"),
                        prefix: d->str("prefix"),
                        err: None,
                    }), ~default=()=>defaultSettings.typeSettings),
                    webSrcNextId: 0,
                    webSrcSettings: d->arr("webSrcSettings", asObj(_, d=>{
                        id: "0",
                        alias: d->str("alias"),
                        url: d->str("url"),
                        trusted: d->bool("trusted"),
                        err: None,
                    }), ~default=()=>defaultSettings.webSrcSettings),
                    longClickEnabled: d->bool( "longClickEnabled", ~default=()=>defaultSettings.longClickEnabled ),
                    longClickDelayMsStr: d->int( "longClickDelayMs", 
                        ~default = () => longClickDelayMsDefault,
                        ~validator = validateLongClickDelayMs 
                    )->Belt_Int.toString,
                    hideContextSelector: d->bool( "hideContextSelector", 
                        ~default=()=>defaultSettings.hideContextSelector 
                    ),
                    showVisByDefault: d->bool( "showVisByDefault", 
                        ~default=()=>defaultSettings.showVisByDefault 
                    ),
                    editorHistMaxLengthStr: d->int( "editorHistMaxLength", 
                        ~default = () => editorHistMaxLengthDefault,
                        ~validator = validateEditorHistoryMaxLength 
                    )->Belt_Int.toString,
                    useDefaultTransforms: d->bool( "useDefaultTransforms", 
                        ~default=()=>defaultSettings.useDefaultTransforms 
                    ),
                    useCustomTransforms: d->bool( "useCustomTransforms", 
                        ~default=()=>defaultSettings.useCustomTransforms 
                    ),
                    customTransforms: d->str("customTransforms", ~default=()=>defaultSettings.customTransforms),
                    combCntMaxStr: d->int( "combCntMax", 
                        ~default = () => combCntMaxDefault,
                        ~validator = validateCombCntMax 
                    )->Belt_Int.toString,
                }
            }), ~default=()=>defaultSettings)
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
        && st.descrRegexToDiscErr->Belt_Option.isNone
        && st.labelRegexToDiscErr->Belt_Option.isNone
        && st.descrRegexToDeprErr->Belt_Option.isNone
        && st.labelRegexToDeprErr->Belt_Option.isNone
        && st.typeSettings->Array.every(ts => ts.err->Belt_Option.isNone)
        && st.webSrcSettings->Array.every(s => s.err->Belt_Option.isNone)
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
        && st1.descrRegexToDisc == st2.descrRegexToDisc
        && st1.labelRegexToDisc == st2.labelRegexToDisc
        && st1.descrRegexToDepr == st2.descrRegexToDepr
        && st1.labelRegexToDepr == st2.labelRegexToDepr
        && st1.discColor == st2.discColor
        && st1.deprColor == st2.deprColor
        && st1.allowedFrms == st2.allowedFrms
        && st1.tranDeprColor == st2.tranDeprColor
        && st1.editStmtsByLeftClick == st2.editStmtsByLeftClick
        && st1.initStmtIsGoal == st2.initStmtIsGoal
        && st1.defaultStmtLabel == st2.defaultStmtLabel
        && st1.defaultStmtType == st2.defaultStmtType
        && st1.unifMetavarPrefix == st2.unifMetavarPrefix
        && st1.sortDisjByType == st2.sortDisjByType
        && st1.checkSyntax == st2.checkSyntax
        && st1.stickGoalToBottom == st2.stickGoalToBottom
        && st1.autoMergeStmts == st2.autoMergeStmts
        && st1.autoUnifyAll == st2.autoUnifyAll
        && st1.typeSettings->Array.length == st2.typeSettings->Array.length
        && st1.typeSettings->Array.everyWithIndex((ts1,i) => eqTypeSetting(ts1, st2.typeSettings->Array.getUnsafe(i)))
        && st1.webSrcSettings->Array.length == st2.webSrcSettings->Array.length
        && st1.webSrcSettings->Array.everyWithIndex((ts1,i) => eqWebSrcSetting(ts1, st2.webSrcSettings->Array.getUnsafe(i)))
        && st1.longClickEnabled == st2.longClickEnabled
        && st1.longClickDelayMsStr == st2.longClickDelayMsStr
        && st1.hideContextSelector == st2.hideContextSelector
        && st1.showVisByDefault == st2.showVisByDefault
        && st1.editorHistMaxLengthStr == st2.editorHistMaxLengthStr
        && st1.useDefaultTransforms == st2.useDefaultTransforms
        && st1.useCustomTransforms == st2.useCustomTransforms
        && st1.customTransforms == st2.customTransforms
        && st1.combCntMaxStr == st2.combCntMaxStr
}

let updateParens = (st,parens) => {
    {
        ...st,
        parens,
        parensErr: None,
    }
}

let setDescrRegexToDisc = (st, descrRegexToDisc) => {
    {...st, descrRegexToDisc, descrRegexToDiscErr:None }
}
let setLabelRegexToDisc = (st, labelRegexToDisc) => {
    {...st, labelRegexToDisc, labelRegexToDiscErr:None}
}

let setDescrRegexToDepr = (st, descrRegexToDepr) => {
    {...st, descrRegexToDepr, descrRegexToDeprErr:None }
}
let setLabelRegexToDepr = (st, labelRegexToDepr) => {
    {...st, labelRegexToDepr, labelRegexToDeprErr:None}
}

let setDiscColor = (st, color) => {
    {...st, discColor:color}
}
let setDeprColor = (st, color) => {
    {...st, deprColor:color}
}
let setTranDeprColor = (st, color) => {
    {...st, tranDeprColor:color}
}

let updateEditStmtsByLeftClick = (st, editStmtsByLeftClick) => {...st, editStmtsByLeftClick}
let updateInitStmtIsGoal = (st, initStmtIsGoal) => {...st, initStmtIsGoal}
let updateDefaultStmtLabel = (st, defaultStmtLabel) => {...st, defaultStmtLabel}
let updateDefaultStmtType = (st, defaultStmtType) => {...st, defaultStmtType}
let updateUnifMetavarPrefix = (st, unifMetavarPrefix) => {...st, unifMetavarPrefix}
let updateSortDisjByType = (st, sortDisjByType) => {...st, sortDisjByType}
let updateCheckSyntax = (st, checkSyntax) => {...st, checkSyntax}
let updateStickGoalToBottom = (st, stickGoalToBottom) => {...st, stickGoalToBottom}
let updateAutoMergeStmts = (st, autoMergeStmts) => {...st, autoMergeStmts}
let updateAutoUnifyAll = (st, autoUnifyAll) => {...st, autoUnifyAll}
let updateHideContextSelector = (st, hideContextSelector) => {...st, hideContextSelector}
let updateShowVisByDefault = (st, showVisByDefault) => {...st, showVisByDefault}
let updateUseDefaultTransforms = (st, useDefaultTransforms) => {...st, useDefaultTransforms}
let updateUseCustomTransforms = (st, useCustomTransforms) => {...st, useCustomTransforms}
let updateCustomTransforms = (st, customTransforms) => {...st, customTransforms}

let updateUseDiscInSyntax = (st, useDiscInSyntax) => {
    {...st, allowedFrms:{...st.allowedFrms, inSyntax:{...st.allowedFrms.inSyntax, useDisc:useDiscInSyntax}}}
}
let updateUseDiscInEssen = (st, useDiscInEssen) => {
    {...st, allowedFrms:{...st.allowedFrms, inEssen:{...st.allowedFrms.inEssen, useDisc:useDiscInEssen}}}
}
let updateUseDeprInSyntax = (st, useDeprInSyntax) => {
    {...st, allowedFrms:{...st.allowedFrms, inSyntax:{...st.allowedFrms.inSyntax, useDepr:useDeprInSyntax}}}
}
let updateUseDeprInEssen = (st, useDeprInEssen) => {
    {...st, allowedFrms:{...st.allowedFrms, inEssen:{...st.allowedFrms.inEssen, useDepr:useDeprInEssen}}}
}
let updateUseTranDeprInSyntax = (st, useTranDeprInSyntax) => {
    {...st, allowedFrms:{...st.allowedFrms, inSyntax:{...st.allowedFrms.inSyntax, useTranDepr:useTranDeprInSyntax}}}
}
let updateUseTranDeprInEssen = (st, useTranDeprInEssen) => {
    {...st, allowedFrms:{...st.allowedFrms, inEssen:{...st.allowedFrms.inEssen, useTranDepr:useTranDeprInEssen}}}
}

let updateTypeSetting = (st,id,update:typeSettingsState=>typeSettingsState) => {
    {
        ...st,
        typeSettings: st.typeSettings->Array.map(ts => if (ts.id == id) { update(ts) } else { ts })
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
        typeSettings: st.typeSettings->Array.concat([{
            id: newId,
            typ: "",
            color: allColors->Array.getUnsafe(0),
            prefix: "",
            err: None,
        }]),
    }
}

let deleteTypeSetting = (st, id) => {
    {
        ...st,
        typeSettings: st.typeSettings->Array.filter(ts => ts.id != id)
    }
}

let updateWebSrcSetting = (st,id,update:webSrcSettingsState=>webSrcSettingsState) => {
    {
        ...st,
        webSrcSettings: st.webSrcSettings->Array.map(s => if (s.id == id) { update(s) } else { s })
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
        webSrcSettings: st.webSrcSettings->Array.filter(s => s.id != id)
    }
}

let updateLongClickEnabled = (st, longClickEnabled) => {
    { ...st, longClickEnabled: longClickEnabled }
}

let updateLongClickDelayMsStr = (st, longClickDelayMsStr) => {
    { ...st, longClickDelayMsStr: longClickDelayMsStr }
}

let updateCombCntMaxStr = (st, str) => {
    { ...st, combCntMaxStr: str }
}

let updateEditorHistMaxLengthStr = (st, editorHistMaxLengthStr) => {
    { ...st, editorHistMaxLengthStr: editorHistMaxLengthStr }
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~preCtxData:preCtxData,
    ~onChange: settings => unit
) => {
    let (state, setState) = React.useState(_ => preCtxData.settingsV.val->settingsToState)
    let (prevState, setPrevState) = React.useState(_ => state)

    React.useEffect1(() => {
        let newState = preCtxData.settingsV.val->settingsToState
        setState(_ => newState)
        setPrevState(_ => newState)
        None
    }, [preCtxData.settingsV.ver])

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

    let actDescrRegexToDiscUpdated = (descrRegexToDisc:string) => {
        setState(setDescrRegexToDisc(_,descrRegexToDisc))
    }

    let actLabelRegexToDiscUpdated = (labelRegexToDisc:string) => {
        setState(setLabelRegexToDisc(_,labelRegexToDisc))
    }

    let actDescrRegexToDeprUpdated = (descrRegexToDepr:string) => {
        setState(setDescrRegexToDepr(_,descrRegexToDepr))
    }

    let actLabelRegexToDeprUpdated = (labelRegexToDepr:string) => {
        setState(setLabelRegexToDepr(_,labelRegexToDepr))
    }

    let actDiscColorUpdated = (color:string) => {
        setState(setDiscColor(_,color))
    }

    let actDeprColorUpdated = (color:string) => {
        setState(setDeprColor(_,color))
    }

    let actTranDeprColorUpdated = (color:string) => {
        setState(setTranDeprColor(_,color))
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

    let actUnifMetavarPrefixChange = unifMetavarPrefix => {
        setState(updateUnifMetavarPrefix(_, unifMetavarPrefix))
    }

    let actSortDisjByTypeChange = sortDisjByType => {
        setState(updateSortDisjByType(_, sortDisjByType))
    }

    let actLongClickDelayMsStrChange = longClickDelayMsStr => {
        setState(updateLongClickDelayMsStr(_, longClickDelayMsStr))
    }

    let actCombCntMaxStrChange = str => {
        setState(updateCombCntMaxStr(_, str))
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

    let actAutoMergeStmtsChange = autoMergeStmts => {
        setState(updateAutoMergeStmts(_, autoMergeStmts))
    }

    let actAutoUnifyAllChange = autoUnifyAll => {
        setState(updateAutoUnifyAll(_, autoUnifyAll))
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

    let actUseDiscInSyntaxChange = (useDiscInSyntax) => { setState(updateUseDiscInSyntax(_, useDiscInSyntax)) }
    let actUseDiscInEssenChange = (useDiscInEssen) => { setState(updateUseDiscInEssen(_, useDiscInEssen)) }
    let actUseDeprInSyntaxChange = (useDeprInSyntax) => { setState(updateUseDeprInSyntax(_, useDeprInSyntax)) }
    let actUseDeprInEssenChange = (useDeprInEssen) => { setState(updateUseDeprInEssen(_, useDeprInEssen)) }
    let actUseTranDeprInSyntaxChange = (useTranDeprInSyntax) => { setState(updateUseTranDeprInSyntax(_, useTranDeprInSyntax)) }
    let actUseTranDeprInEssenChange = (useTranDeprInEssen) => { setState(updateUseTranDeprInEssen(_, useTranDeprInEssen)) }

    let actUseDefaultTransformsChange = (useDefaultTransforms) => { setState(updateUseDefaultTransforms(_, useDefaultTransforms)) }
    let actUseCustomTransformsChange = (useCustomTransforms) => { setState(updateUseCustomTransforms(_, useCustomTransforms)) }
    let actCustomTransformsChange = (customTransforms) => { setState(updateCustomTransforms(_, customTransforms)) }

    let restoreDefaultsForType = (state:settingsState, typ:string, color:string, prefix:string):settingsState => {
        let state = if (state.typeSettings->Array.find(ts => ts.typ == typ)->Belt.Option.isSome) {
            state
        } else {
            let newId = state.typeNextId->Belt_Int.toString
            let state = state->addTypeSetting
            {
                ...state,
                typeSettings: state.typeSettings->Array.map(ts => {
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
            typeSettings: state.typeSettings->Array.map(ts => {
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
            defaultSettings.typeSettings->Array.reduce(
                state,
                (state, default) => restoreDefaultsForType(state, default.typ, default.color, default.prefix)
            )
        })
    }

    let actRestoreDefaultDescrRegexToDisc = () => {
        actDescrRegexToDiscUpdated(createDefaultSettings().descrRegexToDisc)
    }

    let actRestoreDefaultDiscUsageSettings = () => {
        let defaultSettings = createDefaultSettings()
        setState(st => {
            st
                ->updateUseDiscInSyntax(defaultSettings.allowedFrms.inSyntax.useDisc)
                ->updateUseDeprInSyntax(defaultSettings.allowedFrms.inSyntax.useDepr)
                ->updateUseTranDeprInSyntax(defaultSettings.allowedFrms.inSyntax.useTranDepr)
                ->updateUseDiscInEssen(defaultSettings.allowedFrms.inEssen.useDisc)
                ->updateUseDeprInEssen(defaultSettings.allowedFrms.inEssen.useDepr)
                ->updateUseTranDeprInEssen(defaultSettings.allowedFrms.inEssen.useTranDepr)
                ->setDiscColor(defaultSettings.discColor)
                ->setDeprColor(defaultSettings.deprColor)
                ->setTranDeprColor(defaultSettings.tranDeprColor)
        })
    }

    let makeActTerminate = (modalId:option<modalId>):option<unit=>unit> => {
        modalId->Belt.Option.map(modalId => () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        })
    }

    let rndFindParensProgress = (pct, modalIdOpt) => {
        rndProgress(~text=`Searching parentheses`, ~pct, ~onTerminate=?makeActTerminate(modalIdOpt))
    }

    let syncParens = () => {
        openModal(modalRef, _ => rndFindParensProgress(0., None))->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => rndFindParensProgress(0., Some(modalId)))
            MM_wrk_FindParens.beginFindParens(
                ~ctx=preCtxData.ctxV.val,
                ~onProgress = pct => updateModal(modalRef, modalId, () => rndFindParensProgress(pct, Some(modalId))),
                ~onDone = parens => {
                    actParensChange(parens)
                    closeModal(modalRef, modalId)
                }
            )
        })->ignore
    }

    let actOpenCheckRegexDialog = (~initRegex:string, ~onSave:string=>unit) => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_test_regex 
                    initRegex
                    onSave={regex => {
                        closeModal(modalRef, modalId)
                        onSave(regex)
                    }}
                    onCancel={()=> {
                        closeModal(modalRef, modalId)
                    }}
                />
            })
        })->ignore
    }

    let updatePreCtxDataForFragTransformEditor = (
        ~preCtxData:preCtxData,
        ~useDefaultTransforms:bool,
        ~useCustomTransforms:bool,
    ):preCtxData => {
        {
            ...(preCtxData),
            settingsV: {
                ...(preCtxData.settingsV),
                val: {
                    ...(preCtxData.settingsV.val),
                    useDefaultTransforms,
                    useCustomTransforms,
                }
            }
        }
    }

    let actOpenDefaultTransformsEditor = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_frag_transform_editor
                    modalRef
                    preCtxData=updatePreCtxDataForFragTransformEditor(
                        ~preCtxData:preCtxData,
                        ~useDefaultTransforms=true,
                        ~useCustomTransforms=false,
                    )
                    readOnly=true
                    isCustom=false
                    title="Default transforms script"
                    transformsText=MM_frag_transform_default_script.fragmentTransformsDefaultScript
                    onCancel={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    let actOpenCustomTransformsEditor = () => {
        openModal(modalRef, () => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <MM_cmp_frag_transform_editor
                    modalRef
                    preCtxData=updatePreCtxDataForFragTransformEditor(
                        ~preCtxData:preCtxData,
                        ~useDefaultTransforms=false,
                        ~useCustomTransforms=true,
                    )
                    readOnly=false
                    isCustom=true
                    title="Custom transforms script"
                    transformsText=state.customTransforms
                    onCancel={()=>closeModal(modalRef, modalId)}
                    onSave={newText => {
                        actCustomTransformsChange(newText)
                        closeModal(modalRef, modalId)
                    }}
                />
            })
        })->ignore
    }

    let rndParens = () => {
        let elems = []
        elems->Array.push(
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
        )
        switch state.parensErr {
            | None => ()
            | Some(msg) => {
                elems->Array.push(
                    <pre style=ReactDOM.Style.make(~color="red", ()) key="parens-error">{React.string(msg)}</pre>
                )
            }
        }
        elems->React.array
    }

    let rndError = (err:option<string>) => {
        switch err {
            | None => React.null
            | Some(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ()) >{React.string(msg)}</pre>
            }
        }
    }

    let rndSingleCheckbox = ( ~checked:bool, ~onChange:bool=>unit, ):React.element => {
        <Checkbox checked onChange=evt2bool(onChange) />
    }

    let addAlignAttr = (elem:React.element, alignValue):React.element => {
        elem->React.cloneElement({"align":alignValue})
    }

    let rndDiscAsrtsSettings = () => {
        <Col spacing=1.5>
            <Row alignItems=#center>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="500px", ())
                    label="Regex to determine discouraged assertions by description" 
                    value=state.descrRegexToDisc
                    onChange=evt2str(actDescrRegexToDiscUpdated)
                    title="All assertions with a description matching this regular expression will be marked as discouraged."
                    error={state.descrRegexToDiscErr->Belt_Option.isSome}
                />
                {
                    rndSmallTextBtn(
                        ~text="Check regex",
                        ~onClick=()=>{
                            actOpenCheckRegexDialog(~initRegex=state.descrRegexToDisc, ~onSave=actDescrRegexToDiscUpdated)
                        }
                    )
                }
                {
                    rndSmallTextBtn(
                        ~text="Restore default regex",
                        ~onClick=actRestoreDefaultDescrRegexToDisc
                    )
                }
            </Row>
            {rndError(state.descrRegexToDiscErr)}
            <Row alignItems=#center>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="500px", ())
                    label="Regex to determine discouraged assertions by label" 
                    value=state.labelRegexToDisc
                    onChange=evt2str(actLabelRegexToDiscUpdated)
                    title="All assertions with a label matching this regular expression will be marked as discouraged."
                    error={state.labelRegexToDiscErr->Belt_Option.isSome}
                />
                {
                    rndSmallTextBtn(
                        ~text="Check regex",
                        ~onClick=()=>{
                            actOpenCheckRegexDialog(~initRegex=state.labelRegexToDisc, ~onSave=actLabelRegexToDiscUpdated)
                        }
                    )
                }
            </Row>
            {rndError(state.labelRegexToDiscErr)}
            <Row alignItems=#center>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="500px", ())
                    label="Regex to determine deprecated assertions by description" 
                    value=state.descrRegexToDepr
                    onChange=evt2str(actDescrRegexToDeprUpdated)
                    title="All assertions with a description matching this regular expression will be marked as deprecated."
                    error={state.descrRegexToDeprErr->Belt_Option.isSome}
                />
                {
                    rndSmallTextBtn(
                        ~text="Check regex",
                        ~onClick=()=>{
                            actOpenCheckRegexDialog(~initRegex=state.descrRegexToDepr, ~onSave=actDescrRegexToDeprUpdated)
                        }
                    )
                }
            </Row>
            {rndError(state.descrRegexToDeprErr)}
            <Row alignItems=#center>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="500px", ())
                    label="Regex to determine deprecated assertions by label" 
                    value=state.labelRegexToDepr
                    onChange=evt2str(actLabelRegexToDeprUpdated)
                    title="All assertions with a label matching this regular expression will be marked as deprecated."
                    error={state.labelRegexToDeprErr->Belt_Option.isSome}
                />
                {
                    rndSmallTextBtn(
                        ~text="Check regex",
                        ~onClick=()=>{
                            actOpenCheckRegexDialog(~initRegex=state.labelRegexToDepr, ~onSave=actLabelRegexToDeprUpdated)
                        }
                    )
                }
            </Row>
            {rndError(state.labelRegexToDeprErr)}
            <table style=ReactDOM.Style.make(~borderCollapse="collapse", ~border="none", ())>
                <thead>
                    <tr>
                        {
                            <td className="table-single-border" colSpan=2 style=ReactDOM.Style.make(~border="none", ())>
                                {"Allowed usage"->React.string}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border" colSpan=2>
                                <span>
                                    {"Proofs"->React.string}
                                </span>
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border" rowSpan=2>
                                <span>
                                    {"Color"->React.string}
                                </span>
                            </td>->addAlignAttr("center")
                        }
                    </tr>
                    <tr>
                        {
                            <td className="table-single-border" colSpan=2 style=ReactDOM.Style.make(~border="none", ())>
                                {"of assertions in proofs"->React.string}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border" style=ReactDOM.Style.make(~minWidth="70px", ())>
                                <span>
                                    {"Syntax"->React.string}
                                </span>
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border" style=ReactDOM.Style.make(~minWidth="70px", ())>
                                <span>
                                    {"Essential"->React.string}
                                </span>
                            </td>->addAlignAttr("center")
                        }
                    </tr>
                    <tr>
                        <td className="table-single-border rotateM90" rowSpan=3>{"Assertions"->React.string}</td>
                        {<td className="table-single-border" style=ReactDOM.Style.make(~padding="3px", ()) >{"Discouraged"->React.string}</td>->addAlignAttr("right")}
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inSyntax.useDisc, ~onChange=actUseDiscInSyntaxChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inEssen.useDisc, ~onChange=actUseDiscInEssenChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {
                                    rndColorSelect( 
                                        ~availableColors=allColors, 
                                        ~selectedColor=state.discColor, 
                                        ~onNewColorSelected = actDiscColorUpdated
                                    )
                                }
                            </td>->addAlignAttr("center")
                        }
                    </tr>
                    <tr>
                        {<td className="table-single-border" style=ReactDOM.Style.make(~padding="3px", ())>{"Deprecated"->React.string}</td>->addAlignAttr("right")}
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inSyntax.useDepr, ~onChange=actUseDeprInSyntaxChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inEssen.useDepr, ~onChange=actUseDeprInEssenChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {
                                    rndColorSelect( 
                                        ~availableColors=allColors, 
                                        ~selectedColor=state.deprColor, 
                                        ~onNewColorSelected = actDeprColorUpdated
                                    )
                                }
                            </td>->addAlignAttr("center")
                        }
                    </tr>
                    <tr>
                        {<td className="table-single-border" style=ReactDOM.Style.make(~padding="3px", ())>{"Transitively deprecated"->React.string}</td>->addAlignAttr("right")}
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inSyntax.useTranDepr, ~onChange=actUseTranDeprInSyntaxChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {rndSingleCheckbox( ~checked=state.allowedFrms.inEssen.useTranDepr, ~onChange=actUseTranDeprInEssenChange )}
                            </td>->addAlignAttr("center")
                        }
                        {
                            <td className="table-single-border">
                                {
                                    rndColorSelect( 
                                        ~availableColors=allColors, 
                                        ~selectedColor=state.tranDeprColor, 
                                        ~onNewColorSelected = actTranDeprColorUpdated
                                    )
                                }
                            </td>->addAlignAttr("center")
                        }
                    </tr>
                </thead>
            </table>
            {
                rndSmallTextBtn(
                    ~text="Restore default assertion usage settings",
                    ~onClick=actRestoreDefaultDiscUsageSettings
                )
            }
        </Col>
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
        let disabled = eqState(prevState, state) 
        <Row spacing=3. >
            <Button disabled onClick={_=>actApplyChanges()} variant=#contained 
                color=?{if(!isValid(state)){Some("pastelred")}else{None}}
            >
                {React.string("Apply changes")}
            </Button>
            <Button disabled onClick={_ => discardChanges()}>
                {React.string("Discard changes")}
            </Button>
        </Row>
    }

    let aboutThisAppText = `
        <p>
            Metamath-lamp is a proof assistant for creating formal mathematical proofs in the 
            <a href="https://us.metamath.org">Metamath</a> system. 
        </p>
        <p>
            See the <a href="https://lamp-guide.metamath.org">Metamath-lamp Guide</a> to get started with Metamath-lamp.
        </p>
        <p>
            <a href="https://github.com/expln/metamath-lamp-docs">metamath-lamp-docs</a>
            contains a collection of short documents describing selected features of Metamath-lamp.
        </p>
        <p>
            Check the source code <a href="https://github.com/expln/metamath-lamp">repository</a> to get more technical 
            details or report an issue.
        </p>
    `

    let actOpenAboutThisAppDialog = () => {
        openInfoDialog(
            ~modalRef,
            ~title="About this application",
            ~content=<Static_XML_to_HTML xmlStr=aboutThisAppText/>, 
        )
    }

    let rndAboutThisApp = () => {
        <Row>
            <span style=ReactDOM.Style.make(~cursor="pointer", ()) onClick=clickHnd(~act=actOpenAboutThisAppDialog)>
                <MM_Icons.HelpOutline/>
            </span>
            <span style=ReactDOM.Style.make(~cursor="pointer", ()) onClick=clickHnd(~act=actOpenAboutThisAppDialog)>
                {React.string("About this application")}
            </span>
        </Row>
    }

    <Col spacing=2. style=ReactDOM.Style.make(~margin="30px", ())>
        {rndAboutThisApp()}
        <Divider/>
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
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.autoMergeStmts
                    onChange=evt2bool(actAutoMergeStmtsChange)
                />
            }
            label="Automatically merge similar steps"
        />
        <FormControlLabel
            control={
                <Checkbox
                    checked=state.autoUnifyAll
                    onChange=evt2bool(actAutoUnifyAllChange)
                />
            }
            label="Automatically \"Unify All\""
        />
        <Row alignItems=#center spacing=0. >
            <FormControlLabel
                control={
                    <Checkbox
                        checked=state.useDefaultTransforms
                        onChange=evt2bool(actUseDefaultTransformsChange)
                    />
                }
                label="Use default transforms"
            />
            { rndSmallTextBtn( ~text="View default transforms", ~onClick=actOpenDefaultTransformsEditor ) }
        </Row>
        <Row alignItems=#center spacing=0. >
            <FormControlLabel
                control={
                    <Checkbox
                        checked=state.useCustomTransforms
                        onChange=evt2bool(actUseCustomTransformsChange)
                    />
                }
                label="Use custom transforms"
            />
            { rndSmallTextBtn( ~text="Edit custom transforms", ~onClick=actOpenCustomTransformsEditor ) }
        </Row>
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            label="Max length of editor history" 
            value=state.editorHistMaxLengthStr
            onChange=evt2str(actEditorHistMaxLengthStrChange)
            title="How many previous editor states to store."
        />
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="200px", ())
            label="Max number of combinations" 
            value=state.combCntMaxStr
            onChange=evt2str(actCombCntMaxStrChange)
            title="Max number of distinct combinations of arguments to check per assertion in \"Unify All\" and when proving bottom-up."
        />
        <Divider/>
        {rndDiscAsrtsSettings()}
        <Divider/>
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="310px", ())
            label="Prefix of metavariables in unification" 
            value=state.unifMetavarPrefix
            onChange=evt2str(actUnifMetavarPrefixChange)
            title="All variables with names starting with this prefix will be considered as metavariables during the unification process."
        />
        <TextField 
            size=#small
            style=ReactDOM.Style.make(~width="310px", ())
            label="Sort disjoint variables by type" 
            value=state.sortDisjByType
            onChange=evt2str(actSortDisjByTypeChange)
            title="All variables in disjoint groups will be sorted by type according to this space separated list of types."
        />
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
                    ->Array.map(defaultAlias => 
                        state.webSrcSettings->Array.find(webSrc => webSrc.alias == defaultAlias)
                    )
                    ->Array.filter(Belt_Option.isSome(_))
                    ->Array.map(webSrcOpt => (webSrcOpt->Belt_Option.getExn).id)
            }
        />
        <Divider/>
        {rndApplyChangesBtn()}
    </Col>
}
