open Expln_React_Mui
open Expln_React_common
open Expln_utils_promise
open MM_parser
open MM_react_common
open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_editor
open Local_storage_utils
open MM_wrk_pre_ctx_data

type fileText = 
    | Text(string)
    | UseAst

type mmSingleScope = {
    id:string,
    srcType:mmFileSourceType,
    fileSrc: option<mmFileSource>,
    fileText: option<fileText>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
    label: option<string>,
    resetNestingLevel:bool,
}

type mmScope = {
    nextId: int,
    expanded: bool,
    singleScopes: array<mmSingleScope>,
    loadedContextSummary: string,
}

type reloadCtxFunc = (
    ~srcs:array<mmCtxSrcDto>, 
    ~settings:settings, 
    ~force:bool=?, 
    ~showError:bool=?, 
    ()
) => promise<result<unit,string>>

let createEmptySingleScope = (~id:string, ~srcType:mmFileSourceType) => {
    {
        id,
        srcType,
        fileSrc:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:ReadAll,
        label:None,
        resetNestingLevel:true,
    }
}

let createInitialMmScope = (~defaultSrcType:mmFileSourceType) => {
    {
        nextId: 1,
        singleScopes: [createEmptySingleScope(~id="0", ~srcType=defaultSrcType)],
        expanded: true,
        loadedContextSummary: "",
    }
}

let setSrcType = (ss:mmSingleScope, srcType) => {...ss, srcType}
let setFileSrc = (ss:mmSingleScope, fileSrc:option<mmFileSource>) => {...ss, fileSrc}
let setFileText = (ss:mmSingleScope, fileText) => {...ss, fileText}
let setAst = (ss:mmSingleScope, ast) => {...ss, ast}
let setAllLabels = (ss:mmSingleScope, allLabels) => {...ss, allLabels}
let setReadInstr = (ss:mmSingleScope, readInstr) => {...ss, readInstr}
let setLabel = (ss:mmSingleScope, label) => {...ss, label}
let setResetNestingLevel = (ss:mmSingleScope, resetNestingLevel) => {...ss, resetNestingLevel}

let addSingleScope = (st:mmScope, ~defaultSrcType:mmFileSourceType) => {
    {
        ...st,
        nextId:st.nextId+1,
        singleScopes: st.singleScopes->Belt.Array.concat([
            createEmptySingleScope(~id=st.nextId->Belt_Int.toString, ~srcType=defaultSrcType)
        ])
    }
}
let updateSingleScope = (st,id,update) => {...st, singleScopes:st.singleScopes->Js_array2.map(ss => if ss.id == id {update(ss)} else {ss})}
let deleteSingleScope = (st,id,~defaultSrcType:mmFileSourceType) => {
    let st = {
        ...st, 
        singleScopes:st.singleScopes->Belt.Array.keep(ss => ss.id != id)
    }
    if (st.singleScopes->Js_array2.length == 0) {
        addSingleScope(st, ~defaultSrcType)
    } else {
        st
    }
}
let setExpanded = (st,expanded) => {...st, expanded}
let setLoadedContextSummary = (st,loadedContextSummary) => {...st, loadedContextSummary}

let getNameFromFileSrc = (src:option<mmFileSource>):option<string> => {
    switch src {
        | None => None
        | Some(Local({fileName})) => Some(fileName)
        | Some(Web({alias,url})) => {
            if (alias->Js_string2.trim != "") {
                Some(alias)
            } else {
                Some(url)
            }
        }
    }
}

let getSummary = st => {
    if (st.singleScopes->Js.Array2.length == 1 && (st.singleScopes->Array.getUnsafe(0)).fileSrc->Belt_Option.isNone) {
        "No Metamath database is loaded; please select a database to load."
    } else {
        let filesInfo = st.singleScopes->Js_array2.map(ss => {
            let name = getNameFromFileSrc(ss.fileSrc)->Belt_Option.getWithDefault("")
            let readInstr = switch ss.readInstr {
                | ReadAll => ""
                | StopBefore => `, stopped before ${ss.label->Belt_Option.getWithDefault("")}`
                | StopAfter => `, stopped after ${ss.label->Belt_Option.getWithDefault("")}`
            }
            name ++ readInstr
        })
        "Loaded: " ++ filesInfo->Js_array2.joinWith("; ")
    }
}

let makeActTerminate = (modalRef:modalRef, modalId:modalId):(unit=>unit) => {
    () => {
        MM_wrk_client.terminateWorker()
        closeModal(modalRef, modalId)
    }
}

let webTypStr = Web->mmFileSourceTypeToStr
let localTypStr = Local->mmFileSourceTypeToStr

let isReadInstrSame = (ss:mmSingleScope,srcDto:mmCtxSrcDto):bool => {
    ss.readInstr == srcDto.readInstr->readInstrFromStr
        && (
            ss.readInstr == ReadAll 
                || ss.label->Belt.Option.mapWithDefault(false, ssLabel => ssLabel == srcDto.label) 
        )
        && ss.resetNestingLevel == srcDto.resetNestingLevel
}

let isAstSame = (ss:mmSingleScope,srcDto:mmCtxSrcDto):bool => {
    switch ss.ast {
        | Some(Ok(ssAst)) => {
            switch srcDto.ast {
                | None => false
                | Some(dtoAst) => ssAst === dtoAst
            }
        }
        | _ => false
    }
}

let isSingleScopeSame = (ss:mmSingleScope,srcDto:mmCtxSrcDto):bool => {
    switch ss.fileSrc {
        | None => false
        | Some(ssFileSrc) => {
            switch ssFileSrc {
                | Local({fileName:ssFileName}) => {
                    srcDto.typ == localTypStr
                        && ssFileName == srcDto.fileName 
                        && isReadInstrSame(ss,srcDto)
                        && isAstSame(ss,srcDto)
                }
                | Web({url:ssUrl}) => {
                    srcDto.typ == webTypStr
                        && ssUrl == srcDto.url
                        && isReadInstrSame(ss,srcDto)
                }
            }
        }
    }
}

let isScopeSame = (singleScopes: array<mmSingleScope>, srcs: array<mmCtxSrcDto>):bool => {
    singleScopes->Js.Array2.length == srcs->Js_array2.length
        && singleScopes->Js.Array2.everyi((ss,i) => isSingleScopeSame(ss, srcs->Array.getUnsafe(i)))
}

let canLoadContext = (srcs: array<mmCtxSrcDto>):bool => {
    srcs->Js_array2.length > 0 && srcs->Js_array2.every(src => {
        src.ast->Belt.Option.isSome || src.typ == webTypStr
    })
}

let shouldReloadContext = (singleScopes: array<mmSingleScope>, srcs: array<mmCtxSrcDto>, force:bool):bool => {
    canLoadContext(srcs) && (force || !isScopeSame(singleScopes, srcs))
}

let parseMmFileForSingleScope = (st:mmScope, ~singleScopeId:string, ~modalRef:modalRef):promise<mmScope> => {
    switch st.singleScopes->Js_array2.find(ss => ss.id == singleScopeId) {
        | None => raise(MmException({msg:`Could not find an mmSingleScope with id '${singleScopeId}'`}))
        | Some(ss) => {
            switch ss.fileSrc {
                | None => raise(MmException({
                    msg:`fileSrc is not set for the mmSingleScope with id '${singleScopeId}'`
                }))
                | Some(src) => {
                    switch ss.fileText {
                        | None => raise(MmException({
                            msg:`fileText is not set for the mmSingleScope with id '${singleScopeId}'`
                        }))
                        | Some(Text(text)) => {
                            let name = getNameFromFileSrc(Some(src))->Belt_Option.getExn
                            let progressText = `Parsing ${name}`
                            promise(rsv => {
                                openModal(modalRef, _ => rndProgress(~text=progressText, ~pct=0., ()))->promiseMap(modalId => {
                                    let onTerminate = makeActTerminate(modalRef, modalId)
                                    updateModal( 
                                        modalRef, modalId, () => rndProgress(~text=progressText, ~pct=0., ~onTerminate, ()) 
                                    )
                                    MM_wrk_ParseMmFile.beginParsingMmFile(
                                        ~mmFileText = text,
                                        ~onProgress = pct => updateModal( 
                                            modalRef, modalId, 
                                            () => rndProgress(~text=progressText, ~pct, ~onTerminate, ())
                                        ),
                                        ~onDone = parseResult => {
                                            let st = switch parseResult {
                                                | Error(msg) => {
                                                    let st = st->updateSingleScope(ss.id,setAst(_, Some(Error(msg))))
                                                    let st = st->updateSingleScope(ss.id,setAllLabels(_, []))
                                                    st
                                                }
                                                | Ok((ast,allLabels)) => {
                                                    let st = st->updateSingleScope(ss.id,setAst(_,Some(Ok(ast))))
                                                    let st = st->updateSingleScope(ss.id,setAllLabels(_, allLabels))
                                                    st
                                                }
                                            }
                                            closeModal(modalRef, modalId)
                                            rsv(st)
                                        }
                                    )
                                })->ignore
                            })
                        }
                        | Some(UseAst) => promise(rsv => rsv(st))
                    }
                }
            }
        }
    }
}

let rec parseMmFileForSingleScopeRec = (mmScope:mmScope, ~modalRef:modalRef, ~ssIdx:int):promise<result<mmScope,string>> => {
    if (ssIdx == mmScope.singleScopes->Js.Array2.length) {
        promise(rslv => rslv(Ok(mmScope)))
    } else {
        let ss = mmScope.singleScopes->Array.getUnsafe(ssIdx)
        parseMmFileForSingleScope(mmScope, ~singleScopeId=ss.id, ~modalRef)->promiseFlatMap(mmScope => {
            switch mmScope.singleScopes->Js_array2.find(s => s.id == ss.id) {
                | None => raise(MmException({msg:`None == singleScopes->find(s => s.id == ss.id)`}))
                | Some(ss) => {
                    switch ss.ast {
                        | None => raise(MmException({msg:`Could not parse MM file for ss.id = ${ss.id}`}))
                        | Some(Error(msg)) => promise(rslv => rslv(Error(msg)))
                        | Some(Ok(_)) => parseMmFileForSingleScopeRec(mmScope, ~modalRef, ~ssIdx = ssIdx + 1)
                    }
                }
            }
        })
    }
}

let scopeIsEmpty = (singleScopes: array<mmSingleScope>):bool => 
    singleScopes->Js.Array2.length == 1 && (singleScopes->Array.getUnsafe(0)).fileSrc->Belt_Option.isNone

let loadMmContext = (
    ~singleScopes: array<mmSingleScope>, 
    ~settings:settings,
    ~modalRef:modalRef,
    ~showError:bool,
):promise<result<mmContext,string>> => {
    promise(rsv => {
        if (scopeIsEmpty(singleScopes)) {
            rsv(Ok(createContext(())))
        } else {
            let progressText = `Loading MM context`
            openModal(modalRef, () => rndProgress(~text=progressText, ~pct=0., ()))->promiseMap(modalId => {
                let onTerminate = makeActTerminate(modalRef, modalId)
                updateModal( modalRef, modalId, () => rndProgress(~text=progressText, ~pct=0., ~onTerminate, ()) )
                MM_wrk_LoadCtx.beginLoadingMmContext(
                    ~scopes = singleScopes->Js.Array2.map(ss => {
                        let stopBefore = if (ss.readInstr == StopBefore) {ss.label} else {None}
                        let stopAfter = if (ss.readInstr == StopAfter) {ss.label} else {None}
                        let label = stopBefore->Belt_Option.getWithDefault(
                            stopAfter->Belt_Option.getWithDefault(
                                ss.allLabels->Belt_Array.get(ss.allLabels->Js_array2.length-1)->Belt_Option.getWithDefault("")
                            )
                        )
                        {
                            MM_wrk_LoadCtx.ast: switch ss.ast {
                                | Some(Ok(ast)) => ast
                                | _ => raise(MmException({msg:`Cannot load an MM context from an empty or error ast.`}))
                            },
                            stopBefore,
                            stopAfter,
                            expectedNumOfAssertions: ss.allLabels->Js_array2.indexOf(label) + 1,
                            resetNestingLevel:ss.resetNestingLevel,
                        }
                    }),
                    ~descrRegexToDisc=settings.descrRegexToDisc,
                    ~labelRegexToDisc=settings.labelRegexToDisc,
                    ~descrRegexToDepr=settings.descrRegexToDepr,
                    ~labelRegexToDepr=settings.labelRegexToDepr,
                    ~onProgress = pct => 
                        updateModal( modalRef, modalId, () => rndProgress(~text=progressText, ~pct, ~onTerminate, ())),
                    ~onDone = ctx => {
                        switch ctx {
                            | Error(msg) => {
                                if (showError) {
                                    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                                        updateModal(modalRef, modalId, () => {
                                            <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                                                <Col spacing=1.>
                                                    {React.string("Error loading context:")}
                                                    <pre style=ReactDOM.Style.make(~color="red", ())>
                                                        {React.string(msg)}
                                                    </pre>
                                                    <Button
                                                        onClick={_=>closeModal(modalRef, modalId)}
                                                        variant=#contained
                                                    > 
                                                        {React.string("Ok")}
                                                    </Button>
                                                </Col>
                                            </Paper>
                                        })
                                    })->ignore
                                }
                            }
                            | Ok(_) => ()
                        }
                        rsv(ctx)
                        closeModal(modalRef, modalId)
                    },
                )
            })->ignore
        }
    })
}

let loadMmFileText = (
    ~modalRef:modalRef,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:string=>unit,
    ~alias:string,
    ~url:string,
):promise<result<string,string>> => {
    promise(rslv => {
        FileLoader.loadFileWithProgress(
            ~modalRef,
            ~showWarning=!(trustedUrls->Js_array2.includes(url)),
            ~progressText=`Downloading MM file from "${alias}"`,
            ~url,
            ~onUrlBecomesTrusted,
            ~onReady = text => rslv(Ok(text)),
            ~onError = msg => {
                rslv(Error(
                    `An error occurred while downloading from "${alias}": ${msg->Belt.Option.getWithDefault("")}.`
                ))
            },
            ~onTerminated = () => rslv(Error(`Downloading from "${alias}" was terminated.`)),
            ()
        )
    })
}

let rec loadMmFileTextForSingleScope = (
    ~mmScope:mmScope,
    ~modalRef:modalRef,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:string=>unit,
    ~loadedTexts:Belt_HashMapString.t<string>,
    ~ssIdx:int,
):promise<result<mmScope,string>> => {
    if (ssIdx == mmScope.singleScopes->Js.Array2.length) {
        promise(rslv => rslv(Ok(mmScope)))
    } else {
        let ss = mmScope.singleScopes->Array.getUnsafe(ssIdx)
        let continue = (text:fileText):promise<result<mmScope,string>> => {
            let mmScope = mmScope->updateSingleScope(ss.id, setFileText(_,Some(text)))
            loadMmFileTextForSingleScope(
                ~mmScope,
                ~modalRef,
                ~trustedUrls,
                ~onUrlBecomesTrusted,
                ~loadedTexts,
                ~ssIdx = ssIdx + 1,
            )
        }

        switch ss.fileSrc {
            | None => raise(MmException({msg:`Cannot load MM file text for a None fileSrc.`}))
            | Some(Local(_)) => {
                switch ss.ast {
                    | None => raise(MmException({msg:`Cannot load MM file text for a Local fileSrc.`}))
                    | Some(_) => continue(UseAst)
                }
            }
            | Some(Web({alias,url})) => {
                switch loadedTexts->Belt_HashMapString.get(url) {
                    | Some(text) => continue(Text(text))
                    | None => {
                        loadMmFileText( ~modalRef, ~trustedUrls, ~onUrlBecomesTrusted, ~alias, ~url, )->promiseFlatMap(res => {
                            switch res {
                                | Error(msg) => promise(rslv => rslv(Error(msg)))
                                | Ok(text) => continue(Text(text))
                            }
                        })
                    }
                }
            }
        }
    }
}

let srcDtoToFileSrc = (~src:mmCtxSrcDto, ~webSrcSettings:array<webSrcSettings>):mmFileSource => {
    if (src.typ == localTypStr) {
        Local({ fileName:src.fileName, })
    } else if (src.typ == webTypStr) {
        Web({
            alias: switch webSrcSettings->Js_array2.find(ws => ws.url == src.url) {
                | Some({alias}) => {
                    if (alias->Js_string2.trim != "") {
                        alias
                    } else {
                        src.url
                    }
                }
                | None => src.url
            },
            url: src.url
        })
    } else {
        raise(MmException({msg:`Cannot convert an mmCtxSrcDto to an mmFileSource.`}))
    }
}

let makeMmScopeFromSrcDtos = (
    ~modalRef:modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~srcs: array<mmCtxSrcDto>,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:string=>unit,
    ~loadedTexts:Belt_HashMapString.t<string>,
):promise<result<mmScope,string>> => {
    let mmScope = srcs->Js_array2.reduce(
        (mmScope, src) => {
            let mmScope = mmScope->addSingleScope(~defaultSrcType=src.typ->mmFileSourceTypeFromStr)
            let ssId = (mmScope.singleScopes->Array.getUnsafe(mmScope.singleScopes->Js_array2.length-1)).id
            let mmScope = mmScope->updateSingleScope( ssId,setFileSrc(_,Some(srcDtoToFileSrc(~src, ~webSrcSettings))) )
            let mmScope = mmScope->updateSingleScope( ssId,setAst(_,src.ast->Belt_Option.map(ast => Ok(ast))))
            let mmScope = mmScope->updateSingleScope( ssId,setAllLabels(_,src.allLabels))
            let mmScope = mmScope->updateSingleScope( ssId,setReadInstr(_,src.readInstr->readInstrFromStr))
            let mmScope = mmScope->updateSingleScope( ssId,setLabel(_,Some(src.label)))
            let mmScope = mmScope->updateSingleScope( ssId,setResetNestingLevel(_,src.resetNestingLevel))
            mmScope
        },
        {
            nextId: 0,
            singleScopes: [],
            expanded: false,
            loadedContextSummary: "",
        }
    )
    loadMmFileTextForSingleScope(
        ~mmScope,
        ~modalRef,
        ~trustedUrls,
        ~onUrlBecomesTrusted,
        ~loadedTexts,
        ~ssIdx = 0,
    )->promiseFlatMap(res => {
        switch res {
            | Error(msg) => promise(rslv => rslv(Error(msg)))
            | Ok(mmScope) => parseMmFileForSingleScopeRec(mmScope, ~modalRef, ~ssIdx=0)
        }
    })
}

let defaultValueOfDefaultSrcTypeStr = Web->mmFileSourceTypeToStr

@react.component
let make = (
    ~modalRef:modalRef,
    ~settings:settings,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange:(array<mmCtxSrcDto>, mmContext)=>unit, 
    ~reloadCtx: React.ref<Js.Nullable.t<reloadCtxFunc>>,
    ~style as _ :option<reStyle>=?,
    ~onExpandedChange:bool=>unit,
    ~doToggle: React.ref<Js.Nullable.t<unit=>unit>>,
) => {
    let (defaultSrcTypeStr, setDefaultSrcTypeStr) = useStateFromLocalStorageStr(
        ~key="ctx-selector-default-src-type", ~default=defaultValueOfDefaultSrcTypeStr
    )

    let defaultSrcType = mmFileSourceTypeFromStrOpt(defaultSrcTypeStr)->Belt_Option.getWithDefault(Web)

    let (state, setState) = React.useState(() => createInitialMmScope(~defaultSrcType))
    let (prevState, setPrevState) = React.useState(_ => state)

    React.useEffect0(() => {
        setState(prev => prev->setLoadedContextSummary(getSummary(prev)))
        None
    })

    let actNewCtxIsReady = (srcs:array<mmCtxSrcDto>, ctx:mmContext) => {
        setState(st => {
            let st = st->setLoadedContextSummary(getSummary(st))
            setPrevState(_ => st)
            st
        })
        onChange(srcs,ctx)
    }

    let trustedUrls= settings.webSrcSettings->Js_array2.filter(s => s.trusted)->Js_array2.map(s => s.url)

    let actParseMmFileText = (id:string, src:mmFileSource, text:string):unit => {
        let st = state->updateSingleScope(id,setFileSrc(_,Some(src)))
        let st = st->updateSingleScope(id,setFileText(_,Some(Text(text))))
        st->parseMmFileForSingleScope(~singleScopeId=id, ~modalRef)->promiseMap(st => setState(_ => st))->ignore
    }

    let actToggleAccordion = () => {
        setState(prev => prev->setExpanded(!prev.expanded))
    }
    
    let actCloseAccordion = () => {
        setState(setExpanded(_, false))
    }

    React.useEffect1(() => {
        onExpandedChange(state.expanded)
        None
    }, [state.expanded])

    let rndSingleScopeSelectors = () => {
        let renderDeleteButton = state.singleScopes->Js.Array2.length > 1 || (state.singleScopes->Array.getUnsafe(0)).fileSrc->Belt_Option.isSome
        state.singleScopes->Js_array2.map(singleScope => {
            <MM_cmp_context_selector_single 
                key=singleScope.id
                modalRef
                availableWebSrcs={
                    settings.webSrcSettings
                        ->Js_array2.filter(s => s.alias->Js_string2.trim->Js_string2.length > 0)
                        ->Js_array2.map(s => {
                            {
                                alias:s.alias,
                                url:s.url,
                            }
                        })
                }
                trustedUrls
                onUrlBecomesTrusted
                srcType=singleScope.srcType
                onSrcTypeChange={srcType => {
                    if (state.singleScopes->Js.Array2.length == 1) {
                        setDefaultSrcTypeStr(_ => srcType->mmFileSourceTypeToStr)
                    }
                    setState(updateSingleScope(_,singleScope.id,setSrcType(_,srcType)))
                }}
                fileSrc=singleScope.fileSrc
                onFileChange={(src,text)=>actParseMmFileText(singleScope.id, src, text)}
                parseError={
                    switch singleScope.ast {
                        | Some(Error(msg)) => Some(msg)
                        | _ => None
                    }
                }
                readInstr=singleScope.readInstr
                onReadInstrChange={readInstr => setState(updateSingleScope(_,singleScope.id,setReadInstr(_,readInstr)))}
                label=singleScope.label
                onLabelChange={labelOpt => setState(updateSingleScope(_,singleScope.id,setLabel(_,labelOpt)))}
                allLabels=singleScope.allLabels
                renderDeleteButton
                onDelete={_=>setState(deleteSingleScope(_,singleScope.id,~defaultSrcType))}
            />
        })->React.array
    }

    let rndAddButton = () => {
        let thereIsAtLeastOneValidSingleScope = state.singleScopes->Js_array2.some(singleScope => {
            switch singleScope.ast {
                | Some(Ok(_)) => true
                | _ => false
            }
        })
        if (thereIsAtLeastOneValidSingleScope) {
            <IconButton key="add-button" onClick={_ => setState(addSingleScope(_, ~defaultSrcType))} >
                <MM_Icons.Add/>
            </IconButton>
        } else {
            React.null
        }
    }

    let applyChanges = ( ~state:mmScope, ~showError:bool, ~settings:settings, ):promise<result<unit,string>> => {
        if (scopeIsEmpty(state.singleScopes)) {
            promise(rslv => {
                setState(_ => state)
                actNewCtxIsReady([],createContext(()))
                rslv(Ok(()))
            })
        } else {
            loadMmContext(
                ~singleScopes=state.singleScopes, 
                ~settings,
                ~modalRef, 
                ~showError
            )->promiseMap(res => {
                switch res {
                    | Error(msg) => Error(msg)
                    | Ok(ctx) => {
                        let mmCtxSrcDtos = state.singleScopes->Js.Array2.map(ss => {
                            switch ss.fileSrc {
                                | None => raise(MmException({msg:`ss.fileSrc is None`}))
                                | Some(src) => {
                                    let ast = switch ss.ast {
                                        | Some(Ok(ast)) => Some(ast)
                                        | _ => raise(MmException({msg:`Cannot create mmCtxSrcDto from empty ast.`}))
                                    }
                                    switch src {
                                        | Local({fileName}) => {
                                            {
                                                typ: Local->mmFileSourceTypeToStr,
                                                fileName,
                                                url:"",
                                                readInstr: ss.readInstr->readInstrToStr,
                                                label: ss.label->Belt.Option.getWithDefault(""),
                                                ast,
                                                allLabels: ss.allLabels,
                                                resetNestingLevel: ss.resetNestingLevel,
                                            }
                                        }
                                        | Web({ url, }) => {
                                            {
                                                typ: Web->mmFileSourceTypeToStr,
                                                fileName:"",
                                                url,
                                                readInstr: ss.readInstr->readInstrToStr,
                                                label: ss.label->Belt.Option.getWithDefault(""),
                                                ast,
                                                allLabels: ss.allLabels,
                                                resetNestingLevel: ss.resetNestingLevel,
                                            }
                                        }
                                    }
                                }
                            }
                        })
                        setState(_ => state)
                        actNewCtxIsReady(mmCtxSrcDtos, ctx)
                        Ok(())
                    }
                }
            })
        }
    }

    reloadCtx.current = Js.Nullable.return(
        (~srcs:array<mmCtxSrcDto>, ~settings:settings, ~force:bool=false, ~showError:bool=false, ()):promise<result<unit,string>> => {
            if (!shouldReloadContext(prevState.singleScopes, srcs, force)) {
                promise(rslv => rslv(Ok(())))
            } else {
                let loadedTexts = Belt_HashMapString.fromArray(
                    prevState.singleScopes->Js_array2.map(ss => {
                        switch ss.fileSrc {
                            | None | Some(Local(_)) => None
                            | Some(Web({url})) => {
                                switch ss.fileText {
                                    | None | Some(UseAst) => None
                                    | Some(Text(text)) => Some((url,text))
                                }
                            }
                            
                        }
                    })->Js_array2.filter(Belt_Option.isSome)->Js_array2.map(Belt_Option.getExn)
                )
                makeMmScopeFromSrcDtos(
                    ~modalRef,
                    ~webSrcSettings=settings.webSrcSettings,
                    ~srcs,
                    ~trustedUrls,
                    ~onUrlBecomesTrusted,
                    ~loadedTexts,
                )->promiseFlatMap(res => {
                    switch res {
                        | Error(msg) => promise(rslv => rslv(Error(msg)))
                        | Ok(mmScope) => applyChanges( ~state=mmScope, ~showError, ~settings )
                    }
                })
            }
        }
    )

    doToggle.current = Js.Nullable.return(actToggleAccordion)

    let rndSaveButtons = () => {
        let thereAreNoChanges = (scopeIsEmpty(state.singleScopes) && scopeIsEmpty(prevState.singleScopes)) 
                                    || state.singleScopes == prevState.singleScopes
        if (thereAreNoChanges) {
            React.null
        } else {
            let scopeIsCorrect = state.singleScopes->Js.Array2.every(ss => {
                switch ss.ast {
                    | Some(Ok(_)) => {
                        switch ss.readInstr {
                            | ReadAll => true
                            | StopBefore | StopAfter => {
                                switch ss.label {
                                    | Some(_) => true
                                    | None => false
                                }
                            }
                        }
                    }
                    | _ => false
                }
            })
            let scopeIsEmpty = scopeIsEmpty(state.singleScopes)
            <Row>
                <Button variant=#contained disabled={!scopeIsCorrect && !scopeIsEmpty} 
                    onClick={_=>{
                        applyChanges( ~state, ~showError=true, ~settings )->promiseMap(res => {
                            switch res {
                                | Error(_) => ()
                                | Ok(_) => {
                                    if (!scopeIsEmpty) {
                                        actCloseAccordion()
                                    }
                                }
                            }
                        })->ignore
                    }} 
                >
                    {React.string("Apply changes")}
                </Button>
            </Row>
        }
    }


    <Accordion expanded=state.expanded >
        <AccordionSummaryStyled expandIcon={<MM_Icons.ExpandMore/>} onClick=actToggleAccordion >
            {state.loadedContextSummary->React.string}
        </AccordionSummaryStyled>
        <AccordionDetails>
            <Col spacing=2.>
                {rndSingleScopeSelectors()}
                {rndAddButton()}
                {rndSaveButtons()}
            </Col>
        </AccordionDetails>
    </Accordion>

}
