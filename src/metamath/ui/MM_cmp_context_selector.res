open Expln_React_Mui
open Expln_utils_promise
open MM_parser
open MM_react_common
open MM_context
open Expln_React_Modal
open MM_wrk_settings
open MM_wrk_editor

type mmSingleScope = {
    id:string,
    srcType:mmFileSourceType,
    fileSrc: option<mmFileSource>,
    fileText: option<string>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
    label: option<string>,
}

type mmScope = {
    nextId: int,
    expanded: bool,
    singleScopes: array<mmSingleScope>,
    loadedContextSummary: string,
}

let createEmptySingleScope = id => {
    {
        id,
        srcType:Web,
        fileSrc:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:ReadAll,
        label:None
    }
}

let setSrcType = (ss:mmSingleScope, srcType) => {...ss, srcType}
let setFileSrc = (ss:mmSingleScope, fileSrc:option<mmFileSource>) => {...ss, fileSrc}
let setFileText = (ss:mmSingleScope, fileText) => {...ss, fileText}
let setAst = (ss:mmSingleScope, ast) => {...ss, ast}
let setAllLabels = (ss:mmSingleScope, allLabels) => {...ss, allLabels}
let setReadInstr = (ss:mmSingleScope, readInstr) => {...ss, readInstr}
let setLabel = (ss:mmSingleScope, label) => {...ss, label}
let reset = (ss:mmSingleScope) => createEmptySingleScope(ss.id)

let addSingleScope = st => {
    {
        ...st,
        nextId:st.nextId+1,
        singleScopes: st.singleScopes->Belt.Array.concat([createEmptySingleScope(st.nextId->Belt_Int.toString)])
    }
}
let updateSingleScope = (st,id,update) => {...st, singleScopes:st.singleScopes->Js_array2.map(ss => if ss.id == id {update(ss)} else {ss})}
let deleteSingleScope = (st,id) => {
    let st = {
        ...st, 
        singleScopes:st.singleScopes->Belt.Array.keep(ss => ss.id != id)
    }
    if (st.singleScopes->Js_array2.length == 0) {
        addSingleScope(st)
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
    if (st.singleScopes->Js.Array2.length == 1 && st.singleScopes[0].fileSrc->Belt_Option.isNone) {
        "Empty MM context is loaded."
    } else {
        let filesInfo = st.singleScopes->Js_array2.map(ss => {
            let name = getNameFromFileSrc(ss.fileSrc)->Belt_Option.getWithDefault("")
            let readInstr = switch ss.readInstr {
                | ReadAll => ""
                | StopBefore => `, stopped before '${ss.label->Belt_Option.getWithDefault("")}'`
                | StopAfter => `, stopped after '${ss.label->Belt_Option.getWithDefault("")}'`
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
                        | Some(text) => {
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
                                            let st = st->updateSingleScope(ss.id,setReadInstr(_,ReadAll))
                                            let st = st->updateSingleScope(ss.id,setLabel(_,None))
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
                    }
                }
            }
        }
    }
}

let scopeIsEmpty = (st:mmScope):bool => 
    st.singleScopes->Js.Array2.length == 1 && st.singleScopes[0].fileSrc->Belt_Option.isNone

let loadMmContext = (st:mmScope, ~modalRef:modalRef):promise<result<mmContext,string>> => {
    promise(rsv => {
        if (scopeIsEmpty(st)) {
            rsv(Ok(createContext(())))
        } else {
            let progressText = `Loading MM context`
            openModal(modalRef, () => rndProgress(~text=progressText, ~pct=0., ()))->promiseMap(modalId => {
                let onTerminate = makeActTerminate(modalRef, modalId)
                updateModal( modalRef, modalId, () => rndProgress(~text=progressText, ~pct=0., ~onTerminate, ()) )
                MM_wrk_LoadCtx.beginLoadingMmContext(
                    ~scopes = st.singleScopes->Js.Array2.map(ss => {
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
                            expectedNumOfAssertions: ss.allLabels->Js_array2.indexOf(label) + 1
                        }
                    }),
                    ~onProgress = pct => 
                        updateModal( modalRef, modalId, () => rndProgress(~text=progressText, ~pct, ~onTerminate, ())),
                    ~onDone = ctx => {
                        switch ctx {
                            | Error(msg) => {
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
                            | Ok(_) => ()
                        }
                        rsv(ctx)
                        closeModal(modalRef, modalId)
                    }
                )
            })->ignore
        }
    })
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~webSrcSettings:array<webSrcSettings>,
    ~onUrlBecomesTrusted:string=>unit,
    ~onChange:(array<mmCtxSrcDto>, mmContext)=>unit, 
) => {
    let (state, setState) = React.useState(_ => {
        {
            nextId: 1, 
            singleScopes: [createEmptySingleScope("0")], 
            expanded: true, 
            loadedContextSummary: "",
        }
    })
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

    let actParseMmFileText = (id:string, src:mmFileSource, text:string):unit => {
        let st = state->updateSingleScope(id,setFileSrc(_,Some(src)))
        let st = st->updateSingleScope(id,setFileText(_,Some(text)))
        st->parseMmFileForSingleScope(~singleScopeId=id, ~modalRef)->promiseMap(st => setState(_ => st))->ignore
    }

    let toggleAccordion = () => {
        setState(prev => prev->setExpanded(!prev.expanded))
    }
    
    let closeAccordion = () => {
        setState(setExpanded(_, false))
    }

    let rndSingleScopeSelectors = () => {
        let renderDeleteButton = state.singleScopes->Js.Array2.length > 1 || state.singleScopes[0].fileSrc->Belt_Option.isSome
        state.singleScopes->Js_array2.map(singleScope => {
            <MM_cmp_context_selector_single 
                key=singleScope.id
                modalRef
                availableWebSrcs={
                    webSrcSettings
                        ->Js_array2.filter(s => s.alias->Js_string2.trim->Js_string2.length > 0)
                        ->Js_array2.map(s => {
                            {
                                alias:s.alias,
                                url:s.url,
                            }
                        })
                }
                trustedUrls={
                    webSrcSettings
                        ->Js_array2.filter(s => s.trusted)
                        ->Js_array2.map(s => s.url)
                }
                onUrlBecomesTrusted
                srcType=singleScope.srcType
                onSrcTypeChange={srcType => setState(updateSingleScope(_,singleScope.id,setSrcType(_,srcType)))}
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
                onDelete={_=>setState(deleteSingleScope(_,singleScope.id))}
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
            <IconButton key="add-button" onClick={_ => setState(addSingleScope)} >
                <MM_Icons.Add/>
            </IconButton>
        } else {
            React.null
        }
    }

    let scopeIsEmpty = scopeIsEmpty(state)

    let applyChanges = () => {
        if (scopeIsEmpty) {
            actNewCtxIsReady([],createContext(()))
        } else {
            loadMmContext(state, ~modalRef)->promiseMap(ctx => {
                switch ctx {
                    | Error(_) => ()
                    | Ok(ctx) => {
                        let mmCtxSrcDtos = state.singleScopes->Js.Array2.map(ss => {
                            switch ss.fileSrc {
                                | None => raise(MmException({msg:`ss.fileSrc is None`}))
                                | Some(src) => {
                                    switch src {
                                        | Local({fileName}) => {
                                            {
                                                typ: Local->mmFileSourceTypeToStr,
                                                fileName,
                                                url:"",
                                                readInstr: ss.readInstr->readInstrToStr,
                                                label: ss.label->Belt.Option.getWithDefault(""),
                                            }
                                        }
                                        | Web({ url, }) => {
                                            {
                                                typ: Web->mmFileSourceTypeToStr,
                                                fileName:"",
                                                url,
                                                readInstr: ss.readInstr->readInstrToStr,
                                                label: ss.label->Belt.Option.getWithDefault(""),
                                            }
                                        }
                                    }
                                }
                            }
                        })
                        actNewCtxIsReady(mmCtxSrcDtos, ctx)
                        closeAccordion()
                    }
                }
            })->ignore
        }
    }

    let rndSaveButtons = () => {
        let thereAreNoChanges = state.singleScopes == prevState.singleScopes
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
            <Row>
                <Button variant=#contained disabled={!scopeIsCorrect && !scopeIsEmpty} onClick={_=>applyChanges()} >
                    {React.string("Apply changes")}
                </Button>
            </Row>
        }
    }


    <Accordion expanded=state.expanded >
        <AccordionSummaryStyled expandIcon={<MM_Icons.ExpandMore/>} onClick=toggleAccordion >
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