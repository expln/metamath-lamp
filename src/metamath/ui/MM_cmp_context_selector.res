open Expln_React_Mui
open Expln_utils_promise
open MM_parser
open MM_react_common
open MM_cmp_context_selector_single
open MM_context
open Expln_React_Modal
open MM_wrk_settings

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

type mmCtxSrcDto = {
    fileSrc: mmFileSource,
    readInstr: readInstr,
    label: string,
}

type rec mmScope = {
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
        | Some(Web({alias})) => Some(alias)
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

    let makeActTerminate = (modalId:option<modalId>):option<unit=>unit> => {
        modalId->Belt.Option.map(modalId => () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        })
    }

    let rndParseMmFileProgress = (fileName, pct, modalIdOpt) => {
        rndProgress(~text=`Parsing ${fileName}`, ~pct, ~onTerminate=?makeActTerminate(modalIdOpt), ())
    }

    let rndLoadMmContextProgress = (pct, modalIdOpt) => {
        rndProgress(~text=`Loading MM context`, ~pct, ~onTerminate=?makeActTerminate(modalIdOpt), ())
    }

    let parseMmFileText = (id:string, src:mmFileSource, text:string):unit => {
        let name = getNameFromFileSrc(Some(src))->Belt_Option.getExn
        openModal(modalRef, _ => rndParseMmFileProgress(name, 0., None))->promiseMap(modalId => {
            updateModal(
                modalRef, modalId, () => rndParseMmFileProgress(name, 0., Some(modalId))
            )
            MM_wrk_ParseMmFile.beginParsingMmFile(
                ~mmFileText = text,
                ~onProgress = pct => updateModal(
                    modalRef, modalId, () => rndParseMmFileProgress(name, pct, Some(modalId))
                ),
                ~onDone = parseResult => {
                    setState(updateSingleScope(_,id,setFileSrc(_,Some(src))))
                    setState(updateSingleScope(_,id,setFileText(_,Some(text))))
                    setState(updateSingleScope(_,id,setReadInstr(_,ReadAll)))
                    setState(updateSingleScope(_,id,setLabel(_,None)))
                    switch parseResult {
                        | Error(msg) => {
                            setState(updateSingleScope(_,id,setAst(_, Some(Error(msg)))))
                            setState(updateSingleScope(_,id,setAllLabels(_, [])))
                        }
                        | Ok((ast,allLabels)) => {
                            setState(updateSingleScope(_,id,setAst(_,Some(Ok(ast)))))
                            setState(updateSingleScope(_,id,setAllLabels(_, allLabels)))
                        }
                    }
                    closeModal(modalRef, modalId)
                }
            )
        })->ignore
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
                onFileChange={(src,text)=>parseMmFileText(singleScope.id, src, text)}
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

    let scopeIsEmpty = state.singleScopes->Js.Array2.length == 1 && state.singleScopes[0].fileSrc->Belt_Option.isNone

    let applyChanges = () => {
        if (scopeIsEmpty) {
            actNewCtxIsReady([],createContext(()))
        } else {
            openModal(modalRef, () => rndLoadMmContextProgress(0., None))->promiseMap(modalId => {
                updateModal(
                    modalRef, modalId, () => rndLoadMmContextProgress(0., Some(modalId))
                )
                MM_wrk_LoadCtx.beginLoadingMmContext(
                    ~scopes = state.singleScopes->Js.Array2.map(ss => {
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
                    ~onProgress = pct => updateModal(
                        modalRef, modalId, () => rndLoadMmContextProgress(pct, Some(modalId))
                    ),
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
                            | Ok(ctx) => {
                                let mmCtxSrcDtos = state.singleScopes->Js.Array2.map(ss => {
                                    {
                                        fileSrc: switch ss.fileSrc {
                                            | None => raise(MmException({msg:`ss.fileSrc is None`}))
                                            | Some(src) => src
                                        },
                                        readInstr: ss.readInstr,
                                        label: ss.label->Belt_Option.getWithDefault(""),
                                    }
                                })
                                actNewCtxIsReady(mmCtxSrcDtos, ctx)
                                closeAccordion()
                            }
                        }
                        closeModal(modalRef, modalId)
                    }
                )
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