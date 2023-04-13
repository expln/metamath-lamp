open Expln_React_Mui
open Expln_utils_promise
open MM_parser
open MM_react_common
open MM_cmp_context_selector_single
open MM_context
open Expln_React_Modal

let readInstrFromStr = str => {
    switch str {
        | "all" => #all
        | "stopBefore" => #stopBefore
        | "stopAfter" => #stopAfter
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to a readInstr.`}))
    }
}

type mmSingleScope = {
    id:string,
    fileName: option<string>,
    fileText: option<string>,
    ast: option<result<mmAstNode,string>>,
    allLabels: array<string>,
    readInstr: readInstr,
    label: option<string>,
}

let createEmptySingleScope = id => {
    {
        id,
        fileName:None,
        fileText:None,
        ast:None,
        allLabels:[],
        readInstr:#all,
        label:None
    }
}

type rec mmScope = {
    nextId: int,
    expanded: bool,
    singleScopes: array<mmSingleScope>,
    loadedContextSummary: string,
}

let setFileName = (ss, fileName) => {...ss, fileName}
let setFileText = (ss, fileText) => {...ss, fileText}
let setAst = (ss, ast) => {...ss, ast}
let setAllLabels = (ss, allLabels) => {...ss, allLabels}
let setReadInstr = (ss, readInstr) => {...ss, readInstr}
let setLabel = (ss, label) => {...ss, label}
let reset = ss => createEmptySingleScope(ss.id)

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

let getSummary = st => {
    if (st.singleScopes->Js.Array2.length == 1 && st.singleScopes[0].fileName->Belt_Option.isNone) {
        "Empty MM context is loaded."
    } else {
        let filesInfo = st.singleScopes->Js_array2.map(ss => {
            let fileName = ss.fileName->Belt_Option.getWithDefault("")
            let readInstr = switch ss.readInstr {
                | #all => ""
                | #stopBefore => `, stopped before '${ss.label->Belt_Option.getWithDefault("")}'`
                | #stopAfter => `, stopped after '${ss.label->Belt_Option.getWithDefault("")}'`
            }
            fileName ++ readInstr
        })
        "Loaded: " ++ filesInfo->Js_array2.joinWith("; ")
    }
}

@react.component
let make = (~onChange:mmContext=>unit, ~modalRef:modalRef) => {
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

    let actNewCtxIsReady = ctx => {
        setState(st => {
            let st = st->setLoadedContextSummary(getSummary(st))
            setPrevState(_ => st)
            st
        })
        onChange(ctx)
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

    let parseMmFileText = (id, nameAndTextOpt) => {
        switch nameAndTextOpt {
            | None => setState(updateSingleScope(_,id,reset))
            | Some((name,text)) => {
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
                            setState(updateSingleScope(_,id,setFileName(_,Some(name))))
                            setState(updateSingleScope(_,id,setFileText(_,Some(text))))
                            setState(updateSingleScope(_,id,setReadInstr(_,#all)))
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
        }
    }

    let toggleAccordion = () => {
        setState(prev => prev->setExpanded(!prev.expanded))
    }
    
    let closeAccordion = () => {
        setState(setExpanded(_, false))
    }

    let rndSingleScopeSelectors = () => {
        let renderDeleteButton = state.singleScopes->Js.Array2.length > 1 || state.singleScopes[0].fileName->Belt_Option.isSome
        React.array(
            state.singleScopes->Js_array2.map(singleScope => {
                <MM_cmp_context_selector_single 
                    key=singleScope.id
                    fileName=singleScope.fileName
                    onFileChange=parseMmFileText(singleScope.id, _)
                    parseError={
                        switch singleScope.ast {
                            | Some(Error(msg)) => Some(msg)
                            | _ => None
                        }
                    }
                    readInstr=singleScope.readInstr
                    onReadInstrChange={readInstrStr => setState(updateSingleScope(_,singleScope.id,setReadInstr(_,readInstrFromStr(readInstrStr))))}
                    label=singleScope.label
                    onLabelChange={labelOpt => setState(updateSingleScope(_,singleScope.id,setLabel(_,labelOpt)))}
                    allLabels=singleScope.allLabels
                    renderDeleteButton
                    onDelete={_=>setState(deleteSingleScope(_,singleScope.id))}
                />
            })
        )
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

    let rndWebFileLoader = () => {
        <IconButton key="add-button" onClick={_ => {
            FileLoader.loadFile(
                ~url="https://us.metamath.org/metamath/set.mm",
                ~onProgress = (loaded,total) => {
                    let pct = loaded->Belt_Int.toFloat /. total->Belt_Int.toFloat
                    Js.Console.log2("Loaded: ", (pct *. 100.)->Js_math.round->Belt_Float.toString ++ "%")
                },
                ~onError = () => {
                    Js.Console.log("An error occurred.")
                },
                ~onReady = text => {
                    Js.Console.log2("Done: ", text->Js_string2.substrAtMost(~from=0, ~length=20) ++ "...")
                },
                ()
            )
        }} >
            <MM_Icons.Hub/>
        </IconButton>
    }

    let scopeIsEmpty = state.singleScopes->Js.Array2.length == 1 && state.singleScopes[0].fileName->Belt_Option.isNone

    let applyChanges = () => {
        if (scopeIsEmpty) {
            actNewCtxIsReady(createContext(()))
        } else {
            openModal(modalRef, () => rndLoadMmContextProgress(0., None))->promiseMap(modalId => {
                updateModal(
                    modalRef, modalId, () => rndLoadMmContextProgress(0., Some(modalId))
                )
                MM_wrk_LoadCtx.beginLoadingMmContext(
                    ~scopes = state.singleScopes->Js.Array2.map(ss => {
                        let stopBefore = if (ss.readInstr == #stopBefore) {ss.label} else {None}
                        let stopAfter = if (ss.readInstr == #stopAfter) {ss.label} else {None}
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
                                                <Row>
                                                </Row>
                                            </Col>
                                        </Paper>
                                    })
                                })->ignore
                            }
                            | Ok(ctx) => {
                                actNewCtxIsReady(ctx)
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
                            | #all => true
                            | #stopBefore | #stopAfter => {
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
                {rndWebFileLoader()}
            </Col>
        </AccordionDetails>
    </Accordion>

}