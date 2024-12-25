open Expln_React_common
open Expln_React_Mui
open MM_react_common
open MM_cmp_single_frag_transf
open MM_wrk_frag_transform
open MM_wrk_editor
open Raw_js_utils

let transformsTextCache:ref<array<string>> = ref([])
let allTransformsCache: ref<array<fragmentTransform>> = ref([])

type state = {
    step:JSON.t,
    transformsParseErr:option<string>,
    availableTransforms:option<reElem>,
    selectedTransform: option<fragmentTransform>,
}

let createInitialState = (
    ~step:userStmt,
    ~ctxConstIntToSymExn:int=>string,
):state => {
    {
        step: MM_api.stmtToJson(step, Some(ctxConstIntToSymExn)),
        transformsParseErr: None,
        availableTransforms: None,
        selectedTransform: None,
    }
}

let setTransformsParseErr = (st:state,str:string) => {
    {...st, transformsParseErr:Some(str)}
}

let setAvailableTransforms = (st:state,elem:reElem) => {
    {...st, availableTransforms:Some(elem)}
}

let setSelectedTransform = (st:state,tr:option<fragmentTransform>) => {
    {...st, selectedTransform:tr}
}

@react.component
let make = (
    ~step:userStmt,
    ~ctxConstIntToSymExn:int=>string,
    ~transformsText:array<string>,
    ~onInsertAbove:(bool,string)=>unit,
    ~onInsertBelow:(bool,string)=>unit,
    ~onUpdateCurrent:(bool,string)=>unit,
    ~onCancel:unit=>unit,
) => {
    let (state, setState) = React.useState(() => createInitialState(~step, ~ctxConstIntToSymExn))

    let rndAvailableTransforms = (availableTransforms:array<fragmentTransform>):result<reElem,string> => {
        let param = {"step":state.step}
        let listItems = invokeExnFunc( "Listing available transforms", () => {
            availableTransforms->Array.mapWithIndex((availableTransform,i) => {
                <ListItem key={i->Belt_Int.toString} disablePadding=true >
                    <ListItemButton onClick={_=>{setState(setSelectedTransform(_,Some(availableTransform)))}}>
                        <ListItemText>
                            {React.string(availableTransform.displayName(param))}
                        </ListItemText>
                    </ListItemButton>
                </ListItem>
            })->React.array
        })
        switch listItems {
            | Error(msg) => Error(msg)
            | Ok(listItems) => {
                Ok(
                    <ListCmp disablePadding=true>
                        listItems
                    </ListCmp>
                )
            }
        }
    }

    React.useEffect0(() => {
        let param = {"step":state.step}
        if (transformsText != transformsTextCache.contents) {
            let allTransformsRef = ref([])
            let availableTransformsElem = arrStrToFragTransforms(transformsText)
                ->Belt.Result.flatMap(allTransforms => {
                    allTransformsRef := allTransforms
                    invokeExnFunc(
                        "Getting available transforms",
                        () => allTransforms->Array.filter(tr => tr.canApply(param))
                    )
                })
                ->Belt.Result.flatMap(rndAvailableTransforms)
            switch availableTransformsElem {
                | Error(msg) => setState(setTransformsParseErr(_, msg))
                | Ok(availableTransformsElem) => {
                    transformsTextCache := transformsText
                    allTransformsCache := allTransformsRef.contents
                    setState(setAvailableTransforms(_, availableTransformsElem))
                }
            }
        } else {
            let availableTransformsElem = invokeExnFunc(
                    "Getting available transforms from cache",
                    () => allTransformsCache.contents->Array.filter(tr => tr.canApply(param))
                )
                ->Belt.Result.flatMap(rndAvailableTransforms)
            switch availableTransformsElem {
                | Error(msg) => setState(setTransformsParseErr(_, msg))
                | Ok(availableTransformsElem) => setState(setAvailableTransforms(_, availableTransformsElem))
            }
        }
        None
    })

    let actUnselectTransform = () => {
        setState(setSelectedTransform(_,None))
    }

    let rndButtons = () => {
        <Row>
            <Button onClick={_=>onCancel()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
            {
                rndHiddenTextField(
                    ~onKeyDown=kbrdHnds([
                        kbrdClbkMake(~key=keyEsc, ~act=onCancel),
                    ])
                )
            }
        </Row>
    }

    let rndSelectedTransform = (selectedTransform) => {
        <MM_cmp_single_frag_transf
            step=state.step
            transform=selectedTransform
            onBack=actUnselectTransform
            onInsertAbove
            onInsertBelow
            onUpdateCurrent
        />
    }

    let rndContent = () => {
        switch state.transformsParseErr {
            | Some(msg) => {
                <Col>
                    <span>
                        {React.string(`Please make sure the custom transforms script is non-blank and valid or `
                                                ++ `the "Use custom transforms" setting is unchecked.`)}
                    </span>
                    <pre>
                        {React.string(`Error: ${msg}`)}
                    </pre>
                </Col>
            }
            | None => {
                switch state.selectedTransform {
                    | None => {
                        switch state.availableTransforms {
                            | None => React.string(`Loading available transforms...`)
                            | Some(availableTransforms) => availableTransforms
                        }
                    }
                    | Some(selectedTransform) => rndSelectedTransform(selectedTransform)
                }
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            {rndContent()}
            <Divider/>
            {rndButtons()}
        </Col>
    </Paper>
}