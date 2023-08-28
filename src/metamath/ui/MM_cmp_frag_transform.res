open MM_syntax_tree
open Expln_React_Modal
open MM_react_common
open Expln_React_common
open Expln_React_Mui
open MM_cmp_single_frag_transf
open MM_wrk_frag_transform

let transformsTextCache:ref<array<string>> = ref([])
let allTransformsCache: ref<array<fragmentTransform>> = ref([])

type state = {
    selection:selection,
    transformsParseErr:option<string>,
    availableTransforms:option<reElem>,
    selectedTransform: option<fragmentTransform>,
}

let createInitialState = (
    ~selectedSubtree:childNode,
):state => {
    {
        selection: syntaxTreeToSelection(selectedSubtree),
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
    ~onCancel:unit=>unit,
    ~selectedSubtree:childNode,
    ~transformsText:array<string>,
    ~onInsertAbove:string=>unit,
    ~onInsertBelow:string=>unit,
    ~onUpdateCurrent:string=>unit,
) => {
    let (state, setState) = React.useState(() => createInitialState(~selectedSubtree))

    let rndAvailableTransforms = (availableTransforms:array<fragmentTransform>):result<reElem,string> => {
        let param = {"selection":state.selection}
        let listItems = unsafeFunc( "Listing available transforms", () => {
            availableTransforms->Js_array2.mapi((availableTransform,i) => {
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
                    <List disablePadding=true>
                        listItems
                    </List>
                )
            }
        }
    }

    React.useEffect0(() => {
        let param = {"selection":state.selection}
        if (transformsText != transformsTextCache.contents) {
            let allTransformsRef = ref([])
            let availableTransformsElem = arrStrToFragTransforms(transformsText)
                ->Belt.Result.flatMap(allTransforms => {
                    allTransformsRef := allTransforms
                    unsafeFunc(
                        "Getting available transforms",
                        () => allTransforms->Js_array2.filter(tr => tr.canApply(param))
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
            let availableTransformsElem = unsafeFunc(
                    "Getting available transforms from cache",
                    () => allTransformsCache.contents->Js_array2.filter(tr => tr.canApply(param))
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
        </Row>
    }

    let rndSelectedTransform = (selectedTransform) => {
        <MM_cmp_single_frag_transf
            selection={state.selection}
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
                <span>
                    {React.string(`Error: ${msg}`)}
                </span>
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