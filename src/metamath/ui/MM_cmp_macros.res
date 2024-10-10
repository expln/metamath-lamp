open Expln_React_common
open MM_react_common
open Expln_React_Mui
open Raw_js_utils
open Local_storage_utils
open Expln_React_Modal
open Expln_utils_promise

type macroGlobalContext = string

type macro = {
    displayName: string,
    run:macroGlobalContext=>unit,
}

type collOfMacros = {
    id:int,
    version:int,
    displayName:string,
    displayNameEdit:string,
    scriptText:string,
    scriptTextEdit:string,
    macros:result<array<macro>,string>
}

type state = {
    nextId:int,
    activeCollOfMacrosId:int,
    collsOfMacros:array<collOfMacros>,
}

type collOfMacrosLocStor = {
    displayName:string,
    scriptText:string,
}

type stateLocStor = {
    activeCollOfMacrosIdx:int,
    collsOfMacros:array<collOfMacrosLocStor>,
}

let macrosGlobalContext:macroGlobalContext = %raw(`{}`)

let macrosCache:Belt_HashMapString.t<result<array<macro>,string>> = Belt_HashMapString.make(~hintSize=1)

let createMacroFromObject = (obj:{..}):macro => {
    let runFn = reqFuncExn(obj["run"], "'run' attribute of a macro must be a function.")
    {
        displayName: reqStrExn(obj["displayName"], "'displayName' attribute of a macro must be a string."),
        run: ctx => runFn(. ctx),
    }
}

let stringToMacros = (~displayName:string, ~script:string):result<array<macro>,string> => {
    switch invokeExnFunc(`Execute the script for '${displayName}'`, () => executeFunctionBody(script)) {
        | Error(msg) => Error(msg)
        | Ok(macros) => {
            invokeExnFunc(
                `Convert macros from the script for '${displayName}' to internal representation`, 
                () => macros["map"](. createMacroFromObject)
            )
        }
    }
}

let getMacrosCacheKey = (~displayName:string, ~script:string):string => {
    displayName ++ " ### " ++ script
}

let getMacrosFromCache = (~displayName:string, ~script:string):result<array<macro>,string> => {
    let cacheKey = getMacrosCacheKey(~displayName, ~script)
    switch macrosCache->Belt_HashMapString.get(cacheKey) {
        | Some(macros) => macros
        | None => {
            let macros = stringToMacros(~displayName, ~script)
            macrosCache->Belt_HashMapString.set(cacheKey, macros)
            macros
        }
    }
}

let removeStaleMacrosFromCache = (st:state):unit => {
    let validKeys = st.collsOfMacros
        ->Array.map(coll => getMacrosCacheKey(~displayName=coll.displayName, ~script=coll.scriptText))
    let allKeys = macrosCache->Belt_HashMapString.keysToArray
    allKeys->Array.forEach(key => {
        if (!(validKeys->Array.includes(key))) {
            macrosCache->Belt_HashMapString.remove(key)
        }
    })
}

let setMmExampleScript = MM_macros_set_mm_example.setMmExampleMacros
    ->String.replaceRegExp(%re("/\[!@#\]/g"), "`")
    ->String.replaceRegExp(%re("/\{!@#\}/g"), "$")
let setMmExampleDisplayName = "set.mm example macros"

let makeEmptyState = () => {
    {
        nextId:0,
        activeCollOfMacrosId:-1,
        collsOfMacros:[
            {
                id: -1,
                version: 1,
                displayName: setMmExampleDisplayName,
                displayNameEdit: setMmExampleDisplayName,
                scriptText: setMmExampleScript,
                scriptTextEdit: setMmExampleScript,
                macros: getMacrosFromCache(
                    ~displayName=setMmExampleDisplayName,
                    ~script=setMmExampleScript
                )
            }
        ],
    }
}

let addNewCollOfMacros = (st:state):state => {
    let displayName = `Macros-${(st.nextId+1)->Belt.Int.toString}`
    let scriptText = "return [{displayName:'empty macro', run:() => console.log('empty macro')}]"
    {
        nextId:st.nextId+1,
        activeCollOfMacrosId:st.nextId,
        collsOfMacros:Belt_Array.concatMany([
            [{
                id:st.nextId,
                version:1,
                displayName,
                displayNameEdit:displayName,
                scriptText,
                scriptTextEdit:scriptText,
                macros: getMacrosFromCache( ~displayName, ~script=scriptText )
            }],
            st.collsOfMacros, 
        ])
    }
}

let deleteMacros = (st:state, ~id:int):result<state,string> => {
    if (id < 0) {
        Error("Cannot delete these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collsOfMacros:st.collsOfMacros->Array.filter(macros => macros.id != id)
            }
        )
    }
}

let saveEdits = (st:state, ~id:int):result<state,string> => {
    if (id < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        let st = {
            ...st,
            collsOfMacros:st.collsOfMacros->Array.map(collOfMacros => {
                if (collOfMacros.id != id) {
                    collOfMacros
                } else {
                    {
                        ...collOfMacros,
                        version:collOfMacros.version+1,
                        displayName:collOfMacros.displayNameEdit,
                        scriptText:collOfMacros.scriptTextEdit,
                        macros: getMacrosFromCache(
                            ~displayName=collOfMacros.displayNameEdit,
                            ~script=collOfMacros.scriptTextEdit
                        )
                    }
                }
            })
        }
        removeStaleMacrosFromCache(st:state)
        Ok(st)
    }
}

let setDisplayNameEdit = (st:state, ~collOfMacrosId:int, ~displayNameEdit:string):result<state,string> => {
    if (collOfMacrosId < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collsOfMacros:st.collsOfMacros->Array.map(collOfMacros => {
                    if (collOfMacros.id != collOfMacrosId) {
                        collOfMacros
                    } else {
                        { ...collOfMacros, displayNameEdit, }
                    }
                })
            }
        )
    }
}

let setScriptTextEdit = (st:state, ~collOfMacrosId:int, ~scriptTextEdit:string):result<state,string> => {
    if (collOfMacrosId < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collsOfMacros:st.collsOfMacros->Array.map(collOfMacros => {
                    if (collOfMacros.id != collOfMacrosId) {
                        collOfMacros
                    } else {
                        { ...collOfMacros, scriptTextEdit, }
                    }
                })
            }
        )
    }
}

let resetEditsForCollOfMacros = (st:state, id:int):result<state,string> => {
    if (id < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collsOfMacros:st.collsOfMacros->Array.map(collOfMacros => {
                    if (collOfMacros.id != id) {
                        collOfMacros
                    } else {
                        {
                            ...collOfMacros,
                            displayNameEdit:collOfMacros.displayName,
                            scriptTextEdit:collOfMacros.scriptText,
                        }
                    }
                })
            }
        )
    }
}

let setActiveCollOfMacrosId = (st:state, newActiveCollOfMacrosId:int):result<state,string> => {
    switch st.collsOfMacros->Array.find(coll => coll.id == newActiveCollOfMacrosId) {
        | None => Ok(st)
        | Some(_) => {
            let st = {
                ...st,
                activeCollOfMacrosId:newActiveCollOfMacrosId
            }
            if (newActiveCollOfMacrosId < 0) {
                Ok(st)
            } else {
                st->resetEditsForCollOfMacros( newActiveCollOfMacrosId )
            }
        }
    }
}

let expBtnBaseStyle = ReactDOM.Style.make(
    ~display="block",
    ~color="grey", 
    ~border="1px solid",
    ~marginRight="3px",
    ~marginTop="13px",
    ~padding="1px 3px 0px 2px",
    ~fontFamily="courier",
    ~fontSize="10px",
    ~cursor="pointer",
    ()
)

let stateToStateLocStor = (st:state):stateLocStor => {
    {
        activeCollOfMacrosIdx: st.collsOfMacros->Array.findIndex(coll => coll.id == st.activeCollOfMacrosId),
        collsOfMacros: st.collsOfMacros->Array.filter(coll => coll.id >= 0)->Array.map(coll => {
            {
                displayName: coll.displayName,
                scriptText: coll.scriptText,
            }
        }),
    }
}

let stateLocStorToState = (ls:stateLocStor):state => {
    let defaultColls = [
        {
            id: -1,
            version: 1,
            displayName: setMmExampleDisplayName,
            displayNameEdit: setMmExampleDisplayName,
            scriptText: setMmExampleScript,
            scriptTextEdit: setMmExampleScript,
            macros: getMacrosFromCache(
                ~displayName=setMmExampleDisplayName,
                ~script=setMmExampleScript
            )
        }
    ]
    let collsOfMacros = Belt_Array.concatMany([
        ls.collsOfMacros->Array.mapWithIndex((coll,i) => {
            {
                id:i,
                version:1,
                displayName:coll.displayName,
                displayNameEdit:coll.displayName,
                scriptText:coll.scriptText,
                scriptTextEdit:coll.scriptText,
                macros: getMacrosFromCache(
                    ~displayName=coll.displayName,
                    ~script=coll.scriptText
                )
            }
        }),
        defaultColls,
    ])
    {
        nextId:ls.collsOfMacros->Array.length,
        activeCollOfMacrosId:
            switch collsOfMacros->Belt_Array.get(ls.activeCollOfMacrosIdx) {
                | None => 0
                | Some(coll) => coll.id
            },
        collsOfMacros,
    }
}

let macrosLocStorKey = "macros"
let saveStateToLocStor = (st:state):unit => {
    locStorWriteString(macrosLocStorKey, Expln_utils_common.stringify(st->stateToStateLocStor))
}

let readStateLocStorFromJsonStr = (jsonStr:string):result<stateLocStor,string> => {
    open Expln_utils_jsonParse
    parseJson(jsonStr, asObj(_, d=>{
        {
            activeCollOfMacrosIdx: d->int("activeCollOfMacrosIdx", ()),
            collsOfMacros: d->arr("collsOfMacros", asObj(_, d=>{
                {
                    displayName: d->str("displayName", ()),
                    scriptText: d->str("scriptText", ()),
                }
            }, ()), ())
        }
    }, ()), ())
}

let readStateFromLocStor = ():state => {
    switch locStorReadString(macrosLocStorKey) {
        | None => makeEmptyState()
        | Some(jsonStr) => {
            switch readStateLocStorFromJsonStr(jsonStr) {
                | Error(_) => makeEmptyState()
                | Ok(stateLocStor) => stateLocStor->stateLocStorToState
            }
        }
    }
}

let warningText = `
    Please be careful with what JavaScript code you use for macros. 
    This code will be executed by your browser "as is" meaning no safety measures will be taken to protect your browser from hurmful code.
    Before putting any code into this dialog please make sure you understand what that code does or make sure the code is not harmful.
`

@react.component
let make = (
    ~modalRef:modalRef,
    ~onClose:unit=>unit
) => {
    let (state, setState) = React.useState(readStateFromLocStor)
    let (isExpanded, setIsExpanded) = useStateFromLocalStorageBool(
        ~key="macros-dialog-is-expanded", ~default=false
    )
    let (hideWarning, setHideWarning) = useStateFromLocalStorageBool(
        ~key="custom-macros-js-warning-hide", ~default=false
    )

    let activeCollOfMacros = state.collsOfMacros
        ->Array.find(collOfMacros => collOfMacros.id == state.activeCollOfMacrosId)

    let thereAreChangesInActiveCollOfMacros = activeCollOfMacros->Belt.Option.map(coll => {
        coll.displayName != coll.displayNameEdit || coll.scriptText != coll.scriptTextEdit
    })->Belt_Option.getWithDefault(false)

    let activeCollOfMacrosIsReadOnly = activeCollOfMacros->Belt.Option.map(coll => {
        coll.id < 0
    })->Belt_Option.getWithDefault(false)

    let actShowWarning = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Warning_modal
                    title="Security warning"
                    warningText
                    hideInit=hideWarning
                    onHideChange = {b => setHideWarning(_ => b)}
                    onClose={()=>closeModal(modalRef, modalId)}
                />
            })
        })->ignore
    }

    React.useEffect1(() => {
        if (!activeCollOfMacrosIsReadOnly && !hideWarning && isExpanded) {
            actShowWarning()
        }
        None
    }, [activeCollOfMacrosIsReadOnly])

    let actActiveCollOfMacrosChange = (newActiveCollOfMacrosIdStr:string) => {
        switch newActiveCollOfMacrosIdStr->Belt_Int.fromString {
            | None => ()
            | Some(id) => {
                setState(st => {
                    switch st->setActiveCollOfMacrosId(id) {
                        | Error(_) => st
                        | Ok(st) => {
                            saveStateToLocStor(st)
                            st
                        }
                    }
                })
            }
        }
    }

    let actSetDisplayNameEdit = (str:string) => {
        switch activeCollOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->setDisplayNameEdit(~collOfMacrosId=id, ~displayNameEdit=str) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actSetScriptTextEdit = (str:string) => {
        switch activeCollOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->setScriptTextEdit(~collOfMacrosId=id, ~scriptTextEdit=str) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actSaveEdits = () => {
        switch activeCollOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->saveEdits(~id) {
                        | Error(_) => st
                        | Ok(st) => {
                            saveStateToLocStor(st)
                            st
                        }
                    }
                })
            }
        }
    }

    let deleteCollOfMacros = (collId:int) => {
        setState(prevSt => {
            switch prevSt->deleteMacros(~id=collId) {
                | Error(_) => prevSt
                | Ok(st) => {
                    switch st->setActiveCollOfMacrosId((st.collsOfMacros->Array.getUnsafe(0)).id) {
                        | Error(_) => prevSt
                        | Ok(st) => {
                            saveStateToLocStor(st)
                            st
                        }
                    }
                }
            }
        })
    }

    let actDeleteActiveCollOfMacros = () => {
        switch activeCollOfMacros {
            | None => ()
            | Some({id:collId, displayName}) => {
                openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                    updateModal(modalRef, modalId, () => {
                        <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                            <Col spacing=1.>
                                {
                                    React.string(`Delete "${displayName}" collection of macros?`)
                                }
                                <Row>
                                    <Button onClick={_=>closeModal(modalRef, modalId)}> {React.string("Cancel")} </Button>
                                    <Button
                                        onClick={_=>{
                                            closeModal(modalRef, modalId)
                                            deleteCollOfMacros(collId)
                                        }}
                                        variant=#contained
                                    > 
                                        {React.string("Delete")} 
                                    </Button>
                                </Row>
                            </Col>
                        </Paper>
                    })
                })->ignore
            }
        }
    }

    let actAddNewCollOfMacros = () => {
        setState(st => {
            let st = st->addNewCollOfMacros
            saveStateToLocStor(st)
            st
        })
    }

    let actToggleExpanded = () => {
        setIsExpanded(prev => !prev)
    }

    let rndError = msg => {
        <pre> { React.string( msg ) } </pre>
    }
    
    let rndExpBtn = ():reElem => {
        <a style=expBtnBaseStyle onClick={_=>actToggleExpanded()}>
            {React.string(if (isExpanded) {"-"} else {"+"})}
        </a>
    }

    let rndMacrosDropdown = () => {
        <Row >
            <FormControl size=#small >
                <Select
                    value={state.activeCollOfMacrosId->Belt_Int.toString}
                    onChange=evt2str(actActiveCollOfMacrosChange)
                    sx={"width": 330}
                >
                    {
                        state.collsOfMacros->Array.map(macros => {
                            let value = macros.id->Belt_Int.toString
                            <MenuItem key=value value>{React.string(macros.displayName)}</MenuItem>
                        })->React.array
                    }
                </Select>
            </FormControl>
            {rndExpBtn()}
        </Row>
    }

    let rndCancelBtn = () => {
        <Row>
            <Button onClick={_=>onClose()} variant=#outlined>
                {React.string("Cancel")}
            </Button>
            {
                if (isExpanded) {
                    React.null
                } else {
                    rndHiddenTextField(
                        ~onKeyDown=kbrdHnds([
                            kbrdClbkMake(~key=keyEsc, ~act=onClose, ()),
                        ]),
                        ()
                    )
                }
            }
        </Row>
    }

    let rndScriptTextField = (collOfMacros:collOfMacros) => {
        <TextField
            key={collOfMacros.id->Belt.Int.toString}
            size=#small
            style=ReactDOM.Style.make(~width=if(activeCollOfMacrosIsReadOnly) {"350px"} else {"300px"}, ())
            label="Script"
            autoFocus=true
            multiline=true
            maxRows=3
            value=collOfMacros.scriptTextEdit
            onChange=evt2str(actSetScriptTextEdit)
            onKeyDown=kbrdHnds([
                kbrdClbkMake(~key=keyEnter, ~act=actSaveEdits, ()),
                kbrdClbkMake(~key=keyEsc, ~act=onClose, ()),
            ])
            disabled=activeCollOfMacrosIsReadOnly
        />
    }

    let rndEditControls = () => {
        if (!isExpanded) {
            React.null
        } else {
            switch activeCollOfMacros {
                | None => React.null
                | Some(collOfMacros) => {
                    <Col>
                        <TextField
                            size=#small
                            style=ReactDOM.Style.make(~width="350px", ())
                            label="Display name" 
                            value=collOfMacros.displayNameEdit
                            onChange=evt2str(actSetDisplayNameEdit)
                            onKeyDown=kbrdHnds([
                                kbrdClbkMake(~key=keyEnter, ~act=actSaveEdits, ()),
                                kbrdClbkMake(~key=keyEsc, ~act=onClose, ()),
                            ])
                            disabled={collOfMacros.id < 0}
                        />
                        {
                            if (activeCollOfMacrosIsReadOnly) {
                                {rndScriptTextField(collOfMacros)}
                            } else {
                                <table>
                                    <tbody>
                                        <tr>
                                            <td>
                                                <IconButton title="Security warning" onClick={_=>actShowWarning()} 
                                                    color="orange" 
                                                >
                                                    <MM_Icons.Warning/>
                                                </IconButton>
                                            </td>
                                            <td style=ReactDOM.Style.make(~paddingLeft="5px", () )>
                                                {rndScriptTextField(collOfMacros)}
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            }
                        }
                        <Row>
                            <Button 
                                disabled={!thereAreChangesInActiveCollOfMacros} 
                                onClick={_=>actSaveEdits()} variant=#contained 
                            >
                                {React.string("Save changes")}
                            </Button>
                            <Button 
                                onClick={_=>actAddNewCollOfMacros()} variant=#outlined 
                            >
                                {React.string("Add new")}
                            </Button>
                            <Button
                                disabled=activeCollOfMacrosIsReadOnly 
                                onClick={_=>actDeleteActiveCollOfMacros()} variant=#outlined 
                            >
                                {React.string("Delete")}
                            </Button>
                        </Row>
                    </Col>
                }
            }
        }
    }

    let rndActiveMacros = () => {
        switch activeCollOfMacros {
            | None => {
                rndError(
                    `Cannot find a collection of macros with id ` 
                        ++ ` ${state.activeCollOfMacrosId->Belt_Int.toString}`
                )
            }
            | Some(collOfMacros) => {
                switch collOfMacros.macros {
                    | Error(msg) => {
                        rndError(`There was an error during initialization of this collection of macros:\n${msg}`)
                    }
                    | Ok(macros) => {
                        <ListCmp disablePadding=true key={collOfMacros.id->Belt_Int.toString}>
                            {
                                macros->Array.mapWithIndex((macro,i) => {
                                    <ListItem 
                                        key={
                                            collOfMacros.version->Belt_Int.toString 
                                                ++ "-" ++ i->Belt_Int.toString
                                        } 
                                        disablePadding=true 
                                    >
                                        <ListItemButton 
                                            onClick={_=>{
                                                onClose()
                                                macro.run(macrosGlobalContext)
                                            }}
                                        >
                                            <ListItemText>
                                                {React.string(macro.displayName)}
                                            </ListItemText>
                                        </ListItemButton>
                                    </ListItem>
                                })->React.array
                            }
                        </ListCmp>
                    }
                }
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col>
            {rndMacrosDropdown()}
            {rndEditControls()}
            {rndActiveMacros()}
            {rndCancelBtn()}
        </Col>
    </Paper>
}