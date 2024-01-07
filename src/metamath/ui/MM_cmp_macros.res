open Expln_React_common
open MM_react_common
open Expln_React_Mui
open Raw_js_utils

type macro = {
    displayName: string,
    run:unit=>unit,
}

type collectionOfMacros = {
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
    activeCollectionOfMacrosId:int,
    collectionsOfMacros:array<collectionOfMacros>,
}

let createMacroFromObject = (obj:{..}):macro => {
    let runFn = reqFuncExn(obj["run"], "'run' attribute of a macro must be a function.")
    {
        displayName: reqStrExn(obj["displayName"], "'displayName' attribute of a macro must be a string."),
        run: () => runFn(.),
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

let makeEmptyState = () => {
    let setMmExampleScript = MM_macros_set_mm_example2.setMmExampleMacros
    let setMmExampleDisplayName = "set.mm example"
    {
        nextId:1,
        activeCollectionOfMacrosId:-1,
        collectionsOfMacros:[
            {
                id: -1,
                version: 1,
                displayName: setMmExampleDisplayName,
                displayNameEdit: setMmExampleDisplayName,
                scriptText: setMmExampleScript,
                scriptTextEdit: setMmExampleScript,
                macros: stringToMacros(
                    ~displayName=setMmExampleDisplayName,
                    ~script=setMmExampleScript
                )
            }
        ],
    }
}

let addNewCollectionOfMacros = (st:state):state => {
    let displayName = `Macros-${st.nextId->Belt.Int.toString}`
    let scriptText = "return [{displayName:'empty macro', run:() => console.log('empty macro')}]"
    {
        nextId:st.nextId+1,
        activeCollectionOfMacrosId:st.nextId,
        collectionsOfMacros:Belt_Array.concatMany([
            [{
                id:st.nextId,
                version:1,
                displayName,
                displayNameEdit:displayName,
                scriptText,
                scriptTextEdit:scriptText,
                macros: stringToMacros( ~displayName, ~script=scriptText )
            }],
            st.collectionsOfMacros, 
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
                collectionsOfMacros:st.collectionsOfMacros->Js_array2.filter(macros => macros.id != id)
            }
        )
    }
}

let saveEdits = (st:state, ~id:int):result<state,string> => {
    if (id < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collectionsOfMacros:st.collectionsOfMacros->Js_array2.map(collectionOfMacros => {
                    if (collectionOfMacros.id != id) {
                        collectionOfMacros
                    } else {
                        {
                            ...collectionOfMacros,
                            version:collectionOfMacros.version+1,
                            displayName:collectionOfMacros.displayNameEdit,
                            scriptText:collectionOfMacros.scriptTextEdit,
                            macros: stringToMacros(
                                ~displayName=collectionOfMacros.displayNameEdit,
                                ~script=collectionOfMacros.scriptTextEdit
                            )
                        }
                    }
                })
            }
        )
    }
}

let setDisplayNameEdit = (st:state, ~collectionOfMacrosId:int, ~displayNameEdit:string):result<state,string> => {
    if (collectionOfMacrosId < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collectionsOfMacros:st.collectionsOfMacros->Js_array2.map(collectionOfMacros => {
                    if (collectionOfMacros.id != collectionOfMacrosId) {
                        collectionOfMacros
                    } else {
                        { ...collectionOfMacros, displayNameEdit, }
                    }
                })
            }
        )
    }
}

let setScriptTextEdit = (st:state, ~collectionOfMacrosId:int, ~scriptTextEdit:string):result<state,string> => {
    if (collectionOfMacrosId < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collectionsOfMacros:st.collectionsOfMacros->Js_array2.map(collectionOfMacros => {
                    if (collectionOfMacros.id != collectionOfMacrosId) {
                        collectionOfMacros
                    } else {
                        { ...collectionOfMacros, scriptTextEdit, }
                    }
                })
            }
        )
    }
}

let resetEditsForCollectionOfMacros = (st:state, id:int):result<state,string> => {
    if (id < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collectionsOfMacros:st.collectionsOfMacros->Js_array2.map(collectionOfMacros => {
                    if (collectionOfMacros.id != id) {
                        collectionOfMacros
                    } else {
                        {
                            ...collectionOfMacros,
                            displayNameEdit:collectionOfMacros.displayName,
                            scriptTextEdit:collectionOfMacros.scriptText,
                        }
                    }
                })
            }
        )
    }
}

let setActiveCollectionOfMacrosId = (st:state, newActiveCollectionOfMacrosId:int):result<state,string> => {
    switch st.collectionsOfMacros->Js_array2.find(coll => coll.id == newActiveCollectionOfMacrosId) {
        | None => Ok(st)
        | Some(newActiveCollectionOfMacros) => {
            let st = {
                ...st,
                activeCollectionOfMacrosId:newActiveCollectionOfMacrosId
            }
            if (newActiveCollectionOfMacrosId < 0) {
                Ok(st)
            } else {
                st->resetEditsForCollectionOfMacros( newActiveCollectionOfMacrosId )
            }
        }
    }
}

@react.component
let make = (
    ~onClose:unit=>unit
) => {
    let (state, setState) = React.useState(makeEmptyState)

    let activeCollectionOfMacros = state.collectionsOfMacros
        ->Js_array2.find(collectionOfMacros => collectionOfMacros.id == state.activeCollectionOfMacrosId)
    let thereAreChangesInActiveCollectionOfMacros = activeCollectionOfMacros->Belt.Option.map(coll => {
        coll.displayName != coll.displayNameEdit || coll.scriptText != coll.scriptTextEdit
    })->Belt_Option.getWithDefault(false)

    let actActiveCollectionOfMacrosChange = (newActiveCollectionOfMacrosIdStr:string) => {
        switch newActiveCollectionOfMacrosIdStr->Belt_Int.fromString {
            | None => ()
            | Some(id) => {
                setState(st => {
                    switch st->setActiveCollectionOfMacrosId(id) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actSetDisplayNameEdit = (str:string) => {
        switch activeCollectionOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->setDisplayNameEdit(~collectionOfMacrosId=id, ~displayNameEdit=str) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actSetScriptTextEdit = (str:string) => {
        switch activeCollectionOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->setScriptTextEdit(~collectionOfMacrosId=id, ~scriptTextEdit=str) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actSaveEdits = () => {
        switch activeCollectionOfMacros {
            | None => ()
            | Some({id}) => {
                setState(st => {
                    switch st->saveEdits(~id) {
                        | Error(_) => st
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actAddNewCollectionOfMacros = () => {
        setState(addNewCollectionOfMacros)
    }

    let rndError = msg => {
        <pre> { React.string( msg ) } </pre>
    }

    let rndMacrosDropdown = () => {
        <FormControl size=#small >
            <Select
                value={state.activeCollectionOfMacrosId->Belt_Int.toString}
                onChange=evt2str(actActiveCollectionOfMacrosChange)
                sx={"width": 300}
            >
                {
                    state.collectionsOfMacros->Js_array2.map(macros => {
                        let value = macros.id->Belt_Int.toString
                        <MenuItem key=value value>{React.string(macros.displayName)}</MenuItem>
                    })->React.array
                }
            </Select>
        </FormControl>
    }

    let rndCancelBtn = () => {
        <Button onClick={_=>onClose()} variant=#outlined>
            {React.string("Cancel")}
        </Button>
    }

    let rndEditControls = () => {
        switch activeCollectionOfMacros {
            | None => React.null
            | Some(collectionOfMacros) => {
                <Col>
                    <TextField
                        size=#small
                        style=ReactDOM.Style.make(~width="300px", ())
                        label="Display name" 
                        value=collectionOfMacros.displayNameEdit
                        onChange=evt2str(actSetDisplayNameEdit)
                        onKeyDown=kbrdHnd(~key=keyEnter, ~act=actSaveEdits, ())
                        disabled={collectionOfMacros.id < 0}
                    />
                    <TextField
                        size=#small
                        style=ReactDOM.Style.make(~width="300px", ())
                        label="Script"
                        autoFocus=true
                        multiline=true
                        maxRows=3
                        value=collectionOfMacros.scriptTextEdit
                        onChange=evt2str(actSetScriptTextEdit)
                        onKeyDown=kbrdHnd(~key=keyEnter, ~act=actSaveEdits, ())
                        disabled={collectionOfMacros.id < 0}
                    />
                    <Row>
                        <Button 
                            disabled={!thereAreChangesInActiveCollectionOfMacros} 
                            onClick={_=>actSaveEdits()} variant=#contained 
                        >
                            {React.string("Save changes")}
                        </Button>
                        <Button 
                            onClick={_=>actAddNewCollectionOfMacros()} variant=#outlined 
                        >
                            {React.string("Add new")}
                        </Button>
                    </Row>
                </Col>
            }
        }
    }

    let rndActiveMacros = () => {
        switch activeCollectionOfMacros {
            | None => {
                rndError(
                    `Cannot find a collection of macros with id ` 
                        ++ ` ${state.activeCollectionOfMacrosId->Belt_Int.toString}`
                )
            }
            | Some(collectionOfMacros) => {
                switch collectionOfMacros.macros {
                    | Error(msg) => {
                        rndError(`There was an error during initialization of this collection of macros:\n${msg}`)
                    }
                    | Ok(macros) => {
                        <List disablePadding=true key={collectionOfMacros.id->Belt_Int.toString}>
                            {
                                macros->Js_array2.mapi((macro,i) => {
                                    <ListItem 
                                        key={
                                            collectionOfMacros.version->Belt_Int.toString 
                                                ++ "-" ++ i->Belt_Int.toString
                                        } 
                                        disablePadding=true 
                                    >
                                        <ListItemButton 
                                            onClick={_=>{
                                                onClose()
                                                macro.run()
                                            }}
                                        >
                                            <ListItemText>
                                                {React.string(macro.displayName)}
                                            </ListItemText>
                                        </ListItemButton>
                                    </ListItem>
                                })->React.array
                            }
                        </List>
                    }
                }
            }
        }
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col>
            {rndCancelBtn()}
            {rndMacrosDropdown()}
            {rndEditControls()}
            {rndActiveMacros()}
            {rndCancelBtn()}
        </Col>
    </Paper>
}