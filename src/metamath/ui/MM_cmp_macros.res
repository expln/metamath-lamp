open Expln_React_common
open MM_react_common
open Expln_React_Mui
open Raw_js_utils
open Local_storage_utils

type macro = {
    displayName: string,
    run:unit=>unit,
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
        activeCollOfMacrosId:-1,
        collsOfMacros:[
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

let addNewCollOfMacros = (st:state):state => {
    let displayName = `Macros-${st.nextId->Belt.Int.toString}`
    let scriptText = "return [{displayName:'empty macro', run:() => console.log('empty macro')}]"
    {
        ...st,
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
                macros: stringToMacros( ~displayName, ~script=scriptText )
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
                collsOfMacros:st.collsOfMacros->Js_array2.filter(macros => macros.id != id)
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
                collsOfMacros:st.collsOfMacros->Js_array2.map(collOfMacros => {
                    if (collOfMacros.id != id) {
                        collOfMacros
                    } else {
                        {
                            ...collOfMacros,
                            version:collOfMacros.version+1,
                            displayName:collOfMacros.displayNameEdit,
                            scriptText:collOfMacros.scriptTextEdit,
                            macros: stringToMacros(
                                ~displayName=collOfMacros.displayNameEdit,
                                ~script=collOfMacros.scriptTextEdit
                            )
                        }
                    }
                })
            }
        )
    }
}

let setDisplayNameEdit = (st:state, ~collOfMacrosId:int, ~displayNameEdit:string):result<state,string> => {
    if (collOfMacrosId < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                collsOfMacros:st.collsOfMacros->Js_array2.map(collOfMacros => {
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
                collsOfMacros:st.collsOfMacros->Js_array2.map(collOfMacros => {
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
                collsOfMacros:st.collsOfMacros->Js_array2.map(collOfMacros => {
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
    switch st.collsOfMacros->Js_array2.find(coll => coll.id == newActiveCollOfMacrosId) {
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

@react.component
let make = (
    ~onClose:unit=>unit
) => {
    let (state, setState) = React.useState(makeEmptyState)
    let (isExpanded, setIsExpanded) = useStateFromLocalStorageBool(
        ~key="macros-dialog-is-expanded", ~default=false
    )

    let activeCollOfMacros = state.collsOfMacros
        ->Js_array2.find(collOfMacros => collOfMacros.id == state.activeCollOfMacrosId)
    let thereAreChangesInActiveCollOfMacros = activeCollOfMacros->Belt.Option.map(coll => {
        coll.displayName != coll.displayNameEdit || coll.scriptText != coll.scriptTextEdit
    })->Belt_Option.getWithDefault(false)

    let actActiveCollOfMacrosChange = (newActiveCollOfMacrosIdStr:string) => {
        switch newActiveCollOfMacrosIdStr->Belt_Int.fromString {
            | None => ()
            | Some(id) => {
                setState(st => {
                    switch st->setActiveCollOfMacrosId(id) {
                        | Error(_) => st
                        | Ok(st) => st
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
                        | Ok(st) => st
                    }
                })
            }
        }
    }

    let actAddNewCollOfMacros = () => {
        setState(addNewCollOfMacros)
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
                    sx={"width": 300}
                >
                    {
                        state.collsOfMacros->Js_array2.map(macros => {
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
        <Button onClick={_=>onClose()} variant=#outlined>
            {React.string("Cancel")}
        </Button>
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
                            style=ReactDOM.Style.make(~width="320px", ())
                            label="Display name" 
                            value=collOfMacros.displayNameEdit
                            onChange=evt2str(actSetDisplayNameEdit)
                            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actSaveEdits, ())
                            disabled={collOfMacros.id < 0}
                        />
                        <TextField
                            size=#small
                            style=ReactDOM.Style.make(~width="320px", ())
                            label="Script"
                            autoFocus=true
                            multiline=true
                            maxRows=3
                            value=collOfMacros.scriptTextEdit
                            onChange=evt2str(actSetScriptTextEdit)
                            onKeyDown=kbrdHnd(~key=keyEnter, ~act=actSaveEdits, ())
                            disabled={collOfMacros.id < 0}
                        />
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
                        <List disablePadding=true key={collOfMacros.id->Belt_Int.toString}>
                            {
                                macros->Js_array2.mapi((macro,i) => {
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