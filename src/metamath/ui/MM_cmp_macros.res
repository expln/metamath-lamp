open Expln_React_common
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
    scriptText:string,
    macros:result<array<macro>,string>
}

type state = {
    nextId:int,
    activeMacrosId:int,
    allMacros:array<collectionOfMacros>,
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
        nextId:0,
        activeMacrosId:-1,
        allMacros:[
            {
                id: -1,
                version: 1,
                displayName: setMmExampleDisplayName,
                scriptText: setMmExampleScript,
                macros: stringToMacros(
                    ~displayName=setMmExampleDisplayName,
                    ~script=setMmExampleScript
                )
            }
        ],
    }
}

let addNewMacros = (st:state):state => {
    {
        nextId:st.nextId+1,
        activeMacrosId:st.nextId,
        allMacros:Belt_Array.concatMany([
            [{
                id:st.nextId,
                version:1,
                displayName:`Macros-${st.nextId->Belt.Int.toString}`,
                scriptText:"return [{displayName:'empty macro', run:() => console.log('empty macro')}]",
                macros:Ok([])
            }],
            st.allMacros, 
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
                allMacros:st.allMacros->Js_array2.filter(macros => macros.id != id)
            }
        )
    }
}

let updateMacros = (st:state, ~id:int, ~displayName:string, ~scriptText:string):result<state,string> => {
    if (id < 0) {
        Error("Cannot update these macros because they are read-only.")
    } else {
        Ok(
            {
                ...st,
                allMacros:st.allMacros->Js_array2.map(collectionOfMacros => {
                    if (collectionOfMacros.id != id) {
                        collectionOfMacros
                    } else {
                        {
                            ...collectionOfMacros,
                            version:collectionOfMacros.version+1,
                            displayName,
                            scriptText,
                        }
                    }
                })
            }
        )
    }
}

let setActiveMacrosId = (st:state, activeMacrosId:int):state => {
    {
        ...st,
        activeMacrosId
    }
}

@react.component
let make = (
    ~onClose:unit=>unit
) => {
    let (state, setState) = React.useState(makeEmptyState)

    let actActiveMacrosIdChange = (newActiveMacrosIdStr:string) => {
        switch newActiveMacrosIdStr->Belt_Int.fromString {
            | None => ()
            | Some(id) => setState(setActiveMacrosId(_, id))
        }
    }

    let rndMacrosDropdown = () => {
        <FormControl size=#small >
            <Select
                value={state.activeMacrosId->Belt_Int.toString}
                onChange=evt2str(actActiveMacrosIdChange)
            >
                {
                    state.allMacros->Js_array2.map(macros => {
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

    let rndActiveMacros = () => {
        switch state.allMacros->Js_array2.find(collectionOfMacros => collectionOfMacros.id == state.activeMacrosId) {
            | None => {
                <pre>
                    {
                        React.string(
                            `Cannot find a collection of macros with id ${state.activeMacrosId->Belt_Int.toString}`
                        )
                    }
                </pre>
            }
            | Some(collectionOfMacros) => {
                switch collectionOfMacros.macros {
                    | Error(msg) => {
                        <pre>
                            {
                                React.string(
                                    `There was an error during initialization of this collection of macros:\n${msg}`
                                )
                            }
                        </pre>
                    }
                    | Ok(macros) => {
                        <List disablePadding=true>
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
            {rndActiveMacros()}
            {rndCancelBtn()}
        </Col>
    </Paper>
}