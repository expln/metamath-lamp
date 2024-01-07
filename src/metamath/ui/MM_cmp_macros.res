open Expln_React_common
open Expln_React_Mui
open Raw_js_utils

type macro = {
    displayName: string,
    run:unit=>unit,
}

type macros = {
    id:int,
    displayName:string,
    scriptText:string,
    macros:result<array<macro>,string>
}

type state = {
    nextId:int,
    activeMacros:int,
    allMacros:array<macros>,
}

let makeEmptyState = () => {
    {
        nextId:0,
        activeMacros:-1,
        allMacros:[
            {
                id:-1,
                displayName:"set.mm example",
                scriptText:MM_macros_set_mm_example.setMmExampleMacros,
                macros:Ok([])
            }
        ],
    }
}

let addNewMacros = (st:state):state => {
    {
        nextId:st.nextId+1,
        activeMacros:st.nextId,
        allMacros:Belt_Array.concatMany([
            [{
                id:st.nextId,
                displayName:`Macros-${st.nextId->Belt.Int.toString}`,
                scriptText:"return []",
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
                allMacros:st.allMacros->Js_array2.map(macros => {
                    if (macros.id != id) {
                        macros
                    } else {
                        {
                            ...macros,
                            displayName,
                            scriptText,
                        }
                    }
                })
            }
        )
    }
}

let setActiveMacros = (st:state, activeMacros:int):state => {
    {
        ...st,
        activeMacros
    }
}

@react.component
let make = (
    ~onClose:unit=>unit
) => {
    let (state, setState) = React.useState(makeEmptyState)

    let actActiveMacrosChange = (newActiveMacrosIdStr:string) => {
        switch newActiveMacrosIdStr->Belt_Int.fromString {
            | None => ()
            | Some(id) => setState(setActiveMacros(_, id))
        }
    }

    let rndMacrosDropdown = () => {
        <FormControl size=#small >
            <Select
                value={state.activeMacros->Belt_Int.toString}
                onChange=evt2str(actActiveMacrosChange)
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

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col>
            {rndMacrosDropdown()}
            {rndCancelBtn()}
        </Col>
    </Paper>
}