open Expln_React_common
open MM_react_common
open Expln_React_Mui
open Raw_js_utils
open Local_storage_utils
open Expln_React_Modal
open Expln_utils_promise

type macroModule = {
    nameEdit:string,
    isActive:bool,
    isActiveEdit:bool,
    scriptText:string,
    scriptTextEdit:string,
    scriptExecutionErr:option<string>,
    macros:array<string>,
}

type state = {
    selectedMacroModuleName:string,
    macroModules:Belt_MapString.t<macroModule>,
}

type macroModuleLocStor = {
    name:string,
    isActive:bool,
    scriptText:string,
}

type stateLocStor = {
    selectedMacroModuleName:string,
    macroModules:array<macroModuleLocStor>,
}

let scriptCache:Belt_HashMapString.t<result<unit,string>> = Belt_HashMapString.make(~hintSize=1)

let getScriptCacheKey = (~macroModuleName:string, ~script:string):string => {
    macroModuleName ++ " ### " ++ script
}

let runScript = (~macroModuleName:string, ~script:string):result<unit,string> => {
    let key = getScriptCacheKey(~macroModuleName, ~script)
    switch scriptCache->Belt_HashMapString.get(key) {
        | Some(result) => result
        | None => {
            MM_api_macros.setOverrideMacroModuleName(Some(macroModuleName))
            let result = switch invokeExnFunc(
                `Execute the script for '${macroModuleName}'`, 
                () => executeAsyncFunctionBody(script)
            ) {
                | Error(msg) => Error(msg)
                | Ok(_) => Ok(())
            }
            MM_api_macros.setOverrideMacroModuleName(None)
            scriptCache->Belt_HashMapString.set(key, result)
            result
        }
    }
}

let removeStaleDataFromScriptCache = (st:state):unit => {
    let validKeys = []
    st.macroModules->Belt_MapString.forEach((modName,mod) => {
        validKeys->Array.push(getScriptCacheKey(~macroModuleName=modName, ~script=mod.scriptText))
    })
    let allKeys = scriptCache->Belt_HashMapString.keysToArray
    allKeys->Array.forEach(key => {
        if (!(validKeys->Array.includes(key))) {
            scriptCache->Belt_HashMapString.remove(key)
        }
    })
}

let textToScript = (text:string):string => {
    text
        ->String.replaceRegExp(%re("/\[!@#\]/g"), "`")
        ->String.replaceRegExp(%re("/\{!@#\}/g"), "$")
}

let predefinedMacroModuleScripts = Belt_HashMapString.fromArray([
    ("set.mm example macros", textToScript(MM_macros_set_mm_example.setMmExampleMacros)),
    ("MMJ2", textToScript(MM_macros_mmj2.mmj2Macros)),
])

let isPredefinedMacroModule = (macroModuleName:string):bool => {
    predefinedMacroModuleScripts->Belt_HashMapString.has(macroModuleName)
}

let stateToStateLocStor = (st:state):stateLocStor => {
    {
        selectedMacroModuleName: st.selectedMacroModuleName,
        // no need to save modules without scripts because either they have been added via the console 
        // or this is a user mistake
        macroModules: 
            st.macroModules->Belt_MapString.toArray
                ->Array.filter(((_,mod)) => mod.scriptText->String.trim != "")
                ->Array.map(((modName,mod)) => {
                    {
                        name: modName,
                        isActive: mod.isActive,
                        scriptText: isPredefinedMacroModule(modName) ? "" : mod.scriptText,
                    }
                }),
    }
}

let makeEmptyStateLocStor = () => {
    {
        selectedMacroModuleName:"",
        macroModules:[],
    }
}

let macrosLocStorKey = "macros"

let readStateFromLocStor = ():state => {
    //load predefined modules
    let macroModules:Belt_HashMapString.t<macroModule> = predefinedMacroModuleScripts
        ->Belt_HashMapString.toArray
        ->Array.map(((modName,script)) => {
            (
                modName,
                {
                    nameEdit:modName,
                    isActive:false,
                    isActiveEdit:false,
                    scriptText:script,
                    scriptTextEdit:script,
                    scriptExecutionErr:None,
                    macros:[],
                }
            )
        })
        ->Belt_HashMapString.fromArray

    //load modules from the local storage
    let stateLocStor: stateLocStor = switch locStorReadString(macrosLocStorKey) {
        | None => makeEmptyStateLocStor()
        | Some(jsonStr) => {
            open Expln_utils_jsonParse
            switch parseJson(jsonStr, asObj(_, d=>{
                {
                    selectedMacroModuleName: d->str("selectedMacroModuleName"),
                    macroModules: d->arr("macroModules", asObj(_, d=>{
                        {
                            name: d->str("name", ~validator=n=>Ok(String.trim(n))),
                            isActive: d->bool("isActive"),
                            scriptText: d->str("scriptText"),
                        }
                    }))
                }
            })) {
                | Error(_) => makeEmptyStateLocStor()
                | Ok(stateLocStor) => stateLocStor
            }
        }
    }
    stateLocStor.macroModules->Array.forEach(mod => {
        switch macroModules->Belt_HashMapString.get(mod.name) {
            | None => {
                macroModules->Belt_HashMapString.set(
                    mod.name, 
                    {
                        nameEdit:mod.name,
                        isActive:mod.isActive,
                        isActiveEdit:mod.isActive,
                        scriptText:mod.scriptText,
                        scriptTextEdit:mod.scriptText,
                        scriptExecutionErr:None,
                        macros:[],
                    }
                )
            }
            | Some(existingMod) => {
                if (isPredefinedMacroModule(mod.name)) {
                    macroModules->Belt_HashMapString.set(
                        mod.name, 
                        {
                            ...existingMod,
                            isActive:mod.isActive,
                            isActiveEdit:mod.isActive,
                        }
                    )
                }
            }
        }
    })

    //add modules registered by the user via the console
    MM_api_macros.listRegisteredMacroModules()->Array.forEach(registeredModName => {
        if (!(macroModules->Belt_HashMapString.has(registeredModName))) {
            macroModules->Belt_HashMapString.set(
                registeredModName, 
                {
                    nameEdit:registeredModName,
                    isActive:true,
                    isActiveEdit:true,
                    scriptText:"",
                    scriptTextEdit:"",
                    scriptExecutionErr:None,
                    macros:MM_api_macros.listRegisteredMacrosInModule(registeredModName)->Option.getOr([]),
                }
            )
        }
    })

    //delete modules with empty names
    let emptyNames = []
    macroModules->Belt_HashMapString.forEach((modName,_) => {
        if (modName->String.trim == "") {
            emptyNames->Array.push(modName)
        }
    })
    emptyNames->Array.forEach(modName => macroModules->Belt_HashMapString.remove(modName))

    //run active scripts
    macroModules->Belt_HashMapString.forEach((modName,mod) => {
        if (mod.isActive && mod.scriptText->String.trim != "") {
            let scriptExecutionErr = switch runScript(~macroModuleName=modName, ~script=mod.scriptText) {
                | Error(msg) => Some(msg)
                | Ok(_) => None
            }
            let macros = MM_api_macros.listRegisteredMacrosInModule(modName)->Option.getOr([])
            macroModules->Belt_HashMapString.set( modName, { ...mod, scriptExecutionErr, macros, } )
        }
    })

    //make the final state object
    let allLoadedMacroModules = macroModules->Belt_HashMapString.keysToArray
    {
        selectedMacroModuleName: allLoadedMacroModules->Array.find(n => n == stateLocStor.selectedMacroModuleName)
            ->Option.getOr(allLoadedMacroModules[0]->Option.getExn(~message="MM_cmp_macros.readStateFromLocStor.1")),
        macroModules: macroModules->Belt_HashMapString.toArray->Belt_MapString.fromArray,
    }
}

let reloadState = (st:state):state => {
    removeStaleDataFromScriptCache(st)
    locStorWriteString(macrosLocStorKey, Expln_utils_common.stringify(st->stateToStateLocStor))
    readStateFromLocStor()
}

let addNewMacroModule = (st:state):state => {
    {
        selectedMacroModuleName:"",
        macroModules: st.macroModules->Belt_MapString.set(
            "",
            {
                nameEdit:"New macros",
                isActive:true,
                isActiveEdit:true,
                scriptText:"",
                scriptTextEdit:"",
                scriptExecutionErr:None,
                macros:[],
            }
        )
    }
}

let deleteMacroModule = (st:state, ~moduleName:string):result<state,string> => {
    if (isPredefinedMacroModule(moduleName)) {
        Error("Cannot delete a predefined macro module.")
    } else {
        MM_api_macros.unregisterMacroModule(moduleName)
        Ok( 
            { 
                ...st, 
                macroModules:st.macroModules->Belt_MapString.remove(moduleName) 
            }->reloadState 
        )
    }
}

let saveEdits = (st:state, ~moduleName:string):result<state,string> => {
    switch st.macroModules->Belt_MapString.get(moduleName) {
        | None => Ok(st)
        | Some(mod) => {
            if (
                isPredefinedMacroModule(moduleName) 
                    && (moduleName != mod.nameEdit || mod.scriptText != mod.scriptTextEdit)
            ) {
                Error("Cannot update a predefined macro module.")
            } else {
                if (moduleName != mod.nameEdit && st.macroModules->Belt_MapString.has(mod.nameEdit)) {
                    Error(`A module with name "${mod.nameEdit}" already exists. Please choose another name.`)
                } else if (mod.nameEdit->String.trim == "") {
                    Error(`A module name must not be empty.`)
                } else if (mod.scriptTextEdit->String.trim == "") {
                    Error(`The script must not be empty.`)
                } else {
                    Ok(
                        {
                            selectedMacroModuleName:
                                moduleName == st.selectedMacroModuleName ? mod.nameEdit : st.selectedMacroModuleName,
                            macroModules: st.macroModules
                                ->Belt_MapString.remove(moduleName)
                                ->Belt_MapString.set(
                                    mod.nameEdit,
                                    { ...mod, isActive:mod.isActiveEdit, scriptText:mod.scriptTextEdit, }
                                ),
                        }->reloadState
                    )
                }
            }
        }
    }
}

let setNameEdit = (st:state, ~moduleName:string, ~nameEdit:string):result<state,string> => {
    if (isPredefinedMacroModule(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        switch st.macroModules->Belt_MapString.get(moduleName) {
            | None => Ok(st)
            | Some(mod) => {
                Ok( 
                    {
                        ...st, 
                        macroModules:st.macroModules->Belt_MapString.set( moduleName, { ...mod, nameEdit, } ), 
                    }
                )
            }
        }
    }
}

let setIsActiveEdit = (st:state, ~moduleName:string, ~isActiveEdit:bool):result<state,string> => {
    switch st.macroModules->Belt_MapString.get(moduleName) {
        | None => Ok(st)
        | Some(mod) => {
            Ok( 
                {
                    ...st, 
                    macroModules:st.macroModules->Belt_MapString.set( moduleName, { ...mod, isActiveEdit, } ), 
                }
            )
        }
    }
}

let setScriptTextEdit = (st:state, ~moduleName:string, ~scriptTextEdit:string):result<state,string> => {
    if (isPredefinedMacroModule(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        switch st.macroModules->Belt_MapString.get(moduleName) {
            | None => Ok(st)
            | Some(mod) => {
                Ok( 
                    {
                        ...st, 
                        macroModules:st.macroModules->Belt_MapString.set( moduleName, { ...mod, scriptTextEdit, } ), 
                    }
                )
            }
        }
    }
}

let resetEditsForMacroModule = (st:state, ~moduleName:string):result<state,string> => {
    if (isPredefinedMacroModule(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        switch st.macroModules->Belt_MapString.get(moduleName) {
            | None => Ok(st)
            | Some(mod) => {
                Ok( 
                    {
                        ...st, 
                        macroModules:st.macroModules->Belt_MapString.set( 
                            moduleName, 
                            { 
                                ...mod, 
                                nameEdit:moduleName, 
                                isActiveEdit:mod.isActive, 
                                scriptTextEdit:mod.scriptText,
                            } 
                        ), 
                    }->reloadState 
                )
            }
        }
    }
}

let setSelectedMacroModuleName = (st:state, newSelectedMacroModuleName:string):result<state,string> => {
    switch st.macroModules->Belt_MapString.get(newSelectedMacroModuleName) {
        | None => Ok(st)
        | Some(_) => {
            Ok(
                {
                    ...st,
                    selectedMacroModuleName:newSelectedMacroModuleName
                }->reloadState
            )
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

    let selectedMacroModule = state.macroModules->Belt_MapString.get(state.selectedMacroModuleName)

    let thereAreChangesInSelectedMacroModule = selectedMacroModule->Belt.Option.map(mod => {
        state.selectedMacroModuleName != mod.nameEdit 
            || mod.isActive != mod.isActiveEdit 
            || mod.scriptText != mod.scriptTextEdit
    })->Belt_Option.getWithDefault(false)

    let selectedMacroModuleIsReadOnly = isPredefinedMacroModule(state.selectedMacroModuleName)

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
        if (!selectedMacroModuleIsReadOnly && !hideWarning && isExpanded) {
            actShowWarning()
        }
        None
    }, [selectedMacroModuleIsReadOnly])

    let updateState = (update:state=>result<state,string>):unit => {
        setState(st => {
            switch update(st) {
                | Error(msg) => {
                    openInfoDialog( ~modalRef, ~content={msg->React.string} )
                    st
                }
                | Ok(st) => st
            }
        })
    }

    let updateSelectedMacroModule = (update:(state,string)=>result<state,string>):unit => {
        switch selectedMacroModule {
            | None => ()
            | Some(_) => {
                updateState(update(_, state.selectedMacroModuleName))
            }
        }
    }

    let actSelectedMacroModuleNameChange = (newSelectedMacroModuleName:string) => {
        updateState(setSelectedMacroModuleName(_, newSelectedMacroModuleName))
    }

    let actSetNameEdit = (nameEdit:string) => {
        updateSelectedMacroModule((st,moduleName) => setNameEdit(st, ~moduleName, ~nameEdit))
    }

    let actSetIsActiveEdit = (isActiveEdit:bool) => {
        updateSelectedMacroModule((st,moduleName) => setIsActiveEdit(st, ~moduleName, ~isActiveEdit))
    }

    let actSetScriptTextEdit = (scriptTextEdit:string) => {
        updateSelectedMacroModule((st,moduleName) => setScriptTextEdit(st, ~moduleName, ~scriptTextEdit))
    }

    let actSaveEdits = () => {
        updateSelectedMacroModule((st,moduleName) => saveEdits(st, ~moduleName))
    }

    let activateSelectedMacroModule = () => {
        updateSelectedMacroModule((st,moduleName) => {
            setIsActiveEdit(st, ~moduleName, ~isActiveEdit=true)->Result.flatMap(saveEdits(_, ~moduleName))
        })
    }

    let actDeleteSelectedMacroModule = () => {
        switch selectedMacroModule {
            | None => ()
            | Some(_) => {
                openOkCancelDialog(
                    ~modalRef, 
                    ~text=`Delete "${state.selectedMacroModuleName}" module of macros?`, 
                )->Promise.thenResolve(confirmed => {
                    if (confirmed) {
                        updateSelectedMacroModule((st,moduleName) => deleteMacroModule(st, ~moduleName))
                    }
                })->ignore
            }
        }
    }

    let actAddNewMacroModule = () => {
        updateState(st => Ok(st->addNewMacroModule))
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
        <Row>
            <FormControl size=#small >
                <Select
                    value={state.selectedMacroModuleName}
                    onChange=evt2str(actSelectedMacroModuleNameChange)
                    sx={"width": 330}
                >
                    {
                        state.macroModules->Belt_MapString.toArray->Array.map(((modName,mod)) => {
                            let value = modName
                            let dispName = modName ++ (mod.isActive ? "" : " (inactive)")
                            <MenuItem key=value value>{React.string(dispName)}</MenuItem>
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
                            kbrdClbkMake(~key=keyEsc, ~act=onClose),
                        ])
                    )
                }
            }
        </Row>
    }

    let rndScriptTextField = (~modName:string, ~selectedMacroModule:macroModule) => {
        <TextField
            key={modName}
            size=#small
            style=ReactDOM.Style.make(~width=if(selectedMacroModuleIsReadOnly) {"350px"} else {"300px"}, ())
            label="Script"
            autoFocus=true
            multiline=true
            maxRows=3
            value=selectedMacroModule.scriptTextEdit
            onChange=evt2str(actSetScriptTextEdit)
            onKeyDown=kbrdHnds([
                kbrdClbkMake(~key=keyEnter, ~act=actSaveEdits),
                kbrdClbkMake(~key=keyEsc, ~act=onClose),
            ])
            disabled=selectedMacroModuleIsReadOnly
        />
    }

    let rndEditControls = () => {
        if (!isExpanded) {
            React.null
        } else {
            switch selectedMacroModule {
                | None => React.null
                | Some(selectedMacroModule) => {
                    <Col>
                        <TextField
                            size=#small
                            style=ReactDOM.Style.make(~width="350px", ())
                            label="Module name" 
                            value=selectedMacroModule.nameEdit
                            onChange=evt2str(actSetNameEdit)
                            onKeyDown=kbrdHnds([
                                kbrdClbkMake(~key=keyEnter, ~act=actSaveEdits),
                                kbrdClbkMake(~key=keyEsc, ~act=onClose),
                            ])
                            disabled={selectedMacroModuleIsReadOnly}
                        />
                        <FormControlLabel
                            control={
                                <Checkbox
                                    checked=selectedMacroModule.isActiveEdit
                                    onChange=evt2bool(actSetIsActiveEdit)
                                />
                            }
                            label="Active"
                        />
                        {
                            if (selectedMacroModuleIsReadOnly) {
                                {rndScriptTextField(~modName=state.selectedMacroModuleName, ~selectedMacroModule)}
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
                                                {
                                                    rndScriptTextField(
                                                        ~modName=state.selectedMacroModuleName, 
                                                        ~selectedMacroModule
                                                    )
                                                }
                                            </td>
                                        </tr>
                                    </tbody>
                                </table>
                            }
                        }
                        <Row>
                            <Button 
                                disabled={!thereAreChangesInSelectedMacroModule} 
                                onClick={_=>actSaveEdits()} variant=#contained 
                            >
                                {React.string("Save changes")}
                            </Button>
                            <Button 
                                onClick={_=>actAddNewMacroModule()} variant=#outlined 
                            >
                                {React.string("Add new")}
                            </Button>
                            <Button
                                disabled=selectedMacroModuleIsReadOnly 
                                onClick={_=>actDeleteSelectedMacroModule()} variant=#outlined 
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
        switch selectedMacroModule {
            | None => rndError(`Cannot find a module of macros with name "${state.selectedMacroModuleName}"`)
            | Some(selectedMacroModule) => {
                switch selectedMacroModule.scriptExecutionErr {
                    | Some(msg) => {
                        rndError(`There was an error during initialization of this module of macros:\n${msg}`)
                    }
                    | None => {
                        if (!selectedMacroModule.isActive) {
                            <Button onClick={_=>activateSelectedMacroModule()} variant=#contained >
                                {React.string("Activate")}
                            </Button>
                        } else if (selectedMacroModule.macros->Array.length == 0) {
                            React.string("This module has no registered macros.")
                        } else {
                            <ListCmp disablePadding=true key={state.selectedMacroModuleName}>
                                {
                                    selectedMacroModule.macros->Array.map(macroName => {
                                        <ListItem key=macroName disablePadding=true >
                                            <ListItemButton 
                                                onClick={_=>{
                                                    onClose()
                                                    MM_api_macros.runMacro(
                                                        ~moduleName=state.selectedMacroModuleName, 
                                                        ~macroName
                                                    )->ignore
                                                }}
                                            >
                                                <ListItemText>
                                                    {React.string(macroName)}
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