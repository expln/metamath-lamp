open Expln_React_common
open MM_react_common
open Expln_React_Mui
open Raw_js_utils
open Local_storage_utils
open Expln_React_Modal
open Expln_utils_promise

type macroModule = {
    name:string,
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
    macroModules:array<macroModule>,
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
                () => executeFunctionBody(script)
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
    let validKeys = st.macroModules
        ->Array.map(mod => getScriptCacheKey(~macroModuleName=mod.name, ~script=mod.scriptText))
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
    ("set.mm example macros", textToScript(MM_macros_set_mm_example.setMmExampleMacros))
])

let isPredefinedMacroModule = (macroModuleName:string):bool => {
    predefinedMacroModuleScripts->Belt_HashMapString.has(macroModuleName)
}

let stateToStateLocStor = (st:state):stateLocStor => {
    {
        selectedMacroModuleName: st.selectedMacroModuleName,
        macroModules: st.macroModules->Array.map(mod => {
            {
                name: mod.name,
                scriptText: isPredefinedMacroModule(mod.name) ? "" : mod.scriptText,
                isActive: mod.isActive,
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
    //read all modules from the local storage
    let ls: stateLocStor = switch locStorReadString(macrosLocStorKey) {
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
    let modulesInLocStor = ls.macroModules->Array.map(mod => mod.name)->Belt_HashSetString.fromArray

    //add modules registered by the user via the console
    MM_api_macros.listRegisteredMacroModules()->Array.forEach(modName => {
        if (!(modulesInLocStor->Belt_HashSetString.has(modName))) {
            ls.macroModules->Array.push({ name: modName, isActive: true, scriptText: "", })
        }
    })

    //add default modules
    let updated = []
    let maxI = ls.macroModules->Array.length-1
    predefinedMacroModuleScripts->Belt_HashMapString.forEach((preModName,script) => {
        for i in 0 to maxI {
            let mod = ls.macroModules->Array.getUnsafe(i)
            if (mod.name == preModName) {
                ls.macroModules->Array.splice(~start=i, ~remove=1, ~insert=[ {...mod, scriptText:script} ])
                updated->Array.push(preModName)
            }
        }
    })
    predefinedMacroModuleScripts->Belt_HashMapString.forEach((preModName,script) => {
        if (!(updated->Array.includes(preModName))) {
            ls.macroModules->Array.push({ name: preModName, isActive: false, scriptText: script, })
        }
    })

    //run all scripts
    let macroModules = ls.macroModules
        ->Array.filter(mod => mod.name->String.length > 0)
        ->Array.map(({name, isActive, scriptText}) => {
            {
                name,
                nameEdit:name,
                isActive,
                isActiveEdit:isActive,
                scriptText,
                scriptTextEdit: scriptText,
                scriptExecutionErr:
                    if (isActive && scriptText->String.trim->String.length > 0) {
                        switch runScript(~macroModuleName=name, ~script=scriptText) {
                            | Error(msg) => Some(msg)
                            | Ok(_) => None
                        }
                    } else {
                        None
                    },
                macros: MM_api_macros.listRegisteredMacrosInModule(name)->Option.getOr([])
            }
        })
    let allFoundMacroModules = macroModules->Array.map(mod => mod.name)
    {
        selectedMacroModuleName: allFoundMacroModules->Array.find(n => n == ls.selectedMacroModuleName)
            ->Option.getOr(allFoundMacroModules[0]->Option.getExn(~message="MM_cmp_macros.readStateFromLocStor.1")),
        macroModules,
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
        macroModules:Belt_Array.concatMany([
            [{
                name:"",
                nameEdit:"New macros",
                isActive:true,
                isActiveEdit:true,
                scriptText:"",
                scriptTextEdit:"",
                scriptExecutionErr:None,
                macros:[],
            }],
            st.macroModules, 
        ])
    }->reloadState
}

let deleteMacroModule = (st:state, ~moduleName:string):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot delete a predefined macro module.")
    } else {
        MM_api_macros.unregisterMacroModule(moduleName)
        Ok(
            {
                ...st,
                macroModules:st.macroModules->Array.filter(mod => mod.name != moduleName)
            }->reloadState
        )
    }
}

let saveEdits = (st:state, ~moduleName:string):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        switch st.macroModules->Array.find(mod => mod.name == moduleName) {
            | None => Ok(st)
            | Some(mod) => {
                if (mod.name != mod.nameEdit && st.macroModules->Array.some(m => m.name == mod.nameEdit)) {
                    Error(`A module with name "${mod.nameEdit}" already exists. Please choose another name.`)
                } else {
                    Ok(
                        {
                            selectedMacroModuleName:
                                mod.name == st.selectedMacroModuleName ? mod.nameEdit : st.selectedMacroModuleName,
                            macroModules: st.macroModules->Array.map(mod => {
                                if (mod.name != moduleName) {
                                    mod
                                } else {
                                    {
                                        ...mod,
                                        name:mod.nameEdit,
                                        isActive:mod.isActiveEdit,
                                        scriptText:mod.scriptTextEdit,
                                        scriptExecutionErr:None,
                                        macros:[],
                                    }
                                }
                            })
                        }->reloadState
                    )
                }
            }
        }
    }
}

let setNameEdit = (st:state, ~moduleName:string, ~nameEdit:string):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        Ok(
            {
                ...st,
                macroModules:st.macroModules->Array.map(mod => {
                    if (mod.name == moduleName) {
                        { ...mod, nameEdit:nameEdit }
                    } else {
                        mod
                    }
                })
            }
        )
    }
}

let setScriptTextEdit = (st:state, ~moduleName:string, ~scriptTextEdit:string):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        Ok(
            {
                ...st,
                macroModules:st.macroModules->Array.map(mod => {
                    if (mod.name == moduleName) {
                        { ...mod, scriptTextEdit:scriptTextEdit }
                    } else {
                        mod
                    }
                })
            }
        )
    }
}

let setIsActiveEdit = (st:state, ~moduleName:string, ~isActiveEdit:bool):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        Ok(
            {
                ...st,
                macroModules:st.macroModules->Array.map(mod => {
                    if (mod.name == moduleName) {
                        { ...mod, isActiveEdit:isActiveEdit }
                    } else {
                        mod
                    }
                })
            }
        )
    }
}

let resetEditsForMacroModule = (st:state, ~moduleName:string):result<state,string> => {
    if (predefinedMacroModuleScripts->Belt_HashMapString.has(moduleName)) {
        Error("Cannot update a predefined macro module.")
    } else {
        Ok(
            {
                ...st,
                macroModules:st.macroModules->Array.map(mod => {
                    if (mod.name == moduleName) {
                        { 
                            ...mod, 
                            nameEdit:mod.name, 
                            isActiveEdit:mod.isActive, 
                            scriptTextEdit:mod.scriptText,
                        }
                    } else {
                        mod
                    }
                })
            }
        )
    }
}

let setSelectedMacroModuleName = (st:state, newSelectedMacroModuleName:string):result<state,string> => {
    switch st.macroModules->Array.find(mod => mod.name == newSelectedMacroModuleName) {
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

    let selectedMacroModule = state.macroModules
        ->Array.find(mod => mod.name == state.selectedMacroModuleName)

    let thereAreChangesInSelectedMacroModule = selectedMacroModule->Belt.Option.map(mod => {
        mod.name != mod.nameEdit 
            || mod.isActive != mod.isActiveEdit 
            || mod.scriptText != mod.scriptTextEdit
    })->Belt_Option.getWithDefault(false)

    let selectedMacroModuleIsReadOnly = selectedMacroModule->Belt.Option.map(mod => {
        predefinedMacroModuleScripts->Belt_HashMapString.has(mod.name)
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
        if (!selectedMacroModuleIsReadOnly && !hideWarning && isExpanded) {
            actShowWarning()
        }
        None
    }, [selectedMacroModuleIsReadOnly])

    let updateState = (fn:state=>result<state,string>):unit => {
        setState(st => {
            switch fn(st) {
                | Error(_) => st
                | Ok(st) => st
            }
        })
    }

    let updateSelectedMacroModule = (fn:(state,string)=>result<state,string>):unit => {
        switch selectedMacroModule {
            | None => ()
            | Some({name}) => {
                updateState(fn(_, name))
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

    let actDeleteSelectedMacroModule = () => {
        switch selectedMacroModule {
            | None => ()
            | Some({name}) => {
                openOkCancelDialog(
                    ~modalRef, 
                    ~text=`Delete "${name}" module of macros?`, 
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
                        state.macroModules->Array.map(mod => {
                            let value = mod.name
                            let dispName = mod.name ++ (mod.isActive ? "" : " (inactive)")
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

    let rndScriptTextField = (selectedMacroModule:macroModule) => {
        <TextField
            key={selectedMacroModule.name}
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
                        {
                            if (selectedMacroModuleIsReadOnly) {
                                {rndScriptTextField(selectedMacroModule)}
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
                                                {rndScriptTextField(selectedMacroModule)}
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
                        <ListCmp disablePadding=true key={selectedMacroModule.name}>
                            {
                                selectedMacroModule.macros->Array.map(macroName => {
                                    <ListItem key=macroName disablePadding=true >
                                        <ListItemButton 
                                            onClick={_=>{
                                                onClose()
                                                MM_api_macros.runMacro(
                                                    ~moduleName=selectedMacroModule.name, 
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

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col>
            {rndMacrosDropdown()}
            {rndEditControls()}
            {rndActiveMacros()}
            {rndCancelBtn()}
        </Col>
    </Paper>
}