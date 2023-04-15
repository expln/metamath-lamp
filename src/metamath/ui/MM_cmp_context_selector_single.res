open Expln_React_common
open Expln_React_Mui
open MM_parser
open Expln_React_Modal
open Expln_utils_promise
open MM_react_common

type mmFileSourceType = Local | Web

type webSource = {
    alias:string,
    url:string,
}

type mmFileSource =
    | Local({fileName:string})
    | Web(webSource)

type readInstr = ReadAll | StopBefore | StopAfter

let readInstrToStr = ri => {
    switch ri {
        | ReadAll => "ReadAll"
        | StopBefore => "StopBefore"
        | StopAfter => "StopAfter"
    }
}

let readInstrFromStr = str => {
    switch str {
        | "ReadAll" => ReadAll
        | "StopBefore" => StopBefore
        | "StopAfter" => StopAfter
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to a readInstr.`}))
    }
}

let mmFileSourceTypeToStr = (src:mmFileSourceType):string => {
    switch src {
        | Local => "Local"
        | Web => "Web"
    }
}

let mmFileSourceTypeFromStr = (str:string):mmFileSourceType => {
    switch str {
        | "Local" => Local
        | "Web" => Web
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to an mmFileSourceType.`}))
    }
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~availableWebSrcs:array<webSource>,
    ~srcType:mmFileSourceType,
    ~onSrcTypeChange:mmFileSourceType=>unit,
    ~fileSrc: option<mmFileSource>,
    ~onFileChange:(mmFileSource,string)=>unit, 
    ~parseError:option<string>, 
    ~readInstr:readInstr,
    ~onReadInstrChange: readInstr => unit,
    ~label:option<string>,
    ~onLabelChange: option<string>=>unit,
    ~allLabels:array<string>,
    ~renderDeleteButton:bool,
    ~onDelete:unit=>unit, 
) => {

    let makeActTerminate = (modalId:modalId, isTerminated:ref<bool>):(unit=>unit) => {
        () => {
            isTerminated.contents = true
            closeModal(modalRef, modalId)
        }
    }

    let actAliasSelected = alias => {
        switch availableWebSrcs->Js_array2.find(src => src.alias == alias) {
            | None => raise(MmException({msg:`Cannot determine a URL for "${alias}" alias.`}))
            | Some(webSrc) => {
                let progressText = `Downloading MM file from "${alias}"`
                let isTerminated = ref(false)
                openModal(modalRef, () => rndProgress(~text=progressText, ~pct=0., ()))->promiseMap(modalId => {
                    updateModal( 
                        modalRef, modalId, 
                        () => rndProgress( ~text=progressText, ~pct=0., ~onTerminate=makeActTerminate(modalId, isTerminated), () )
                    )
                    FileLoader.loadFile(
                        ~url=webSrc.url,
                        ~onProgress = (loaded,total) => {
                            let pct = loaded->Belt_Int.toFloat /. total->Belt_Int.toFloat
                            updateModal( 
                                modalRef, modalId,
                                () => rndProgress( ~text=progressText, ~pct, ~onTerminate=makeActTerminate(modalId, isTerminated), () )
                            )
                        },
                        ~onError = () => {
                            closeModal(modalRef, modalId)
                            openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                                updateModal(modalRef, modalId, () => {
                                    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                                        <Col spacing=1.>
                                            {
                                                React.string(`An error occurred while downloading from "${alias}".`)
                                            }
                                            <Button onClick={_ => closeModal(modalRef, modalId) } variant=#contained> 
                                                {React.string("Ok")} 
                                            </Button>
                                        </Col>
                                    </Paper>
                                })
                            })->ignore
                        },
                        ~onReady = text => {
                            if (!isTerminated.contents) {
                                onFileChange(Web(webSrc), text)
                                closeModal(modalRef, modalId)
                            }
                        },
                        ()
                    )
                })->ignore
            }
        }
    }

    let rndDeleteButton = () => {
        if (renderDeleteButton) {
            <IconButton onClick={_ => onDelete()} >
                <Icons.Delete/>
            </IconButton>
        } else {
            React.null
        }
    }

    let rndSourceTypeSelector = () => {
        if (fileSrc->Belt.Option.isNone) {
            <FormControl size=#small>
                <InputLabel id="src-type-select-label">"Source type"</InputLabel>
                <Select
                    sx={"width": 130}
                    labelId="src-type-select-label"
                    value={srcType->mmFileSourceTypeToStr}
                    label="Source type"
                    onChange=evt2str(str => str->mmFileSourceTypeFromStr->onSrcTypeChange)
                >
                    <MenuItem value="Local">{React.string("Local")}</MenuItem>
                    <MenuItem value="Web">{React.string("Web")}</MenuItem>
                </Select>
            </FormControl>
        } else {
            <span>
                {React.string(srcType->mmFileSourceTypeToStr)}
            </span>
        }
    }

    let rndReadInstrTypeSelector = () => {
        <FormControl size=#small>
            <InputLabel id="scope-type-select-label">"Scope"</InputLabel>
            <Select
                labelId="scope-type-select-label"
                value={readInstr->readInstrToStr}
                label="Scope"
                onChange=evt2str(str => str->readInstrFromStr->onReadInstrChange)
            >
                <MenuItem value="ReadAll">{React.string("Read all")}</MenuItem>
                <MenuItem value="StopBefore">{React.string("Stop before")}</MenuItem>
                <MenuItem value="StopAfter">{React.string("Stop after")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndLabelSelector = () => {
        <AutocompleteVirtualized value=label options=allLabels size=#small onChange=onLabelChange />
    }

    let rndAliasSelector = (alias: option<string>) => {
        if (alias->Belt.Option.isNone) {
            <FormControl size=#small>
                <InputLabel id="alias-select-label">"Alias"</InputLabel>
                <Select
                    sx={"width": 200}
                    labelId="alias-select-label"
                    value={alias->Belt_Option.getWithDefault("")}
                    label="Alias"
                    onChange=evt2str(actAliasSelected)
                >
                    {
                        availableWebSrcs->Js_array2.mapi((webSrc,i) => {
                            <MenuItem value={webSrc.alias} key={i->Belt.Int.toString}>{React.string(webSrc.alias)}</MenuItem>
                        })->React.array
                    }
                </Select>
            </FormControl>
        } else {
            <span>
                {React.string(alias->Belt_Option.getWithDefault("<web-src-alias>"))}
            </span>
        }
    }

    let rndFileSelector = (fileName: option<string>) => {
        if (fileName->Belt.Option.isNone) {
            <Expln_React_TextFileReader 
                onChange={(selected:option<(string,string)>) => {
                    switch selected {
                        | None => ()
                        | Some((fileName, fileText)) => onFileChange(Local({fileName:fileName}),fileText)
                    }
                }} 
            />
        } else {
            <span>
                {React.string(fileName->Belt_Option.getWithDefault("<fileName>"))}
            </span>
        }
    }

    let getFileNameFromFileSrc = (fileSrc: option<mmFileSource>):option<string> => {
        switch fileSrc {
            | Some(Local({fileName})) => Some(fileName)
            | _ => None
        }
    }

    let getAliasFromFileSrc = (fileSrc: option<mmFileSource>):option<string> => {
        switch fileSrc {
            | Some(Web({alias})) => Some(alias)
            | _ => None
        }
    }

    let rndSourceSelector = () => {
        switch srcType {
            | Local => rndFileSelector(getFileNameFromFileSrc(fileSrc))
            | Web => rndAliasSelector(getAliasFromFileSrc(fileSrc))
        }
    }

    let rndReadInstr = () => {
        switch parseError {
            | Some(msg) => {
                <pre style=ReactDOM.Style.make(~color="red", ())>
                    {React.string("Error: " ++ msg)}
                </pre>
            }
            | None => {
                switch fileSrc {
                    | None => React.null
                    | Some(_) => {
                        <Row>
                            {rndReadInstrTypeSelector()}
                            {
                                switch readInstr {
                                    | StopBefore | StopAfter => rndLabelSelector()
                                    | ReadAll => React.null
                                }
                            }
                        </Row>
                    }
                }
            }
        }
    }

    <Row alignItems=#center spacing=1. >
        {rndDeleteButton()}
        {rndSourceTypeSelector()}
        {rndSourceSelector()}
        {rndReadInstr()}
    </Row>
}