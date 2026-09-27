open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open Common

@react.component
let make = (
    ~modalRef:modalRef,
    ~availableWebSrcs:array<webSource>,
    ~trustedUrls: React.ref<array<string>>,
    ~markUrlAsTrusted: React.ref<string=>unit>,
    ~srcType:mmFileSourceType,
    ~onSrcTypeChange:mmFileSourceType=>unit,
    ~fileSrc: option<mmFileSource>,
    ~onFileChange:(mmFileSource,string,Belt_HashMapString.t<string>)=>unit,
    ~parseError:option<string>, 
    ~readInstr:readInstr,
    ~onReadInstrChange: readInstr => unit,
    ~label:option<string>,
    ~onLabelChange: option<string>=>unit,
    ~allLabels:array<string>,
    ~renderDeleteButton:bool,
    ~onDelete:unit=>unit, 
) => {

    let labelSelectorTextFieldRef = React.useRef(Nullable.null)
    
    let actFocusLabelSelector = () => {
        switch labelSelectorTextFieldRef.current->Nullable.toOption {
            | None => ()
            | Some(domElem) => {
                let input = ReactDOM.domElementToObj(domElem)
                input["focus"]()
            }
        }
    }

    React.useEffect1(() => {
        actFocusLabelSelector()
        None
    }, [readInstr])

    let actAliasSelected = alias => {
        switch availableWebSrcs->Array.find(src => src.alias == alias) {
            | None => raise(MmException({msg:`Cannot determine a URL for "${alias}" alias.`}))
            | Some(webSrc) => {
                FileLoader.loadFileWithProgress(
                    ~modalRef,
                    ~showWarning=!(isTrustedUrl(trustedUrls.current, webSrc.url)),
                    ~progressText=`Downloading MM file from "${alias}"`,
                    ~transformErrorMsg= msg => `An error occurred while downloading from "${alias}":` 
                                                    ++ ` ${msg->Belt.Option.getWithDefault("")}.`,
                    ~url=webSrc.url,
                    ~markUrlAsTrusted,
                    ~onReady = text => onFileChange(Web(webSrc), text, Belt_HashMapString.make(~hintSize=0))
                )
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
        <AutocompleteVirtualized
            inputRef=ReactDOM.Ref.domRef(labelSelectorTextFieldRef)
            value=label options=allLabels size=#small onChange=onLabelChange label="Label" 
        />
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
                        availableWebSrcs->Array.mapWithIndex((webSrc,i) => {
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

    let rndRootFileSelector = (
        ~fileNames:array<string>, 
        ~onSelected:string=>unit,
        ~onCancel:unit=>unit
    ) => {
        let selectedFile:ref<string> = ref(fileNames->Array.getUnsafe(0))
        let rec updateDialogContent = (modalRef:modalRef, modalId:modalId):unit => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        <Row>
                            <Button 
                                variant=#contained
                                onClick={_ => {
                                    closeModal(modalRef, modalId)
                                    onSelected(selectedFile.contents)
                                }} 
                            > 
                                {React.string("Ok")} 
                            </Button>
                            <Button 
                                variant=#outlined
                                onClick={_ => {
                                    closeModal(modalRef, modalId)
                                    onCancel()
                                }} 
                            > 
                                {React.string("Cancel")} 
                            </Button>
                        </Row>
                        <span style=ReactDOM.Style.make(~fontWeight="bolder", ())>
                            { React.string("Select file to load") }
                        </span>
                        <RadioGroup 
                            row=false 
                            value={selectedFile.contents}
                            onChange=evt2str(newValue => {
                                selectedFile := newValue
                                updateDialogContent(modalRef, modalId)
                            })
                        >
                            {
                                fileNames->Array.map(fileName => {
                                    <FormControlLabel 
                                        key=fileName value=fileName label=fileName control={ <Radio/> } 
                                        style=ReactDOM.Style.make(~marginRight="30px", ())
                                    />
                                })->React.array
                            }
                        </RadioGroup>
                        <Row>
                            <Button 
                                variant=#contained
                                onClick={_ => {
                                    closeModal(modalRef, modalId)
                                    onSelected(selectedFile.contents)
                                }} 
                            > 
                                {React.string("Ok")} 
                            </Button>
                            <Button 
                                variant=#outlined
                                onClick={_ => {
                                    closeModal(modalRef, modalId)
                                    onCancel()
                                }} 
                            > 
                                {React.string("Cancel")} 
                            </Button>
                        </Row>
                    </Col>
                </Paper>
            })
        }
        openModal(modalRef, _ => React.null)
            ->Promise.thenResolve(modalId => updateDialogContent(modalRef, modalId))
            ->ignore
    }

    let rndFileSelector = (fileName: option<string>) => {
        if (fileName->Belt.Option.isNone) {
            <Expln_React_TextFileReader 
                onChange={(selected:option<array<(string,string)>>) => {
                    switch selected {
                        | None => ()
                        | Some(files) => {
                            if (files->Array.length > 1) {
                                let fileNameToText = Belt_HashMapString.fromArray(files)
                                rndRootFileSelector(
                                    ~fileNames=files->Array.map(((fileName,_)) => fileName),
                                    ~onSelected = fileName => {
                                        onFileChange(
                                            Local({fileName:fileName}),
                                            fileNameToText->Belt_HashMapString.get(fileName)
                                                ->Option.getExn(
                                                    ~message=`Internal error: cannot get file by name ${fileName}`
                                                ),
                                            fileNameToText
                                        )
                                    },
                                    ~onCancel=() => ()
                                )
                            } else if (files->Array.length == 1) {
                                let (fileName, fileContent) = files->Array.getUnsafe(0)
                                onFileChange(
                                    Local({fileName:fileName}),
                                    fileContent,
                                    Belt_HashMapString.make(~hintSize=0)
                                )
                            }
                        }
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