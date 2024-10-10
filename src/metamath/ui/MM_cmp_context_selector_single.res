open Expln_React_common
open Expln_React_Mui
open MM_parser
open Expln_React_Modal
open MM_wrk_editor

@react.component
let make = (
    ~modalRef:modalRef,
    ~availableWebSrcs:array<webSource>,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:string=>unit,
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

    let actAliasSelected = alias => {
        switch availableWebSrcs->Array.find(src => src.alias == alias) {
            | None => raise(MmException({msg:`Cannot determine a URL for "${alias}" alias.`}))
            | Some(webSrc) => {
                FileLoader.loadFileWithProgress(
                    ~modalRef,
                    ~showWarning=!(trustedUrls->Array.includes(webSrc.url)),
                    ~progressText=`Downloading MM file from "${alias}"`,
                    ~transformErrorMsg= msg => `An error occurred while downloading from "${alias}":` 
                                                    ++ ` ${msg->Belt.Option.getWithDefault("")}.`,
                    ~url=webSrc.url,
                    ~onUrlBecomesTrusted,
                    ~onReady = text => onFileChange(Web(webSrc), text),
                    ()
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
        <AutocompleteVirtualized value=label options=allLabels size=#small onChange=onLabelChange label="Label" />
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