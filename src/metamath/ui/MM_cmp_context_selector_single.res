open Expln_React_common
open Expln_React_Mui

type mmFileSourceType = Local | Web

type mmFileSource =
    | Local({fileName:string})
    | Web({url:string, alias:string})

type readInstr = All | StopBefore | StopAfter

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

let mmFileSourceTypeToStr = src => {
    switch src {
        | Local => "Local"
        | Web => "Web"
    }
}

let mmFileSourceTypeFromStr = str => {
    switch str {
        | "Local" => Local
        | "Web" => Web
        | _ => raise(MmException({msg:`Cannot convert string '${str}' to an mmFileSourceType.`}))
    }
}

@react.component
let make = (
    ~srcType:mmFileSourceType,
    ~fileName: option<string>,
    ~onFileChange:option<(string,string)>=>unit, 
    ~parseError:option<string>, 
    ~readInstr:readInstr,
    ~onReadInstrChange: readInstr => unit,
    ~label:option<string>,
    ~onLabelChange: option<string>=>unit,
    ~allLabels:array<string>,
    ~renderDeleteButton:bool,
    ~onDelete:unit=>unit, 
) => {

    let rndDeleteButton = () => {
        if (renderDeleteButton) {
            <IconButton onClick={_ => onDelete()} >
                <Icons.Delete/>
            </IconButton>
        } else {
            React.null
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

    let rndFileSelector = () => {
        if (fileName->Belt.Option.isNone || parseError->Belt_Option.isNone) {
            <Expln_React_TextFileReader onChange=onFileChange />
        } else {
            <span>
                {React.string(fileName->Belt_Option.getWithDefault("<fileName>"))}
            </span>
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
                switch fileName {
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
        {rndFileSelector()}
        {rndReadInstr()}
    </Row>
}