open Expln_React_common
open Expln_React_Mui

type readInstr = [ #all | #stopBefore | #stopAfter]

@react.component
let make = (
    ~onFileChange:option<(string,string)>=>unit, 
    ~fileName: option<string>,
    ~parseError:option<string>, 
    ~readInstr:readInstr,
    ~onReadInstrChange: string => unit,
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
                value={readInstr :> string}
                label="Scope"
                onChange=evt2str(onReadInstrChange)
            >
                <MenuItem value="all">{React.string("Read all")}</MenuItem>
                <MenuItem value="stopBefore">{React.string("Stop before")}</MenuItem>
                <MenuItem value="stopAfter">{React.string("Stop after")}</MenuItem>
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
                                    | #stopBefore | #stopAfter => rndLabelSelector()
                                    | _ => React.null
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