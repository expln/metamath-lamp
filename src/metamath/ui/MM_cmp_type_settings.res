open Expln_React_common
open MM_react_common
open Expln_React_Mui

type typeSettingsState = {
    id: string,
    typ: string,
    color: string,
    prefix: string,
    err: option<string>,
}

@react.component
let make = (
    ~typeSettings:array<typeSettingsState>, 
    ~availableColors: array<string>,
    ~onAdd: unit => unit, 
    ~onTypeChange: (string,string) => unit, 
    ~onColorChange: (string,string) => unit, 
    ~onPrefixChange: (string,string) => unit,
    ~onDelete: string => unit,
    ~onRestoreDefaults: unit=>unit,
) => {
    let rndTypeSetting = ts => {
        <Col key=ts.id>
            <Row>
                <TextField label="Type" size=#small style=ReactDOM.Style.make(~width="100px", ()) 
                    value=ts.typ onChange=evt2str(onTypeChange(ts.id,_)) />
                {
                    rndColorSelect(
                        ~availableColors, ~selectedColor=ts.color, ~onNewColorSelected = onColorChange(ts.id, _),
                        ~label="Color", ()
                    )
                }
                <TextField label="Prefix" size=#small style=ReactDOM.Style.make(~width="100px", ()) 
                    value=ts.prefix onChange=evt2str(onPrefixChange(ts.id,_)) />
                <IconButton key="delete-button" onClick={_ => onDelete(ts.id)}>
                    <MM_Icons.Delete/>
                </IconButton>
            </Row>
            {
                switch ts.err {
                    | None => React.null
                    | Some(msg) => <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
                }
            }
        </Col>
    }

    <Col style=ReactDOM.Style.make(~marginTop="5px", ())>
        { typeSettings->Js_array2.map(rndTypeSetting)->React.array }
        <Row alignItems={#baseline} >
            <IconButton key="add-button" onClick={_ => onAdd()}>
                <MM_Icons.Add/>
            </IconButton>
            {
                rndSmallTextBtn(
                    ~text="Restore default type settings",
                    ~onClick=onRestoreDefaults,
                    ()
                )
            }
        </Row>
    </Col>
}