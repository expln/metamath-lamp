open Expln_React_common
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
    let rndColorSelect = (~selectedColor, ~onNewColorSelected) => {
        <FormControl size=#small >
            <InputLabel id="label-for-color-select">"Color"</InputLabel>
            <Select 
                labelId="label-for-color-select"
                label="Color"
                value=selectedColor
                onChange=evt2str(onNewColorSelected)
            >
                {
                    React.array(availableColors->Js_array2.map(color => {
                        <MenuItem key=color value=color>
                            <div style=ReactDOM.Style.make(~width="50px", ~height="20px", ~backgroundColor=color, ()) />
                        </MenuItem>
                    }))
                }
            </Select>
        </FormControl>
    }

    let rndTypeSetting = ts => {
        <Col key=ts.id>
            <Row>
                <TextField label="Type" size=#small style=ReactDOM.Style.make(~width="100px", ()) 
                    value=ts.typ onChange=evt2str(onTypeChange(ts.id,_)) />
                {rndColorSelect(~selectedColor=ts.color, ~onNewColorSelected = onColorChange(ts.id, _))}
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
            <span
                onClick={_=> onRestoreDefaults() }
                style=ReactDOM.Style.make(~cursor="pointer", ~color="grey", ~fontSize="0.7em", ())
            >
                {React.string("Restore default type settings")}
            </span>
        </Row>
    </Col>
}