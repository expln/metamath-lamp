open Expln_React_common
open Expln_React_Mui

type colors = Belt_MapString.t<string>

@react.component
let make = (
    ~types:array<string>, 
    ~colors:array<string>,
    ~availableColors: array<string>,
    ~onAdd: unit => unit, 
    ~onTypeChange: (int,string) => unit, 
    ~onColorChange: (int,string) => unit, 
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
    <Col>
        {
            React.array(types->Js_array2.mapi((typ,i) => {
                <Row key={i->Belt_Int.toString}>
                    <TextField label="Type" size=#small style=ReactDOM.Style.make(~width="100px", ()) value=typ onChange=evt2str(str => onTypeChange(i,str)) />
                    {rndColorSelect(~selectedColor=colors[i], ~onNewColorSelected = newColor => onColorChange(i, newColor))}
                </Row>
            }))
        }
        <IconButton key="add-button" onClick={_ => onAdd()}>
            <MM_Icons.Add/>
        </IconButton>
    </Col>
}