open Expln_React_common
open Expln_React_Mui

let strToBool = (str:string):bool => {
    switch str {
        | "true" => true
        | "false" => false
        | _ => true
    }
}

let boolToStr = (b:bool):string => Expln_utils_common.stringify(b)

@react.component
let make = (
    ~editStmtsByLeftClick:bool, 
    ~longClickEnabled:bool,
    ~onChange:bool=>unit,
) => {
    let leftClickDescrStr = if (longClickEnabled) {"Short click (Left click)"} else {"Left click"}
    let altLeftClickDescrStr = if (longClickEnabled) {"Long click (Alt + Left click)"} else {"Alt + Left click"}
    <Row alignItems=#center>
        <FormControl size=#small >
            <InputLabel id="label-for-edit-statements-by">"Edit statements by"</InputLabel>
            <Select 
                sx={"width": 250}
                labelId="label-for-edit-statements-by"
                label="Edit statements by"
                value=boolToStr(editStmtsByLeftClick)
                onChange=evt2str(boolStr => boolStr->strToBool->onChange)
            >
                <MenuItem value="true"> {React.string(leftClickDescrStr)} </MenuItem>
                <MenuItem value="false"> {React.string(altLeftClickDescrStr)} </MenuItem>
            </Select>
        </FormControl>
        <FormControl size=#small >
            <InputLabel id="label-for-select-statements-by">"Select statements by"</InputLabel>
            <Select 
                sx={"width": 250}
                labelId="label-for-select-statements-by"
                label="Select statements by"
                value=boolToStr(!editStmtsByLeftClick)
                onChange=evt2str(boolStr => (!(boolStr->strToBool))->onChange)
            >
                <MenuItem value="true"> {React.string(leftClickDescrStr)} </MenuItem>
                <MenuItem value="false"> {React.string(altLeftClickDescrStr)} </MenuItem>
            </Select>
        </FormControl>
    </Row>
}