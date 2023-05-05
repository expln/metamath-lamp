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
    ~onChange:bool=>unit,
) => {
    <Row alignItems=#center>
        {"Edit statements by "->React.string}
        <FormControl size=#small >
            <InputLabel id="label-for-edit-statements-by">"Edit statements by"</InputLabel>
            <Select 
                sx={"width": 150}
                labelId="label-for-edit-statements-by"
                label="Edit statements by"
                value=boolToStr(editStmtsByLeftClick)
                onChange=evt2str(boolStr => boolStr->strToBool->onChange)
            >
                <MenuItem value="true"> {React.string("Left click")} </MenuItem>
                <MenuItem value="false"> {React.string("Alt + Left click")} </MenuItem>
            </Select>
        </FormControl>
        {"Select statements by "->React.string}
        <FormControl size=#small >
            <InputLabel id="label-for-select-statements-by">"Select statements by"</InputLabel>
            <Select 
                sx={"width": 150}
                labelId="label-for-select-statements-by"
                label="Select statements by"
                value=boolToStr(!editStmtsByLeftClick)
                onChange=evt2str(boolStr => (!(boolStr->strToBool))->onChange)
            >
                <MenuItem value="true"> {React.string("Left click")} </MenuItem>
                <MenuItem value="false"> {React.string("Alt + Left click")} </MenuItem>
            </Select>
        </FormControl>
    </Row>
}