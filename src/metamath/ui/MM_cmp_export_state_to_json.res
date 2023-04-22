open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Local_storage_utils

let appendTimestampLocStorKey = "export-to-json-append-timestamp"

@react.component
let make = (
    ~jsonStr:string, 
    ~onClose:unit=>unit
) => {
    let (appendTimestamp, setAppendTimestampPriv) = React.useState(() => {
        locStorReadBool(appendTimestampLocStorKey)->Belt_Option.getWithDefault(false)
    })
    let setAppendTimestamp = (appendTimestamp:bool):unit => {
        locStorWriteBool(appendTimestampLocStorKey, appendTimestamp)
        setAppendTimestampPriv(_ => appendTimestamp)
    }

    let timestampStr = if (appendTimestamp) {
        Common.currTimeStr() ++ " "
    } else {
        ""
    }
    let textToShow = timestampStr ++ jsonStr


    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            <Row spacing=1.>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=appendTimestamp
                            onChange={evt2bool(setAppendTimestamp)}
                        />
                    }
                    label="append timestamp"
                />
                <Button onClick={_=>copyToClipboard(textToShow)} variant=#contained > {React.string("Copy")} </Button>
                <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            </Row>
            <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(textToShow)}</pre>
        </Col>
    </Paper>
}