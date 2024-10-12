open Expln_React_common
open Expln_React_Mui
open Expln_utils_promise
open MM_react_common
open Local_storage_utils

@react.component
let make = (
    ~jsonStr:string, 
    ~onClose:unit=>unit,
) => {
    let (appendTimestamp, setAppendTimestamp) = useStateFromLocalStorageBool(
        ~key="export-to-json-append-timestamp", ~default=false
    )
    let (notes, setNotes) = React.useState(() => "")
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    let actNotesChanged = (newNotes:string) => {
        setNotes(_ => newNotes)
    }

    let timestampStr = if (appendTimestamp) {
        Common.currTimeStr() ++ " "
    } else {
        ""
    }

    let notesStr = if (notes->String.length > 0) {
        notes ++ " "
    } else {
        ""
    }

    let textToShow = [timestampStr, notesStr, jsonStr]
        ->Array.filter(str => str->String.trim != "")
        ->Array.joinUnsafe("\n")

    let actCopyToClipboard = () => {
        copyToClipboard(textToShow)->promiseMap(_ => {
            setCopiedToClipboard(timerId => {
                switch timerId {
                    | None => ()
                    | Some(timerId) => clearTimeout(timerId)
                }
                Some(setTimeout(
                    () => setCopiedToClipboard(_ => None),
                    1000
                ))
            })
        })->ignore
    }

    let actCopyAndClose = () => {
        actCopyToClipboard()
        onClose()
    }

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            <Row alignItems=#center>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=appendTimestamp
                            onChange={evt2bool(b => setAppendTimestamp(_ => b))}
                        />
                    }
                    label="append timestamp"
                />
                <Button onClick={_=>actCopyToClipboard()} variant=#contained style=ReactDOM.Style.make(~width="90px", ()) > 
                    {
                        if (copiedToClipboard->Belt.Option.isSome) {
                            React.string("Copied")
                        } else {
                            React.string("Copy")
                        }
                    } 
                </Button>
                <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            </Row>
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="350px", ())
                label="Notes" 
                value=notes
                onChange=evt2str(actNotesChanged)
                title="Press Enter to copy to the clipboard and close this dialog window."
                onKeyDown=kbrdHnd2(
                    kbrdClbkMake(~key=keyEnter, ~act=actCopyAndClose),
                    kbrdClbkMake(~key=keyEsc, ~act=onClose)
                )
                autoFocus=true
            />
            <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(textToShow)}</pre>
        </Col>
    </Paper>
}