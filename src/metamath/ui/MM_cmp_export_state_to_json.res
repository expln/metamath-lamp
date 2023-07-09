open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Local_storage_utils
open Common

@react.component
let make = (
    ~jsonStr:string, 
    ~onClose:unit=>unit,
    ~tempMode:bool,
) => {
    let (appendTimestamp, setAppendTimestamp) = useStateFromLocalStorageBool(
        ~key="export-to-json-append-timestamp", ~default=false, ~tempMode
    )
    let (descr, setDescr) = React.useState(() => "")
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    let actDescrChanged = (newDescr:string) => {
        setDescr(_ => newDescr)
    }

    let timestampStr = if (appendTimestamp) {
        Common.currTimeStr() ++ " "
    } else {
        ""
    }

    let descrStr = if (descr->Js.String2.length > 0) {
        descr ++ " "
    } else {
        ""
    }

    let textToShow = timestampStr ++ descrStr ++ jsonStr

    let actCopyToClipboard = () => {
        copyToClipboard(textToShow)
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
                label="Description" 
                value=descr
                onChange=evt2str(actDescrChanged)
                title="Press Enter to copy to the clipboard and close this dialog window."
                onKeyDown=kbrdHnd2(
                    kbrdClbkMake(~keyCode=keyCodeEnter, ~act=actCopyAndClose, ()),
                    kbrdClbkMake(~keyCode=keyCodeEsc, ~act=onClose, ())
                )
                autoFocus=true
            />
            <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(textToShow)}</pre>
        </Col>
    </Paper>
}