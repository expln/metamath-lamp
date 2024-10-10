open Expln_React_Mui
open Expln_utils_promise
open MM_react_common

@val external window: {..} = "window"

@react.component
let make = (
    ~editorStateBase64:string,
    ~onClose:unit=>unit
) => {
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => None)

    let location = window["location"]
    let origin = location["origin"]
    let pathname = location["pathname"]->String.replaceRegExp(%re("/\/v\d+\//g"), "/latest/")
    let url = origin ++ pathname ++ "?editorState=" ++ editorStateBase64

    let actCopyToClipboard = () => {
        copyToClipboard(url)->promiseMap(_ => {
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

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            <Row alignItems=#center>
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
            <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(url)}</pre>
        </Col>
    </Paper>
}