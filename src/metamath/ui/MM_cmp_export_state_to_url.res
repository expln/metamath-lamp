open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Local_storage_utils

@val external window: {..} = "window"

@react.component
let make = (
    ~editoStateBase64:string,
    ~onClose:unit=>unit
) => {

    let location = window["location"]
    let origin = location["origin"]
    let pathname = location["pathname"]->Js.String2.replaceByRe(%re("/\/v\d+\//g"), "/latest/")
    let url = origin ++ pathname ++ "?editorState=" 
        ++ (editoStateBase64->Js.String2.replaceByRe(%re("/\+/g"), "-")->Js.String2.replaceByRe(%re("/\//g"), "_"))

    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col>
            <Row spacing=1.>
                <Button onClick={_=>copyToClipboard(url)} variant=#contained > {React.string("Copy")} </Button>
                <Button onClick={_=>onClose()} > {React.string("Close")} </Button>
            </Row>
            <pre style=ReactDOM.Style.make(~overflow="auto", ())>{React.string(url)}</pre>
        </Col>
    </Paper>
}