open MM_react_common
open MM_api
open Expln_React_Modal

@react.component
let make = React.memo((
    ~modalRef:modalRef, 
) => {
    let actShowInfoMsg = (msg:string) => {
        openInfoDialog( ~modalRef, ~content=<pre>{msg->React.string}</pre> )
    }

    let actShowErrMsg = (msg:string) => {
        openInfoDialog( ~modalRef, ~content=<pre>{msg->React.string}</pre>, 
            ~icon=
                <span style=ReactDOM.Style.make(~color="red", () ) >
                    <MM_Icons.PriorityHigh/>
                </span>
        )
    }

    React.useEffect0(() => {
        setUiApi(
            ~showInfoMsg = makeApiFunc("showInfoMsg", params => {
                switch JSON.Decode.string(params) {
                    | None => Promise.resolve(Error("The parameter of showInfoMsg() must be a string."))
                    | Some(msg) => {
                        actShowInfoMsg(msg)
                        Promise.resolve(Ok(JSON.Encode.null))
                    }
                }
            }),
            ~showErrMsg = makeApiFunc("showErrMsg", params => {
                switch JSON.Decode.string(params) {
                    | None => Promise.resolve(Error("The parameter of showErrMsg() must be a string."))
                    | Some(msg) => {
                        actShowErrMsg(msg)
                        Promise.resolve(Ok(JSON.Encode.null))
                    }
                }
            })
        )
        None
    })

    React.null
})