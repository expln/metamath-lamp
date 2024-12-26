open MM_react_common
open MM_api
open Expln_React_Modal
open Raw_js_utils

let apiShowInfoMsg = (modalRef:modalRef, params:apiInput):promise<result<unit,string>> => {
    let params = params->apiInputToObjExn("apiShowInfoMsg: got empty parameters.")
    let msg = reqStrExn(params["msg"], "'msg' must be a string.")
    openInfoDialog( ~modalRef, ~content=<pre>{msg->React.string}</pre> )
    Promise.resolve(Ok(()))
}

let apiShowErrMsg = (modalRef:modalRef, params:apiInput):promise<result<unit,string>> => {
    let params = params->apiInputToObjExn("apiShowErrMsg: got empty parameters.")
    let msg = reqStrExn(params["msg"], "'msg' must be a string.")
    openInfoDialog( ~modalRef, ~content=<pre>{msg->React.string}</pre>, 
        ~icon=
            <span style=ReactDOM.Style.make(~color="red", () ) >
                <MM_Icons.PriorityHigh/>
            </span>
    )
    Promise.resolve(Ok(()))
}

let apiMultilineTextInput = (modalRef:modalRef, params:apiInput):promise<result<JSON.t,string>> => {
    let params = params->apiInputToObjExn("apiMultilineTextInput: got empty parameters.")
    let prompt:option<string> = optStrExn(params["prompt"], "'prompt' must be a string.")
    let initText:option<string> = optStrExn(params["initText"], "'initText' must be a string.")
    Promise.make((resolve,_) => {
        openModalPaneWithTitle(
            ~modalRef, 
            ~title=?prompt,
            ~content=(~close:unit=>unit) => {
                <MM_cmp_multiline_text_input
                    ?initText
                    onOk={text => {
                        close()
                        resolve(
                            Ok(Dict.fromArray([
                                ("text", text->JSON.Encode.string),
                                ("okClicked", true->JSON.Encode.bool),
                            ])->JSON.Encode.object)
                        )
                    }}
                    onCancel={() => {
                        close()
                        resolve(
                            Ok(Dict.fromArray([
                                ("okClicked", false->JSON.Encode.bool),
                            ])->JSON.Encode.object)
                        )
                    }}
                />
            },
        )
    })
}

let initUiApi = (modalRef:modalRef):unit => {
    setUiApi(
        ~showInfoMsg = makeApiFunc("showInfoMsg", apiShowInfoMsg(modalRef, _)),
        ~showErrMsg = makeApiFunc("showErrMsg", apiShowErrMsg(modalRef, _)),
        ~multilineTextInput = makeApiFunc("multilineTextInput", apiMultilineTextInput(modalRef, _)),
    )
}
