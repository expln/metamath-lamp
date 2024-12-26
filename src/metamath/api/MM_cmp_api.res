open Expln_React_Modal

@react.component
let make = React.memo((
    ~modalRef:modalRef, 
) => {

    React.useEffect0(() => {
        MM_api_ui.initUiApi(modalRef)
        MM_api_macros.initMacroApi()
        None
    })

    React.null
})