open MM_context
open Expln_React_Modal
open MM_wrk_pre_ctx_data
open Common

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
}:props) => {
    <pre>
        {
            (
                "Additional info comments containing 'syntax':\n\n"
                ++
                preCtxData.ctxV.val.full->getAddInfoComments
                    ->Array.filter(String.includes(_,"syntax"))
                    ->Array.join("\n\n\n-----------------------------------\n")
            )->React.string
        }
    </pre>

}, propsAreSame)