open Expln_React_UseResizeObserver
open Expln_React_common

@react.component
let make = (~top:int=0, ~header:reElem, ~content:int=>reElem) => {
    let (contentTop, setContentTop) = React.useState(_ => top)

    let headerRef = React.useRef(Nullable.null)
    useClientSizeObserver(headerRef, (_, headerHeight) => setContentTop(_ => top + headerHeight))

    <>
        <div 
            ref=ReactDOM.Ref.domRef(headerRef) 
            style=ReactDOM.Style.make(~position="sticky", ~top=`${top->Belt_Int.toString}px`, ~zIndex="1000", ~background="white", ())
        >
            {header}
        </div>
        {content(contentTop)}
    </>
}