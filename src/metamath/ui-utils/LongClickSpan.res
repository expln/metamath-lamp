open Expln_React_common
open UseLongClick

@react.component
let make = (
    ~onClick:option<ReactEvent.Mouse.t=>unit>=?,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<option<clickAttrs>=>unit>=?,
    ~onLongClick:option<unit=>unit>=?,
    ~children:reElem,
    ~style:option<reStyle>=?,
    ~title:option<string>=?,
) => {
    let { 
        onClick, 
        onMouseDown, onMouseUp, onMouseMove, onMouseLeave, onMouseOut,
        onTouchStart, onTouchEnd, onTouchMove, onTouchCancel, 
    } = useLongClick(
        ~onClick,
        ~longClickEnabled,
        ~longClickDelayMs,
        ~onShortClick,
        ~onLongClick,
    )

    <span 
        onClick 
        onMouseDown onMouseUp onMouseMove onMouseLeave onMouseOut
        onTouchStart onTouchEnd onTouchMove onTouchCancel
        ?style ?title 
    >
        children
    </span>
}