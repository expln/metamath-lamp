open Expln_React_common
open Common
open UseLongClick

@react.component
let make = (
    ~onClick:ReactEvent.Mouse.t=>unit=?,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<clickAttrs>=>unit=?,
    ~onLongClick:unit=>unit=?,
    ~children:reElem,
    ~style:reStyle=?,
    ~title:string=?,
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