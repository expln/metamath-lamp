open Expln_React_common
open Common

let longClickDelay = 1500

@react.component
let make = (
    ~onClick:ReactEvent.Mouse.t=>unit=?,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int=1000,
    ~onShortClick:unit=>unit=?,
    ~onLongClick:unit=>unit=?,
    ~children:reElem,
    ~style:reStyle=?,
    ~title:string=?,
) => {
    let { onClick, onMouseDown, onMouseUp, onTouchStart, onTouchEnd, } = UseLongClick.useLongClick(
        ~onClick,
        ~longClickEnabled,
        ~longClickDelayMs,
        ~onShortClick,
        ~onLongClick,
    )

    <span onClick onMouseDown onMouseUp onTouchStart onTouchEnd ?style ?title >
        children
    </span>
}