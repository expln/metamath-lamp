open Expln_React_common
open Expln_React_Mui
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
    ~ref_:ReactDOM.Ref.t=?,
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

    <Paper
        onClick 
        onMouseDown onMouseUp onMouseMove onMouseLeave onMouseOut
        onTouchStart onTouchEnd onTouchMove onTouchCancel
        ?style ?title ref=?ref_
    >
        children
    </Paper>
}