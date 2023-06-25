open Expln_React_common
open Expln_React_Mui
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
    ~ref_:option<ReactDOM.Ref.t>=?,
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