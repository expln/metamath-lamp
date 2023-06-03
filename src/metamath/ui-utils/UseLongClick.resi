type clickAttrs = {
    alt:bool,
    shift:bool,
    ctrl:bool,
}

type useLongClick = {
    onClick:ReactEvent.Mouse.t=>unit,

    onMouseDown:ReactEvent.Mouse.t=>unit,
    onMouseUp:ReactEvent.Mouse.t=>unit,
    onMouseMove:ReactEvent.Mouse.t=>unit,
    onMouseLeave:ReactEvent.Mouse.t=>unit,
    onMouseOut:ReactEvent.Mouse.t=>unit,

    onTouchStart:ReactEvent.Touch.t=>unit,
    onTouchEnd:ReactEvent.Touch.t=>unit,
    onTouchMove:ReactEvent.Touch.t=>unit,
    onTouchCancel:ReactEvent.Touch.t=>unit,
}
let useLongClick: (
    ~onClick:option<ReactEvent.Mouse.t=>unit>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<option<clickAttrs>=>unit>,
    ~onLongClick:option<unit=>unit>,
) => useLongClick
