type useLongClick = {
    onClick: ReactEvent.Mouse.t => unit,
    onMouseDown: ReactEvent.Mouse.t => unit,
    onMouseUp: ReactEvent.Mouse.t => unit,
    onTouchStart: ReactEvent.Touch.t => unit,
    onTouchEnd: ReactEvent.Touch.t => unit,
}
let useLongClick: (
    ~onClick:option<ReactEvent.Mouse.t=>unit>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<unit=>unit>,
    ~onLongClick:option<unit=>unit>,
) => useLongClick
