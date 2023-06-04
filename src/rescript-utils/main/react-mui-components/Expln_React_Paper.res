open Expln_React_common

type variant = [ #elevation | #outlined ]

@module("@mui/material/Paper") @react.component
external make: (
    ~elevation:int=?,
    ~style: reStyle=?,
    ~variant:variant=?,
    ~square:bool=?,
    ~title:string=?,

    ~onClick:ReactEvent.Mouse.t=>unit=?,
    ~onMouseDown:ReactEvent.Mouse.t=>unit=?,
    ~onMouseUp:ReactEvent.Mouse.t=>unit=?,
    ~onMouseMove:ReactEvent.Mouse.t=>unit=?,
    ~onMouseLeave:ReactEvent.Mouse.t=>unit=?,
    ~onMouseOut:ReactEvent.Mouse.t=>unit=?,

    ~onTouchStart:ReactEvent.Touch.t=>unit=?,
    ~onTouchEnd:ReactEvent.Touch.t=>unit=?,
    ~onTouchMove:ReactEvent.Touch.t=>unit=?,
    ~onTouchCancel:ReactEvent.Touch.t=>unit=?,

    ~children: reElem=?
) => reElem = "default"
