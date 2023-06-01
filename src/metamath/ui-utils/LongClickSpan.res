open Expln_React_common
open Common

let longClickDelay = 1500

@react.component
let make = (
    ~onShortClick:unit=>unit,
    ~onLongClick:unit=>unit,
    ~children:reElem,
) => {
    let (timerId, setTimerId) = React.useState(() => None)
    let (doShortClick, setDoShortClick) = React.useState(() => false)
    let (doLongClick, setDoLongClick) = React.useState(() => None)

    let (lastClickBeginTime, setLastClickBeginTime) = React.useState(() => 0.)
    let (lastClickEndTime, setLastClickEndTime) = React.useState(() => 0.)

    let actReset = () => {
        switch timerId {
            | None => ()
            | Some(timerId) => clearTimeout(timerId)
        }
        setTimerId(_ => None)
        setDoShortClick(_ => false)
        setDoLongClick(_ => None)
    }

    React.useEffect1(() => {
        if (doShortClick) {
            actReset()
            onShortClick()
        }
        None
    }, [doShortClick])

    React.useEffect1(() => {
        switch doLongClick {
            | None => ()
            | Some(timerIdActual) => {
                switch timerId {
                    | None => ()
                    | Some(timerIdExpected) => {
                        actReset()
                        onLongClick()
                    }
                }
            }
        }
        None
    }, [doLongClick])

    let actClickBegin = () => {
        Js.Console.log(`actClickBegin`)
        if (Js.Date.now() -. lastClickBeginTime > 500.) {
            setLastClickBeginTime(_ => Js.Date.now())
            setTimerId(timerId => {
                switch timerId {
                    | None => ()
                    | Some(timerId) => clearTimeout(timerId)
                }
                let timerIdLocal = ref(stubTimeoutId)
                timerIdLocal := setTimeout( 
                    () => {
                        setDoLongClick(_ => Some(timerIdLocal.contents))
                    }, 
                    longClickDelay 
                )
                Some(timerIdLocal.contents)
            })
            setDoShortClick(_ => false)
            setDoLongClick(_ => None)
        }

    }

    let actClickEnd = () => {
        Js.Console.log(`actClickEnd`)
        if (Js.Date.now() -. lastClickEndTime > 500.) {
            setLastClickEndTime(_ => Js.Date.now())
            setTimerId(timerId => {
                switch timerId {
                    | None => ()
                    | Some(timerId) => {
                        clearTimeout(timerId)
                        setDoShortClick(_ => true)
                        setDoLongClick(_ => None)
                    }
                }
                None
            })
        }
    }

    <span 
        onMouseDown = {evt => {
            Js.Console.log(`onMouseDown`)
            evt->JsxEvent.Mouse.preventDefault
            evt->JsxEvent.Mouse.stopPropagation
            actClickBegin()
        }}
        onMouseUp = {evt => {
            Js.Console.log(`onMouseUp`)
            evt->JsxEvent.Mouse.preventDefault
            evt->JsxEvent.Mouse.stopPropagation
            actClickEnd()
        }}
        onTouchStart = {evt => {
            Js.Console.log(`onTouchStart`)
            evt->JsxEvent.Touch.stopPropagation
            actClickBegin()
        }}
        onTouchEnd = {evt => {
            Js.Console.log(`onTouchEnd`)
            evt->JsxEvent.Touch.stopPropagation
            actClickEnd()
        }}
        style=ReactDOM.Style.make(
            ~cursor="pointer", 
            ~fontWeight="bold", 
            // ~display="inline-block", 
            ~margin="20px", 
            ()
        )
    >
        children
    </span>
}