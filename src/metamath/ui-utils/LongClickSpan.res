open Expln_React_common
open Common

@react.component
let make = (
    ~onShortClick:unit=>unit,
    ~onLongClick:unit=>unit,
    ~children:reElem,
) => {
    let (timerId, setTimerId) = React.useState(() => None)
    let (doShortClick, setDoShortClick) = React.useState(() => false)

    Js.Console.log2(`timerId`, timerId)

    React.useEffect1(() => {
        if (doShortClick) {
            setDoShortClick(_ => false)
            onShortClick()
        }
        None
    }, [doShortClick])

    let actClickBegin = () => {
        Js.Console.log(`actClickBegin`)
        setTimerId(timerId => {
            switch timerId {
                | None => ()
                | Some(timerId) => clearTimeout(timerId)
            }
            Some(setTimeout( onLongClick, 1500 ))
        })
    }

    let actClickEnd = () => {
        Js.Console.log(`actClickEnd`)
        setTimerId(timerId => {
            Js.Console.log2(`actClickEnd:timerId`, timerId)
            switch timerId {
                | None => ()
                | Some(timerId) => {
                    clearTimeout(timerId)
                    setDoShortClick(_ => true)
                }
            }
            None
        })
    }

    <span 
        onMouseDown = {evt => {
            // evt->JsxEvent.Mouse.preventDefault
            // evt->JsxEvent.Mouse.stopPropagation
            actClickBegin()
        }}
        onMouseUp = {evt => {
            // evt->JsxEvent.Mouse.preventDefault
            // evt->JsxEvent.Mouse.stopPropagation
            actClickEnd()
        }}
        onTouchStart = {evt => {
            // evt->JsxEvent.Touch.preventDefault
            // evt->JsxEvent.Touch.stopPropagation
            actClickBegin()
        }}
        onTouchEnd = {evt => {
            // evt->JsxEvent.Touch.preventDefault
            // evt->JsxEvent.Touch.stopPropagation
            actClickEnd()
        }}
        style=ReactDOM.Style.make(~cursor="pointer", ~fontWeight="bold", /* ~display="inline-block", */ ~margin="20px", ())
    >
        children
    </span>
}