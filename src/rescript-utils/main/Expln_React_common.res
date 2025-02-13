type reCmp<'a> = React.component<'a>
type reElem = React.element
type reStyle = ReactDOM.Style.t

type reClipboardHnd = ReactEvent.Clipboard.t=>unit
type reCompositionHnd = ReactEvent.Composition.t=>unit
type reKeyboardHnd = ReactEvent.Keyboard.t=>unit
type reFocusHnd = ReactEvent.Focus.t=>unit
type reFormHnd = ReactEvent.Form.t=>unit
type reMouseHnd = ReactEvent.Mouse.t=>unit
type rePointerHnd = ReactEvent.Pointer.t=>unit
type reSelectionHnd = ReactEvent.Selection.t=>unit
type reTouchHnd = ReactEvent.Touch.t=>unit
type reUIHnd = ReactEvent.UI.t=>unit
type reWheelHnd = ReactEvent.Wheel.t=>unit
type reMediaHnd = ReactEvent.Media.t=>unit
type reImageHnd = ReactEvent.Image.t=>unit
type reAnimationHnd = ReactEvent.Animation.t=>unit
type reTransitionHnd = ReactEvent.Transition.t=>unit

type reClickEvt = {
    ctrl:bool,
    alt:bool,
    shift:bool,
    left:bool,
    middle:bool,
    right:bool,
}

let evt2str = (strConsumer:string=>unit) => {
    evt => {
        let target = ReactEvent.Form.target(evt)
        switch target["value"]->Nullable.toOption {
            | None => {
                Console.error2("target", target)
                Exn.raiseError(`evt2str: cannot get 'value' property on the event target.`)
            }
            | Some(str) => strConsumer(str)
        }
    }
}
let evt2bool = (boolConsumer:bool=>unit) => {
    evt => {
        let target = ReactEvent.Form.target(evt)
        switch target["checked"]->Nullable.toOption {
            | None => {
                Console.error2("target", target)
                Exn.raiseError(`evt2bool: cannot get 'checked' property on the event target.`)
            }
            | Some(bool) => boolConsumer(bool)
        }
    }
}
let evt2click = (clickConsumer:reClickEvt=>unit):reMouseHnd => evt => {
    clickConsumer({
        ctrl: evt->ReactEvent.Mouse.ctrlKey,
        alt: evt->ReactEvent.Mouse.altKey,
        shift: evt->ReactEvent.Mouse.shiftKey,
        left: evt->ReactEvent.Mouse.button == 0,
        middle: evt->ReactEvent.Mouse.button == 1,
        right: evt->ReactEvent.Mouse.button == 2,
    })
}

external reElem2Obj: reElem => Nullable.t<{..}> = "%identity"