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

let evt2str = strConsumer => evt => strConsumer(ReactEvent.Form.target(evt)["value"])
let evt2bool = boolConsumer => evt => boolConsumer(ReactEvent.Form.target(evt)["checked"])
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