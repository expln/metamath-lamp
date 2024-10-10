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

type clickAttrs = {
    alt:bool,
    shift:bool,
    ctrl:bool,
}

type state = {
    lastClickBeginTime: float,
    lastClickEndTime: float,
    timerId:option<timeoutId>,
    clickBeginScreenX:int,
    clickBeginScreenY:int,
    doShortClick: option<option<clickAttrs>>,
    doLongClick: option<timeoutId>,
}

let makeInitialState = () => {
    {
        lastClickBeginTime: 0.,
        lastClickEndTime: 0.,
        timerId:None,
        clickBeginScreenX:0,
        clickBeginScreenY:0,
        doShortClick: None,
        doLongClick: None,
    }
}

let resetState = (st:state):state => {
    {
        ...st,
        timerId: {
            switch st.timerId {
                | None => ()
                | Some(timerId) => clearTimeout(timerId)
            }
            None
        },
        doShortClick: None,
        doLongClick: None,
    }
}

let markLongClickIsRequested = (st:state, timerId:timeoutId):state => {
    {
        ...st,
        doShortClick: None,
        doLongClick: Some(timerId),
    }
}

let repeatDelayMs = 300.

let updateStateOnClickBegin = (
    st:state, 
    screenX:int, 
    screenY:int, 
    longClickDelayMs:int,
    updateState: (state=>state)=>unit
):state => {
    if (Date.now() -. st.lastClickBeginTime > repeatDelayMs) {
        {
            ...resetState(st),
            lastClickBeginTime: Date.now(),
            clickBeginScreenX:screenX,
            clickBeginScreenY:screenY,
            timerId: {
                let timerIdLocal = ref(None)
                timerIdLocal := Some(setTimeout( 
                    () => {
                        switch timerIdLocal.contents {
                            | None => ()
                            | Some(timerId) => updateState(markLongClickIsRequested(_, timerId))
                        }
                    }, 
                    longClickDelayMs 
                ))
                timerIdLocal.contents
            }
        }
    } else {
        st
    }
}

let updateStateOnClickEnd = (st:state, evt:option<ReactEvent.Mouse.t>):state => {
    if (Date.now() -. st.lastClickEndTime > repeatDelayMs) {
        {
            ...st,
            lastClickEndTime: Date.now(),
            timerId: {
                switch st.timerId {
                    | None => ()
                    | Some(timerId) => clearTimeout(timerId)
                }
                None
            },
            doShortClick: st.timerId->Belt_Option.map(_ => evt->Belt_Option.map(evt => {
                {
                    alt: evt->ReactEvent.Mouse.altKey,
                    shift: evt->ReactEvent.Mouse.shiftKey,
                    ctrl: evt->ReactEvent.Mouse.ctrlKey,
                }
            })),
            doLongClick: None,
        }
    } else {
        st
    }
}

let shouldDoLongClick = (st:state):bool => {
    switch st.doLongClick {
        | None => false
        | Some(timerIdActual) => {
            switch st.timerId {
                | None => false
                | Some(timerIdExpected) => timerIdExpected == timerIdActual
            }
        }
    }
}

let useLongClick = (
    ~onClick:option<ReactEvent.Mouse.t=>unit>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<option<clickAttrs>=>unit>,
    ~onLongClick:option<unit=>unit>,
):useLongClick => {
    let (state, setState) = React.useState(() => makeInitialState())

    React.useEffect1(() => {
        switch state.doShortClick {
            | None => ()
            | Some(clickAttrsOpt) => {
                setState(resetState)
                onShortClick->Belt_Option.forEach(onShortClick => onShortClick(clickAttrsOpt))
            }
        }
        None
    }, [state.doShortClick])

    React.useEffect1(() => {
        if (state->shouldDoLongClick) {
            setState(resetState)
            onLongClick->Belt_Option.forEach(onLongClick => onLongClick())
        }
        None
    }, [state.doLongClick])

    let actClickBegin = (screenX:int, screenY:int) => {
        setState(updateStateOnClickBegin(_, screenX, screenY, longClickDelayMs, setState))
    }

    let actClickEnd = (mEvt:option<ReactEvent.Mouse.t>) => {
        setState(updateStateOnClickEnd(_, mEvt))
    }

    let actClickMove = (screenX:int, screenY:int) => {
        if (
            state.timerId->Belt_Option.isSome
            && (
                (state.clickBeginScreenX - screenX)->Math.Int.abs > 3
                || (state.clickBeginScreenY - screenY)->Math.Int.abs > 3
            )
        ) {
            setState(resetState)
        }
    }

    let actMouseMove = (evt:ReactEvent.Mouse.t) => {
        if (longClickEnabled) {
            actClickMove( evt->ReactEvent.Mouse.screenX, evt->ReactEvent.Mouse.screenY )
        }
    }

    let getScreenCoordsFromTouchList = (touchList:{..}):option<(int,int)> => {
        if (touchList["length"] > 0) {
            let touch = touchList["item"](. 0)
            Some(( touch["screenX"], touch["screenY"] ))
        } else {
            None
        }
    }

    let actTouchMove = (evt:ReactEvent.Touch.t) => {
        if (longClickEnabled) {
            switch getScreenCoordsFromTouchList(evt->ReactEvent.Touch.touches) {
                | Some((screenX, screenY)) => actClickMove(screenX, screenY)
                | _ => ()
            }
        }
    }

    let actTouchCancel = (_:ReactEvent.Touch.t) => {
        if (longClickEnabled) {
            setState(resetState)
        }
    }

    {
        onClick: evt => {
            if (!longClickEnabled && onClick->Belt_Option.isSome) {
                onClick->Belt_Option.getExn(evt)
            }
        },

        onMouseDown: evt => {
            if (longClickEnabled) {
                actClickBegin(evt->ReactEvent.Mouse.screenX, evt->ReactEvent.Mouse.screenY)
            }
        },
        onMouseUp: evt => {
            if (longClickEnabled) {
                actClickEnd(Some(evt))
            }
        },
        onMouseMove: actMouseMove,
        onMouseLeave: actMouseMove,
        onMouseOut: actMouseMove,

        onTouchStart: evt => {
            if (longClickEnabled) {
                switch getScreenCoordsFromTouchList(evt->ReactEvent.Touch.touches) {
                    | Some((screenX, screenY)) => actClickBegin(screenX, screenY)
                    | _ => ()
                }
            }
        },
        onTouchEnd: _ => {
            if (longClickEnabled) {
                actClickEnd(None)
            }
        },
        onTouchMove: actTouchMove,
        onTouchCancel: actTouchCancel,
    }
}