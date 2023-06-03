open Common

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
    timerId:option<timeoutID>,
    doShortClick: option<option<clickAttrs>>,
    doLongClick: option<timeoutID>,
    longClickDelayMs:int,
}

let makeInitialState = (longClickDelayMs:int) => {
    {
        lastClickBeginTime: 0.,
        lastClickEndTime: 0.,
        timerId:None,
        doShortClick: None,
        doLongClick: None,
        longClickDelayMs,
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

let markLongClickIsRequested = (st:state, timerId:timeoutID):state => {
    {
        ...st,
        doShortClick: None,
        doLongClick: Some(timerId),
    }
}

let repeatDelayMs = 300.

let updateStateOnClickBegin = (st:state, updateState: (state=>state)=>unit):state => {
    if (Js.Date.now() -. st.lastClickBeginTime > repeatDelayMs) {
        {
            ...resetState(st),
            lastClickBeginTime: Js.Date.now(),
            timerId: {
                let timerIdLocal = ref(stubTimeoutId)
                timerIdLocal := setTimeout( 
                    () => updateState(markLongClickIsRequested(_, timerIdLocal.contents)), 
                    st.longClickDelayMs 
                )
                Some(timerIdLocal.contents)
            }
        }
    } else {
        st
    }
}

let updateStateOnClickEnd = (st:state, evt:option<ReactEvent.Mouse.t>):state => {
    if (Js.Date.now() -. st.lastClickEndTime > repeatDelayMs) {
        {
            ...st,
            lastClickEndTime: Js.Date.now(),
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
    let (state, setState) = React.useState(() => makeInitialState(longClickDelayMs))

    React.useEffect1(() => {
        switch state.doShortClick {
            | None => ()
            | Some(clickAttrsOpt) => {
                setState(resetState)
                onShortClick->Belt_Option.getExn(clickAttrsOpt)
            }
        }
        None
    }, [state.doShortClick])

    React.useEffect1(() => {
        if (state->shouldDoLongClick) {
            setState(resetState)
            onLongClick->Belt_Option.getExn()
        }
        None
    }, [state.doLongClick])

    let actClickBegin = () => {
        setState(updateStateOnClickBegin(_, setState))
    }

    let actClickEnd = (mEvt:option<ReactEvent.Mouse.t>) => {
        setState(updateStateOnClickEnd(_, mEvt))
    }

    let actClickCancel = () => {
        setState(resetState)
    }

    {
        onClick: evt => {
            if (!longClickEnabled && onClick->Belt_Option.isSome) {
                onClick->Belt_Option.getExn(evt)
            }
        },
        onMouseDown: _ => {
            if (longClickEnabled) {
                actClickBegin()
            }
        },
        onMouseUp: evt => {
            if (longClickEnabled) {
                actClickEnd(Some(evt))
            }
        },
        onMouseMove: _ => {
            if (longClickEnabled) {
                actClickCancel()
            }
        },
        onMouseLeave: _ => {
            if (longClickEnabled) {
                actClickCancel()
            }
        },
        onMouseOut: _ => {
            if (longClickEnabled) {
                actClickCancel()
            }
        },
        onTouchStart: _ => {
            if (longClickEnabled) {
                actClickBegin()
            }
        },
        onTouchEnd: _ => {
            if (longClickEnabled) {
                actClickEnd(None)
            }
        },
        onTouchMove: _ => {
            if (longClickEnabled) {
                actClickCancel()
            }
        },
        onTouchCancel: _ => {
            if (longClickEnabled) {
                actClickCancel()
            }
        },
    }
}