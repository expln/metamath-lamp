open Common

type useLongClick = {
    onClick:ReactEvent.Mouse.t=>unit,
    onMouseDown:ReactEvent.Mouse.t=>unit,
    onMouseUp:ReactEvent.Mouse.t=>unit,
    onTouchStart:ReactEvent.Touch.t=>unit,
    onTouchEnd:ReactEvent.Touch.t=>unit,
}

let repeatDelayMs = 500.

type state = {
    lastClickBeginTime: float,
    lastClickEndTime: float,
    timerId:option<timeoutID>,
    doShortClick: option<ReactEvent.Mouse.t>,
    doLongClick: option<timeoutID>,
    longClickDelayMs:int,
}

let makeInitialState = (longClickDelayMs:int) => {
    {
        lastClickBeginTime: 0.,
        lastClickEndTime: 0.,
        timerId:None,
        doShortClick: false,
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
        doShortClick: false,
        doLongClick: None,
    }
}

let markShortClickIsRequested = (st:state):state => {
    {
        ...st,
        doShortClick: true,
        doLongClick: None,
    }
}

let markLongClickIsRequested = (st:state, timerId:timeoutID):state => {
    {
        ...st,
        doShortClick: false,
        doLongClick: Some(timerId),
    }
}

let updateStateOnClickBegin = (st:state, updateState: (state=>state)=>unit):state => {
    if (Js.Date.now() -. st.lastClickBeginTime > repeatDelayMs) {
        {
            ...resetState(st),
            lastClickBeginTime: Js.Date.now(),
            timerId: {
                let timerIdLocal = ref(stubTimeoutId)
                timerIdLocal := setTimeout( 
                    () => updateState(markLongClickIsRequested(_, Some(timerIdLocal.contents))), 
                    st.longClickDelayMs 
                )
                Some(timerIdLocal.contents)
            }
        }
    } else {
        st
    }
}

let updateStateOnClickEnd = (st:state, updateState: (state=>state)=>unit):state => {
    if (Js.Date.now() -. lastClickEndTime > repeatDelayMs) {
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
            doShortClick: st.timerId->Belt_Option.isSome,
            doLongClick: None,
        }
    } else {
        st
    }
}

let useLongClick = (
    ~onClick:option<ReactEvent.Mouse.t=>unit>,
    ~longClickEnabled:bool,
    ~longClickDelayMs:int,
    ~onShortClick:option<unit=>unit>,
    ~onLongClick:option<unit=>unit>,
):useLongClick => {
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
            if (onShortClick->Belt_Option.isSome) {
                onShortClick->Belt_Option.getExn()
            }
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
                        if (onLongClick->Belt_Option.isSome) {
                            onLongClick->Belt_Option.getExn()
                        }
                    }
                }
            }
        }
        None
    }, [doLongClick])

    let actClickBegin = () => {
        if (Js.Date.now() -. lastClickBeginTime > repeatDelayMs) {
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
                    longClickDelayMs 
                )
                Some(timerIdLocal.contents)
            })
            setDoShortClick(_ => false)
            setDoLongClick(_ => None)
        }

    }

    let actClickEnd = (mEvt:option<ReactEvent.Mouse.t>, tEvt:option<ReactEvent.Touch.t>) => {
        if (longClickEnabled) {
            mEvt->Belt_Option.forEach(ReactEvent.Mouse.stopPropagation)
            mEvt->Belt_Option.forEach(ReactEvent.Mouse.preventDefault)
            tEvt->Belt_Option.forEach(ReactEvent.Touch.stopPropagation)
        }
        if (Js.Date.now() -. lastClickEndTime > repeatDelayMs) {
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

    {
        onClick: evt => {
            if (!longClickEnabled && onClick->Belt_Option.isSome) {
                onClick->Belt_Option.getExn(evt)
            }
            if (longClickEnabled) {
                evt->ReactEvent.Mouse.stopPropagation
                evt->ReactEvent.Mouse.preventDefault
            }
        },
        onMouseDown: _ => {
            if (longClickEnabled) {
                actClickBegin()
            }
        },
        onMouseUp: evt => {
            if (longClickEnabled) {
                actClickEnd(Some(evt), None)
            }
        },
        onTouchStart: _ => {
            if (longClickEnabled) {
                actClickBegin()
            }
        },
        onTouchEnd: evt => {
            if (longClickEnabled) {
                actClickEnd(None, Some(evt))
            }
        },
    }
}