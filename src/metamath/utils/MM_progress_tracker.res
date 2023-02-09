let scaleFactor = 100_000
let scaleFactorF = scaleFactor->Belt_Int.toFloat

type progressStateMutable = {
    mutable step: int,
    numOfSteps: int,
    mutable lastSentNumOfSteps: int,
    onProgress: option<float=>unit>,
    dontDecrease: bool
}

let pctToNumOfSteps = (~pct:float, ~step:int):int => {
    (pct *. scaleFactorF)->Belt_Float.toInt / step
}

let numOfStepsToPct = (~numOfSteps:int, ~step:int):float => {
    (numOfSteps * step)->Belt_Int.toFloat /. scaleFactorF
}

let progressTrackerMutableMake = (
    ~step:float, 
    ~pct=0., 
    ~onProgress: option<float=>unit>=?, 
    ~dontDecrease:bool=false, 
    ()
):progressStateMutable => {
    let step = (step *. scaleFactorF)->Belt_Float.toInt
    let numOfSteps = pctToNumOfSteps(~pct, ~step)
    {
        step,
        numOfSteps,
        lastSentNumOfSteps: numOfSteps,
        onProgress,
        dontDecrease,
    }
}

let progressTrackerMutableSetCurrPct = (state:progressStateMutable, curPct:float):unit => {
    switch state.onProgress {
        | None => ()
        | Some(onProgress) => {
            let curNumOfSteps = pctToNumOfSteps(~pct=curPct, ~step=state.step)
            if (state.dontDecrease && curNumOfSteps > state.lastSentNumOfSteps || !state.dontDecrease && curNumOfSteps != state.lastSentNumOfSteps) {
                onProgress(numOfStepsToPct(~numOfSteps=curNumOfSteps, ~step=state.step))
                state.lastSentNumOfSteps = curNumOfSteps
            }
        }
    }
}