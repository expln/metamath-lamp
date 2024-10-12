let scaleFactor = 100_000
let scaleFactorF = scaleFactor->Belt_Int.toFloat

type progressState = {
    mutable step: int,
    numOfSteps: int,
    mutable lastSentNumOfSteps: int,
    onProgress: option<float=>unit>,
    dontDecrease: bool,
}

type progressStateInt = {
    progressState: progressState,
    mutable cnt: int,
    maxCnt: float,
    stepInt:int,
}

let pctToNumOfSteps = (~pct:float, ~step:int):int => {
    (pct *. scaleFactorF)->Belt_Float.toInt / step
}

let numOfStepsToPct = (~numOfSteps:int, ~step:int):float => {
    (numOfSteps * step)->Belt_Int.toFloat /. scaleFactorF
}

let progressTrackerMake = (
    ~step:float, 
    ~pct=0., 
    ~onProgress: option<float=>unit>=?, 
    ~dontDecrease:bool=false
):progressState => {
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

let progressTrackerSetCurrPct = (state:progressState, curPct:float):unit => {
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

let progressTrackerIntMake = (
    ~step:float, 
    ~cnt=0, 
    ~maxCnt:int,
    ~onProgress: option<float=>unit>=?, 
    ~dontDecrease:bool=false
):progressStateInt => {
    {
        progressState: progressTrackerMake(
            ~step, 
            ~pct = cnt->Belt_Int.toFloat /. maxCnt->Belt_Int.toFloat, 
            ~onProgress?, 
            ~dontDecrease
        ),
        cnt: cnt,
        maxCnt: maxCnt->Belt_Int.toFloat,
        stepInt: (maxCnt->Belt_Int.toFloat *. step /. 2.)->Belt_Float.toInt->Math.Int.max(1)
    }
    
}

let progressTrackerIntIncCnt = (state:progressStateInt):unit => {
    state.cnt = state.cnt + 1
    if (mod(state.cnt, state.stepInt) == 0) {
        state.progressState->progressTrackerSetCurrPct(
            state.cnt->Belt_Int.toFloat /. state.maxCnt
        )
    }
}