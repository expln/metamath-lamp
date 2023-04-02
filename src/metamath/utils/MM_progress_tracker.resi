type progressState
let progressTrackerMake: (
    ~step: float, 
    ~pct: float=?, 
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?, 
    ()
) => progressState
let progressTrackerSetCurrPct: (progressState, float) => unit
