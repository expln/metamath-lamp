type progressState
let progressTrackerMake: (
    ~step: float, 
    ~pct: float=?, 
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?, 
    ()
) => progressState
let progressTrackerSetCurrPct: (progressState, float) => progressState

type progressStateMutable
let progressTrackerMutableMake: (
    ~step: float, 
    ~pct: float=?, 
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?, 
    ()
) => progressStateMutable
let progressTrackerMutableSetCurrPct: (progressStateMutable, float) => unit
