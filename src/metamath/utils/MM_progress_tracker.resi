type progressStateMutable
let progressTrackerMutableMake: (
    ~step: float, 
    ~pct: float=?, 
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?, 
    ()
) => progressStateMutable
let progressTrackerMutableSetCurrPct: (progressStateMutable, float) => unit
