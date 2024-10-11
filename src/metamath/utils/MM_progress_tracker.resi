type progressState
let progressTrackerMake: (
    ~step: float, 
    ~pct: float=?, 
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?
) => progressState
let progressTrackerSetCurrPct: (progressState, float) => unit

type progressStateInt
let progressTrackerIntMake: (
    ~step:float, 
    ~cnt:int=?, 
    ~maxCnt:int,
    ~onProgress: float=>unit=?, 
    ~dontDecrease:bool=?
) => progressStateInt
let progressTrackerIntIncCnt: progressStateInt => unit