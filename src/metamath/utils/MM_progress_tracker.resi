type progressState
let progressTrackerMake: (~step: float, ~pct: float=?, ~onProgress: float=>unit=?, ~dontDecrease:bool=?, unit) => progressState
let progressTrackerSetCurrPct: (progressState, float) => progressState
