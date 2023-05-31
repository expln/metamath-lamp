open Expln_React_common

type valueLabelDisplay = [ #on | #off | #auto ]
type size = [ #small | #medium ]

@module("@mui/material/Slider") @react.component
external make: (
    ~size:size=?,
    ~defaultValue:float=?,
    ~value:float=?,
    ~valueLabelDisplay:valueLabelDisplay=?,
    ~min:float=?,
    ~max:float=?,
    ~step:float=?,
    ~disabled:bool=?,
    ~onChange:(_, float) => unit,
) => reElem = "default"