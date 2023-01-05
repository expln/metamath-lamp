open Expln_React_common

@module("./AccordionSummaryStyled.js") @react.component
external make: (
    ~onClick: ()=>unit=?,
    ~expandIcon: reElem=?,
    ~children: reElem,
) => reElem = "default"
