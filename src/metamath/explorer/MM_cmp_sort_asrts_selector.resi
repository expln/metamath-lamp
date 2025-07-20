open MM_wrk_sort_asrts

let renderSortBys: (
  ~sortBys: sortBys,
  ~onClick: unit => unit,
  ~onCancel: unit => unit,
) => React.element

@react.component
let make: (
  ~init: sortBys,
  ~onOk: sortBys => unit,
  ~onCancel: unit => unit,
) => React.element
