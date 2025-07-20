type sorting = array<(MM_wrk_sort_asrts.sortBy, MM_wrk_sort_asrts.sortDir)>

let renderSortBy: (
  ~sortBy: sorting,
  ~onClick: unit => unit,
  ~onCancel: unit => unit,
) => React.element

@react.component
let make: (
  ~init: sorting,
  ~onOk: sorting => unit,
  ~onCancel: unit => unit,
) => React.element
