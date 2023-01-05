@react.component
let make = () => {
  <Expln_React_ViewSelector
    allViews = [
      {id: "1", title: "MM_cmp_root", render: _ => <MM_cmp_root/>},
    ]
    defaultViewId="1"
  />
}
