open Expln_React_Mui
open Expln_React_common

type view = {
  id: string,
  title: string,
  render: unit => reElem
}

let renderDefaultView = idOfNonExistentView =>
    <Paper>{ React.string("View not found: " ++ idOfNonExistentView) }</Paper>

@react.component
let make = (~allViews: array<view>, ~defaultViewId = ?) => {
  let (selectedViewId, setSelectedViewId) = React.useState(_ => defaultViewId)

  let openView = view => setSelectedViewId(_ => Some(view.id))

  let renderViewListItem = view =>
    <ListItem key={view.id}>
      <ListItemButton onClick={_ => openView(view)}> 
        <ListItemIcon>
          <Icons.BrightnessLow/>
        </ListItemIcon>
        <ListItemText> {React.string(view.title)} </ListItemText> 
      </ListItemButton>
    </ListItem>

   let renderViewById = viewId =>
      allViews
      -> Belt.Array.getBy(view => view.id == viewId)
      -> Belt.Option.map(view => view.render())
      -> Belt.Option.getWithDefault(renderDefaultView(viewId))


  switch selectedViewId {
  | Some(id) => renderViewById(id)
  | None => <List> {allViews->Belt.Array.map(renderViewListItem)->React.array} </List>
  }
}
