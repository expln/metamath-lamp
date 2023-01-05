open Expln_React_Mui


@react.component
let make = (~title) => {
    let (cnt, setCnt) = React.useState(_ => 0)

    <Button onClick={_=>setCnt(prev => prev+1)}>
        {React.string(`${title}. Clicked: ${cnt->Belt_Int.toString}`)}
    </Button>
}