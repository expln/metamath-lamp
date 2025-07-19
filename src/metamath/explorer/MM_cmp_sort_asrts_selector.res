open Expln_React_common
open Expln_React_Mui
open MM_react_common

type sortBy = UsageCnt | AsrtLen | AsrtLabel
type sortDir = Asc | Dsc

type state = {
    sortBy:array<(sortBy,sortDir)>
}

let panic = (msg:string):'a => Common.panic(`MM_cmp_sort_asrts_selector: ${msg}`)

let sortByToStr = sortBy => {
    switch sortBy {
        | UsageCnt => "UsageCnt"
        | AsrtLen => "AsrtLen"
        | AsrtLabel => "AsrtLabel"
    }
}

let sortByFromStr = str => {
    switch str {
        | "UsageCnt" => UsageCnt
        | "AsrtLen" => AsrtLen
        | "AsrtLabel" => AsrtLabel
        | _ => panic(`Cannot convert value of '${str}' to a sortBy.`)
    }
}

@react.component
let make = (
    ~init:array<(sortBy,sortDir)>,
    ~onOk:array<(sortBy,sortDir)>=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (state, setState) = React.useState(() => {sortBy:init})

    let actOk = () => {
        onOk(state.sortBy)
    }

    let actCancel = () => {
        onCancel()
    }

    <Col spacing=1.>
        <Row alignItems=#center>
            <Button onClick=(_=>actOk()) variant=#contained > 
                { React.string("Sort") }
            </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    </Col>
}