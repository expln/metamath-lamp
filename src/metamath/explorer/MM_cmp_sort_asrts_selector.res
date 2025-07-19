open Expln_React_common
open Expln_React_Mui
open MM_react_common

type sortBy = UsageCnt | AsrtLen | AsrtLabel | NumOfHyps
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
        | NumOfHyps => "NumOfHyps"
    }
}

let allSortByOptions = [
    (UsageCnt, sortByToStr(UsageCnt), "Usage count"),
    (AsrtLen, sortByToStr(AsrtLen), "Length of the assertion"),
    (AsrtLabel, sortByToStr(AsrtLabel), "Label"),
    (NumOfHyps, sortByToStr(NumOfHyps), "Number of hypotheses"),
]

let sortByFromStr = str => {
    switch allSortByOptions->Array.find(((_,c,_)) => c == str) {
        | Some((v,_,_)) => v
        | None => panic(`Cannot convert the value of '${str}' to a sortBy.`)
    }
}

let sortDirToStr = sortDir => {
    switch sortDir {
        | Asc => "Asc"
        | Dsc => "Dsc"
    }
}

let allSortDirOptions = [
    (Asc, sortDirToStr(Asc), "Ascending"),
    (Dsc, sortDirToStr(Dsc), "Descending"),
]

let sortDirFromStr = str => {
    switch allSortDirOptions->Array.find(((_,c,d)) => c == str) {
        | Some((v,_,_)) => v
        | None => panic(`Cannot convert the value of '${str}' to a sortDir.`)
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

    let actDeleteSortBy = (idx:int) => {
        setState(st => {sortBy:st.sortBy->Array.filterWithIndex((e,i) => i != idx)})
    }

    let actMoveUpSortBy = (idx:int) => {
        if (idx > 0) {
            setState(st => {
                let copy = st.sortBy->Array.copy
                let tmp = copy->Array.getUnsafe(idx-1)
                copy->Array.set(idx-1, copy->Array.getUnsafe(idx))
                copy->Array.set(idx, tmp)
                {sortBy:copy}
            })
        }
    }

    let actMoveDownSortBy = (idx:int) => {
        if (idx < state.sortBy->Array.length-1) {
            setState(st => {
                let copy = st.sortBy->Array.copy
                let tmp = copy->Array.getUnsafe(idx+1)
                copy->Array.set(idx+1, copy->Array.getUnsafe(idx))
                copy->Array.set(idx, tmp)
                {sortBy:copy}
            })
        }
    }

    let actAddSortBy = (sortBy,sortDir) => {
        setState(st => {sortBy:[...st.sortBy, (sortBy,sortDir)]})
    }

    let rndSortByButtons = (idx:int) => {
        <ButtonGroup variant=#outlined size=#small >
            <Button title="Delete" onClick={_=>actDeleteSortBy(idx)}> <MM_Icons.DeleteForever/> </Button>
            <Button title="Move down" onClick={_=>actMoveDownSortBy(idx)}> <MM_Icons.ArrowDownward/> </Button>
            <Button title="Move up" onClick={_=>actMoveUpSortBy(idx)}> <MM_Icons.ArrowUpward/> </Button>
        </ButtonGroup>
    }

    let actUpdateSortBy = (idx, newSortBy) => {
        setState(st => {sortBy:st.sortBy->Array.mapWithIndex(((sortBy,sortDir),i) => {
            if (i == idx) {
                (newSortBy,sortDir)
            } else {
                (sortBy,sortDir)
            }
        })})
    }

    let actUpdateSortDir = (idx, newSortDir) => {
        setState(st => {sortBy:st.sortBy->Array.mapWithIndex(((sortBy,sortDir),i) => {
            if (i == idx) {
                (sortBy,newSortDir)
            } else {
                (sortBy,sortDir)
            }
        })})
    }

    let rndSortControls = ((sortBy,sortDir),idx) => {
        <Row key={idx->Int.toString}>
            {rndSortByButtons(idx)}
            <FormControl size=#small>
                <Select
                    value=sortByToStr(sortBy)
                    onChange=evt2str(str => actUpdateSortBy(idx,sortByFromStr(str)))
                >
                    {
                        allSortByOptions->Array.map(((_,code,descr)) => {
                            <MenuItem key=code value=code>{React.string(descr)}</MenuItem>
                        })->React.array
                    }
                </Select>
            </FormControl>
            <FormControl size=#small>
                <Select
                    value=sortDirToStr(sortDir)
                    onChange=evt2str(str => actUpdateSortDir(idx,sortDirFromStr(str)))
                >
                    {
                        allSortDirOptions->Array.map(((_,code,descr)) => {
                            <MenuItem key=code value=code>{React.string(descr)}</MenuItem>
                        })->React.array
                    }
                </Select>
            </FormControl>
        </Row>
    }

    <Col spacing=1.>
        {state.sortBy->Array.mapWithIndex(rndSortControls)->React.array}
        <FormControl size=#small>
            <InputLabel id="sortBy-add-new-item">"Select property to sort by"</InputLabel>
            <Select
                sx={"width": 250}
                labelId="sortBy-add-new-item"
                value=""
                label="Select property to sort by"
                onChange=evt2str(str => actAddSortBy(sortByFromStr(str),Asc))
            >
                {
                    [
                        <MenuItem value="">{React.string("")}</MenuItem>,
                        ...allSortByOptions->Array.map(((_,code,descr)) => {
                            <MenuItem key=code value=code>{React.string(descr)}</MenuItem>
                        })
                    ]->React.array
                }
            </Select>
        </FormControl>
        <Row alignItems=#center>
            <Button onClick=(_=>actOk()) variant=#contained > 
                { React.string("Sort") }
            </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    </Col>
}