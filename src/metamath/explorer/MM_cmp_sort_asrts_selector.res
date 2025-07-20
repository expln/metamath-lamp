open Expln_React_common
open Expln_React_Mui
open MM_wrk_sort_asrts
open Common

type state = {
    sortBys:sortBys
}

let renderSortBys = (
    ~sortBys:sortBys,
    ~onClick:unit=>unit,
    ~onCancel:unit=>unit,
) => {
    let sortByStr = sortBys->Array.map(((sortBy,sortDir)) => {
        sortByGetTitle(sortBy) ++ " " ++ sortDirToStr(sortDir)
    })
    ->Array.join(", ")
    <span>
        <span 
            style=ReactDOM.Style.make(~cursor="pointer", ~borderRadius="4px", ())
            className="dark-grey-bkg-on-hover"
            onClick={_=>onClick()}
        >
            {React.string("Sorting: " ++ sortByStr)}
        </span>
        <span> {React.string(nbsp)} </span>
        <span 
            style=ReactDOM.Style.make(~cursor="pointer", ())
            className="red-on-hover" 
            onClick={_=>onCancel()}
        >
            {React.string("\u2715")}
        </span>
    </span>
}

@react.component
let make = (
    ~init:sortBys,
    ~onOk:sortBys=>unit, 
    ~onCancel:unit=>unit,
) => {
    let (state, setState) = React.useState(() => {sortBys:init})

    let actOk = () => {
        onOk(state.sortBys)
    }

    let actCancel = () => {
        onCancel()
    }

    let actDeleteSortBy = (idx:int) => {
        setState(st => {sortBys:st.sortBys->Array.filterWithIndex((_,i) => i != idx)})
    }

    let actMoveUpSortBy = (idx:int) => {
        if (idx > 0) {
            setState(st => {
                let copy = st.sortBys->Array.copy
                let tmp = copy->Array.getUnsafe(idx-1)
                copy->Array.set(idx-1, copy->Array.getUnsafe(idx))
                copy->Array.set(idx, tmp)
                {sortBys:copy}
            })
        }
    }

    let actMoveDownSortBy = (idx:int) => {
        if (idx < state.sortBys->Array.length-1) {
            setState(st => {
                let copy = st.sortBys->Array.copy
                let tmp = copy->Array.getUnsafe(idx+1)
                copy->Array.set(idx+1, copy->Array.getUnsafe(idx))
                copy->Array.set(idx, tmp)
                {sortBys:copy}
            })
        }
    }

    let actAddSortBy = (sortBys,sortDir) => {
        setState(st => {sortBys:[...st.sortBys, (sortBys,sortDir)]})
    }

    let rndSortByButtons = (idx:int) => {
        <ButtonGroup variant=#outlined size=#small >
            <Button title="Delete" onClick={_=>actDeleteSortBy(idx)}> <MM_Icons.DeleteForever/> </Button>
            <Button title="Move down" onClick={_=>actMoveDownSortBy(idx)}> <MM_Icons.ArrowDownward/> </Button>
            <Button title="Move up" onClick={_=>actMoveUpSortBy(idx)}> <MM_Icons.ArrowUpward/> </Button>
        </ButtonGroup>
    }

    let actUpdateSortBy = (idx, newSortBy) => {
        setState(st => {sortBys:st.sortBys->Array.mapWithIndex(((sortBy,sortDir),i) => {
            if (i == idx) {
                (newSortBy,sortDir)
            } else {
                (sortBy,sortDir)
            }
        })})
    }

    let actUpdateSortDir = (idx, newSortDir) => {
        setState(st => {sortBys:st.sortBys->Array.mapWithIndex(((sortBy,sortDir),i) => {
            if (i == idx) {
                (sortBy,newSortDir)
            } else {
                (sortBy,sortDir)
            }
        })})
    }

    let rndSortControls = ((sortBy,sortDir),idx) => {
        <Row key={idx->Int.toString} alignItems={#center}>
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
        {state.sortBys->Array.mapWithIndex(rndSortControls)->React.array}
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
                        <MenuItem key="" value="">{React.string("")}</MenuItem>,
                        ...allSortByOptions->Array.map(((_,code,descr)) => {
                            <MenuItem key=code value=code>{React.string(descr)}</MenuItem>
                        })
                    ]->React.array
                }
            </Select>
        </FormControl>
        <Row alignItems=#center>
            <Button onClick=(_=>actOk()) variant=#contained > 
                { React.string("Ok") }
            </Button>
            <Button onClick={_=>actCancel()} > {React.string("Cancel")} </Button>
        </Row>
    </Col>
}