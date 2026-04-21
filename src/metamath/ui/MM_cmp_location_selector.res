open Expln_React_common
open Expln_React_Mui
open MM_react_common
open MM_wrk_editor
open MM_wrk_pre_ctx_data
open MM_context

type state = {
    newLoc: location,
}

let makeInitialState = (newLoc: location) => {
    {
        newLoc,
    }
}

let setNewLoc = (_, newLoc):state => {
    {
        newLoc:newLoc
    }
}

let rndIconButton = (
    ~icon:reElem, 
    ~onClick:unit=>unit, 
    ~active:bool=true, 
    ~title:option<string>=?, 
    ~color:option<string>=Some("primary")
) => {
    <span ?title>
        <IconButton disabled={!active} onClick={_ => onClick()} ?color> icon </IconButton>
    </span>
}

let getAllLabels = (preCtxData:preCtxData):array<string> => {
    let preCtx = preCtxData.ctxV.val.min
    let allFramesInDeclarationOrder = preCtx->getAllFramesArr
        ->Expln_utils_common.sortInPlaceWith(Expln_utils_common.comparatorByInt(frm => frm.ord))
    allFramesInDeclarationOrder->Array.map(frm => frm.label)
}

@val external window: {..} = "window"

@react.component
let make = (
    ~loc:location,
    ~editMode:bool,
    ~onEditRequested:unit=>unit,
    ~onEditDone:location=>unit,
    ~onEditCancel:unit=>unit,
    ~preCtxData:preCtxData,
) => {
    let (state, setState) = React.useState(_ => makeInitialState(loc))

    let (allLabels, setAllLabels) = React.useState(_ => getAllLabels(preCtxData))

    React.useEffect1(() => {
        setAllLabels(_ => getAllLabels(preCtxData))
        None
    }, [preCtxData.ctxV.ver])

    let labelSelectorTextFieldRef = React.useRef(Nullable.null)
    
    let actFocusLabelSelector = () => {
        switch labelSelectorTextFieldRef.current->Nullable.toOption {
            | None => ()
            | Some(domElem) => {
                let input = ReactDOM.domElementToObj(domElem)
                input["focus"]()
            }
        }
    }

    React.useEffect1(() => {
        actFocusLabelSelector()
        None
    }, [state.newLoc])

    let actNewLocUpdated = (newLoc:location) => {
        setState(setNewLoc(_, newLoc))
    }

    React.useEffect2(() => {
        if (!editMode) {
            actNewLocUpdated(loc)
        }
        None
    }, (editMode, loc))
    
    let actEditDone = () => {
        onEditDone(state.newLoc)
    }
    
    let actEditCancel = () => {
        onEditCancel()
    }

    let actLabelChanged = (newLabel:option<string>) => {
        newLabel->Option.forEach(newLabel => {
            actNewLocUpdated(makeLocation(~place=locationToPlaceStr(state.newLoc), ~label=newLabel))
        })
    }
    
    let rndButtons = () => {
        let saveBtn = rndIconButton(
            ~icon=<MM_Icons.Save/>, ~active=true,  ~onClick=actEditDone, ~title="Save")
        let cancelBtn = rndIconButton(
            ~icon=<MM_Icons.CancelOutlined/>, ~onClick=actEditCancel, ~title="Cancel", ~color=None)
        <Row spacing=1.> saveBtn cancelBtn </Row>
    }

    let rndPlaceSelector = () => {
        let curLabel = locationToLabel(state.newLoc)
        <FormControl size=#small>
            <InputLabel id="place-select-label">"Place"</InputLabel>
            <Select
                labelId="place-select-label"
                value={locationToPlaceStr(state.newLoc)}
                label="Place"
                onChange=evt2str(str => makeLocation(~place=str, ~label=curLabel)->actNewLocUpdated)
            >
                <MenuItem value=locationToPlaceStr(Before(""))>{React.string("before")}</MenuItem>
                <MenuItem value=locationToPlaceStr(After(""))>{React.string("after")}</MenuItem>
                <MenuItem value=locationToPlaceStr(Last)>{React.string("last")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndLabelSelector = () => {
        let privRndLabelSelector = (label:string) => {
            <AutocompleteVirtualized
                inputRef=ReactDOM.Ref.domRef(labelSelectorTextFieldRef)
                value={allLabels->Array.includes(label) ? Some(label) : None} 
                options=allLabels
                size=#small 
                onChange=actLabelChanged
                label="Label"
            />
        }
        switch state.newLoc {
            | Last => React.null
            | Before(label) => privRndLabelSelector(label)
            | After(label) => privRndLabelSelector(label)
        }
    }

    let rndLoc = () => {
        if (editMode) {
            <Row>
                {rndPlaceSelector()}
                {rndLabelSelector()}
                {rndButtons()}
            </Row>
        } else {
            let style = ReactDOM.Style.make(~padding="0px 4px", ())
            let title = "<left-click> to change"
            <Paper
                variant=#outlined 
                onClick=clickHnd2(
                    clickClbkMake(~act=onEditRequested),
                    clickClbkMake(~alt=true, ~act=onEditRequested),
                )
                style 
                title
            >
                {
                    switch loc {
                        | Before(label) => `before ${label}`
                        | After(label) => `after ${label}`
                        | Last => "last"
                    }->React.string
                }
            </Paper>
        }
    }

    rndLoc()
}