open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Local_storage_utils
open Common

@react.component
let make = (
    ~showCheckbox:bool, ~onShowCheckboxChange:bool=>unit, 
    ~showLabel:bool, ~onShowLabelChange:bool=>unit, 
    ~showType:bool, ~onShowTypeChange:bool=>unit, 
    ~showJstf:bool, ~onShowJstfChange:bool=>unit, 
    ~inlineMode:bool, ~onInlineModeChange:bool=>unit, 
    ~scrollToolbar:bool, ~onScrollToolbarChange:bool=>unit, 
    ~smallBtns:bool, ~onSmallBtnsChange:bool=>unit, 
    ~onClose:unit=>unit,
) => {
    let (showCheckbox, setShowCheckbox) = React.useState(() => showCheckbox)
    let (showLabel, setShowLabel) = React.useState(() => showLabel)
    let (showType, setShowType) = React.useState(() => showType)
    let (showJstf, setShowJstf) = React.useState(() => showJstf)
    let (inlineMode, setInlineMode) = React.useState(() => inlineMode)
    let (scrollToolbar, setScrollToolbar) = React.useState(() => scrollToolbar)
    let (smallBtns, setSmallBtns) = React.useState(() => smallBtns)

    React.useEffect1(() => {
        onShowCheckboxChange(showCheckbox)
        None
    }, [showCheckbox])

    React.useEffect1(() => {
        onShowLabelChange(showLabel)
        None
    }, [showLabel])

    React.useEffect1(() => {
        onShowTypeChange(showType)
        None
    }, [showType])

    React.useEffect1(() => {
        onShowJstfChange(showJstf)
        None
    }, [showJstf])

    React.useEffect1(() => {
        onInlineModeChange(inlineMode)
        None
    }, [inlineMode])

    React.useEffect1(() => {
        onScrollToolbarChange(scrollToolbar)
        None
    }, [scrollToolbar])

    React.useEffect1(() => {
        onSmallBtnsChange(smallBtns)
        None
    }, [smallBtns])

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col>
            <span style=ReactDOM.Style.make(~fontWeight="bold", ~fontSize="1.1em", ())>
                { React.string("View Options") }
            </span>
            <Button onClick={_=>onClose()}> {React.string("Close")} </Button>
            <FormControlLabel
                control={
                    <Checkbox
                        checked=showCheckbox
                        onChange=evt2bool(b => setShowCheckbox(_ => b))
                    />
                }
                label="Checkbox"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=showLabel
                        onChange=evt2bool(b => setShowLabel(_ => b))
                    />
                }
                label="Label"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=showType
                        onChange=evt2bool(b => setShowType(_ => b))
                    />
                }
                label="H/P/G"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=showJstf
                        onChange=evt2bool(b => setShowJstf(_ => b))
                    />
                }
                label="Justification"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=inlineMode
                        onChange=evt2bool(b => setInlineMode(_ => b))
                    />
                }
                label="Compact mode"
            />
            <FormControlLabel
                style=ReactDOM.Style.make(~display="none", ())
                control={
                    <Checkbox
                        checked=scrollToolbar
                        onChange=evt2bool(b => setScrollToolbar(_ => b))
                    />
                }
                label="Scroll toolbar"
            />
            <FormControlLabel
                control={
                    <Checkbox
                        checked=smallBtns
                        onChange=evt2bool(b => setSmallBtns(_ => b))
                    />
                }
                label="Small buttons"
            />
            <Button onClick={_=>onClose()}> {React.string("Close")} </Button>
        </Col>
    </Paper>
}