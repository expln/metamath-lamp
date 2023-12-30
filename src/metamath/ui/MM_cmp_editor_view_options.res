open Expln_React_common
open Expln_React_Mui
open MM_react_common

let nonDigitPattern = %re("/\D/g")

@react.component
let make = (
    ~showCheckbox:bool, ~onShowCheckboxChange:bool=>unit, 
    ~showLabel:bool, ~onShowLabelChange:bool=>unit, 
    ~showType:bool, ~onShowTypeChange:bool=>unit, 
    ~showJstf:bool, ~onShowJstfChange:bool=>unit, 
    ~inlineMode:bool, ~onInlineModeChange:bool=>unit, 
    ~smallBtns:bool, ~onSmallBtnsChange:bool=>unit, 
    ~stepsPerPage:int, ~onStepsPerPageChange:int=>unit, 
    ~onClose:unit=>unit,
) => {
    let (showCheckbox, setShowCheckbox) = React.useState(() => showCheckbox)
    let (showLabel, setShowLabel) = React.useState(() => showLabel)
    let (showType, setShowType) = React.useState(() => showType)
    let (showJstf, setShowJstf) = React.useState(() => showJstf)
    let (inlineMode, setInlineMode) = React.useState(() => inlineMode)
    let (smallBtns, setSmallBtns) = React.useState(() => smallBtns)

    let (curStepsPerPage, setCurStepsPerPage) = React.useState(() => stepsPerPage)
    let (stepsPerPageText, setStepsPerPageText) = React.useState(() => None)
    let stepsPerPageTextEffective = stepsPerPageText->Belt_Option.getWithDefault(curStepsPerPage->Belt_Int.toString)

    let actChangeStepsPerPage = () => {
        switch stepsPerPageTextEffective->Belt_Int.fromString {
            | None => ()
            | Some(newStepsPerPage) => {
                onStepsPerPageChange(newStepsPerPage)
                setCurStepsPerPage(_ => newStepsPerPage)
                setStepsPerPageText(_ => None)
            }
        }
    }

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
                label="Step type"
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
                control={
                    <Checkbox
                        checked=smallBtns
                        onChange=evt2bool(b => setSmallBtns(_ => b))
                    />
                }
                label="Small buttons"
            />
            <Row>
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="110px", ())
                    label="Steps per page" 
                    value=stepsPerPageTextEffective
                    autoFocus=true
                    onChange=evt2str(newStepsPerPage => {
                        setStepsPerPageText(_ => Some(newStepsPerPage->Js.String2.replaceByRe(nonDigitPattern, "")))
                    })
                    onKeyDown=kbrdHnd2(
                        kbrdClbkMake(~key=keyEnter, ~act=actChangeStepsPerPage, ()),
                        kbrdClbkMake(~key=keyEsc, ~act=onClose, ()),
                    )
                />
                <IconButton
                    disabled={stepsPerPageText->Belt_Option.isNone}
                    onClick={_=>actChangeStepsPerPage()}
                    color="primary"
                > 
                    <MM_Icons.Save/>
                </IconButton>
            </Row>
            <Button onClick={_=>onClose()}> {React.string("Close")} </Button>
        </Col>
    </Paper>
}