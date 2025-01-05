open Expln_React_common
open Expln_React_Mui
open MM_wrk_settings

let validate = (values:bottomUpProverDefaults):bottomUpProverDefaults => {
    let values = if (values.searchDepth < 1 || 10000 < values.searchDepth) {
        {...values, searchDepth:1}
    } else {
        values
    }
    let values = switch MM_bottom_up_prover_params.lengthRestrictFromStr(values.lengthRestrict) {
        | None => {
            {...values, lengthRestrict:MM_bottom_up_prover_params.lengthRestrictToStr(MM_bottom_up_prover_params.Less)}
        }
        | Some(_) => values
    }
    let values = if (values.debugLevel < 0 || 1 < values.debugLevel) {
        {...values, debugLevel:0}
    } else {
        values
    }
    values
}

let setSearchDepth = (values:bottomUpProverDefaults, newVal:int) => {
    {...values, searchDepth:newVal}
}

let setLengthRestrict = (values:bottomUpProverDefaults, newVal:string) => {
    {...values, lengthRestrict:newVal}
}

let setAllowNewDisjForExistingVars = (values:bottomUpProverDefaults, newVal:bool) => {
    {...values, allowNewDisjForExistingVars:newVal}
}

let setAllowNewStmts = (values:bottomUpProverDefaults, newVal:bool) => {
    {...values, allowNewStmts:newVal}
}

let setAllowNewVars = (values:bottomUpProverDefaults, newVal:bool) => {
    {...values, allowNewVars:newVal}
}

let setDebugLevel = (values:bottomUpProverDefaults, newVal:int) => {
    {...values, debugLevel:newVal}
}

@react.component
let make = (
    ~settings:bottomUpProverDefaults,
    ~onChange:bottomUpProverDefaults=>unit,
) => {
    let (searchDepthStr, setSearchDepthStrPriv) = React.useState(() => "")

    React.useEffect1(() => {
        setSearchDepthStrPriv(_ => settings.searchDepth->Int.toString)
        None
    }, [settings.searchDepth])

    let updateSettings = (update:bottomUpProverDefaults=>bottomUpProverDefaults):unit => {
        onChange(settings->update->validate)
    }

    let setSearchDepthStr = (newSearchDepthStr:string):unit => {
        setSearchDepthStrPriv(_=>newSearchDepthStr)
        switch newSearchDepthStr->Int.fromString {
            | None => ()
            | Some(searchDepth) => updateSettings(setSearchDepth(_, searchDepth))
        }
    }

    let rndLengthRestrictSelector = (value:string) => {
        <FormControl size=#small>
            <InputLabel id="length-restrict-select-label">"Statement length restriction"</InputLabel>
            <Select
                sx={"width": 190}
                labelId="length-restrict-select-label"
                value
                label="Statement length restriction"
                onChange=evt2str(str => updateSettings(setLengthRestrict(_, str)))
            >
                <MenuItem value="No">{React.string("Unrestricted")}</MenuItem>
                <MenuItem value="LessEq">{React.string("LessEq")}</MenuItem>
                <MenuItem value="Less">{React.string("Less")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndDebugParam = (debugLevel:int) => {
        <Row alignItems=#center>
            {React.string("Logging level")}
            <RadioGroup
                row=true
                value={debugLevel->Belt_Int.toString}
                onChange=evt2str(str => updateSettings(setDebugLevel(_,Int.fromString(str)->Option.getOr(0))))
            >
                <FormControlLabel value="0" control={ <Radio/> } label="0" />
                <FormControlLabel value="1" control={ <Radio/> } label="1" />
            </RadioGroup>
        </Row>
    }

    let rndParams = () => {
        <Col>
            <Row>
                <TextField 
                    label="Search depth"
                    size=#small
                    style=ReactDOM.Style.make(~width="100px", ())
                    autoFocus=true
                    value={searchDepthStr}
                    onChange=evt2str(setSearchDepthStr)
                />
                {rndLengthRestrictSelector(settings.lengthRestrict)}
            </Row>
            <Row>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=settings.allowNewDisjForExistingVars
                            onChange=evt2bool(b => updateSettings(setAllowNewDisjForExistingVars(_,b)))
                        />
                    }
                    label="Allow new disjoints"
                    style=ReactDOM.Style.make(
                        ~border="solid 1px lightgrey", 
                        ~borderRadius="7px", 
                        ~paddingRight="10px",
                        ~marginTop="-2px",
                        ~marginLeft="2px",
                        ()
                    )
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=settings.allowNewStmts
                            onChange=evt2bool(b => updateSettings(setAllowNewStmts(_,b)))
                        />
                    }
                    label="Allow new steps"
                    style=ReactDOM.Style.make(
                        ~border="solid 1px lightgrey", 
                        ~borderRadius="7px", 
                        ~paddingRight="10px",
                        ~marginTop="-2px",
                        ~marginLeft="2px",
                        ()
                    )
                />
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=settings.allowNewVars
                            onChange=evt2bool(b => updateSettings(setAllowNewVars(_,b)))
                        />
                    }
                    label="Allow new variables"
                    disabled={!settings.allowNewStmts}
                    style=ReactDOM.Style.make(
                        ~border="solid 1px lightgrey", 
                        ~borderRadius="7px", 
                        ~paddingRight="10px",
                        ~marginTop="-2px",
                        ()
                    )
                />
            </Row>
            <Row>
                {rndDebugParam(settings.debugLevel)}
            </Row>
        </Col>
    }

    rndParams()
}
