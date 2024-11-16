open Expln_React_common
open Expln_React_Mui
open MM_wrk_settings



type state = {
    values: bottomUpProverDefaults,
    searchDepthStr:string,
}

let makeInitialState = (
    ~initSettings: bottomUpProverDefaults,
) => {
    {
        values: initSettings,
        searchDepthStr: initSettings.searchDepth->Int.toString
    }
}

let updateValues = (st:state, update: bottomUpProverDefaults => bottomUpProverDefaults):state => {
    {...st, values:update(st.values)}
}

let validateState = (st:state):state => {
    let st = if (st.values.searchDepth < 1 || 10000 < st.values.searchDepth) {
        st->updateValues(v => {...v, searchDepth:1})
    } else {
        st
    }
    let st = switch MM_provers.lengthRestrictFromStr(st.values.lengthRestrict) {
        | None => st->updateValues(v => {...v, lengthRestrict:MM_provers.lengthRestrictToStr(MM_provers.Less)})
        | Some(_) => st
    }
    let st = if (st.values.debugLevel < 0 || 1 < st.values.debugLevel) {
        st->updateValues(v => {...v, debugLevel:0})
    } else {
        st
    }
    st
}

let setSearchDepthStr = (st:state, newVal:string) => {
    {...st, searchDepthStr:newVal}
}

let setSearchDepth = (st:state, newVal:int) => {
    st->updateValues(v => {...v, searchDepth:newVal})->validateState
}

let setLengthRestrict = (st:state, newVal:string) => {
    st->updateValues(v => {...v, lengthRestrict:newVal})->validateState
}

let setAllowNewDisjForExistingVars = (st:state, newVal:bool) => {
    st->updateValues(v => {...v, allowNewDisjForExistingVars:newVal})->validateState
}

let setAllowNewStmts = (st:state, newVal:bool) => {
    st->updateValues(v => {...v, allowNewStmts:newVal})->validateState
}

let setAllowNewVars = (st:state, newVal:bool) => {
    st->updateValues(v => {...v, allowNewVars:newVal})->validateState
}

let setDebugLevel = (st:state, newVal:int) => {
    st->updateValues(v => {...v, debugLevel:newVal})->validateState
}

@react.component
let make = (
    ~initSettings:bottomUpProverDefaults,
    ~onChange:bottomUpProverDefaults=>unit,
) => {
    let (state, setStatePriv) = React.useState(() => makeInitialState(~initSettings))

    let setState = (update:state=>state):unit => {
        let st = update(state)
        let st = st->setSearchDepth(st.searchDepthStr->Int.fromString->Option.getOr(initSettings.searchDepth))
        let st = st->validateState
        setStatePriv(_ => st)
        onChange(st.values)
    }

    let rndLengthRestrictSelector = (value:string) => {
        <FormControl size=#small>
            <InputLabel id="length-restrict-select-label">"Statement length restriction"</InputLabel>
            <Select
                sx={"width": 190}
                labelId="length-restrict-select-label"
                value
                label="Statement length restriction"
                onChange=evt2str(str => setState(setLengthRestrict(_, str)))
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
                onChange=evt2str(str => setState(setDebugLevel(_,Int.fromString(str)->Option.getOr(0))))
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
                    value={state.searchDepthStr}
                    onChange=evt2str(str=>setState(setSearchDepthStr(_,str)))
                />
                {rndLengthRestrictSelector(state.values.lengthRestrict)}
            </Row>
            <Row>
                <FormControlLabel
                    control={
                        <Checkbox
                            checked=state.values.allowNewDisjForExistingVars
                            onChange=evt2bool(b => setState(setAllowNewDisjForExistingVars(_,b)))
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
                            checked=state.values.allowNewStmts
                            onChange=evt2bool(b => setState(setAllowNewStmts(_,b)))
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
                            checked=state.values.allowNewVars
                            onChange=evt2bool(b => setState(setAllowNewVars(_,b)))
                        />
                    }
                    label="Allow new variables"
                    disabled={!state.values.allowNewStmts}
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
                {rndDebugParam(state.values.debugLevel)}
            </Row>
        </Col>
    }

    rndParams()
}
