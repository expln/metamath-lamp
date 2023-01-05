open MM_parser
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_wrk_settings
open MM_react_common

let allColors = [
    "#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4",
    "#469990", "#dcbeff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#a9a9a9",
    "#000000"
]

let createDefaultSettings = () => {
    {
        parens: "( ) [ ] { }",
        parensIsValid: true,
        types:  [ "wff",     "term",    "setvar",  "class"],
        colors: [ "#4363d8", "#000000", "#e6194B", "#f032e6"],
    }
}

let settingsSaveToLocStor = (settings, key) => {
    Dom_storage2.localStorage->Dom_storage2.setItem(key, Expln_utils_common.stringify(settings))
}

let settingsReadFromLocStor = (key:string):option<settings> => {
    switch Dom_storage2.localStorage->Dom_storage2.getItem(key) {
        | None => None
        | Some(settingsLocStorStr) => {
            open Expln_utils_jsonParse
            let parseResult = parseObj(settingsLocStorStr, d=>{
                {
                    parens: d->str("parens"),
                    parensIsValid: d->bool("parensIsValid"),
                    types: d->arr("types", asStr),
                    colors: d->arr("colors", asStr),
                }
            })
            switch parseResult {
                | Error(_) => None
                | Ok(res) => Some(res)
            }
        }
    }
}

let getParensAsArray = st => {
    st.parens->getSpaceSeparatedValuesAsArray
}

let getParens: settings => array<string> = st => st->getParensAsArray

let getCorrectedTypesAndColors = st => {
    let correctedTypes = []
    let correctedColors = []
    st.types
        ->Js_array2.mapi((typ,i) => (typ->Js_string2.trim, st.colors[i]->Js_string2.trim))
        ->Js_array2.filter(((t,c)) => t != "" && c != "")
        ->Js_array2.forEach(((t,c)) => {
            correctedTypes->Js_array2.push(t)->ignore
            correctedColors->Js_array2.push(c)->ignore
        })
    (correctedTypes, correctedColors)
}

let getTypeColors: settings => Belt_MapString.t<string> = st => {
    let (types, colors) = getCorrectedTypesAndColors(st)
    types
        ->Js_array2.mapi((typ,i) => (typ, colors[i]))
        ->Belt_MapString.fromArray
}

let setParens = (st, str) => {
    {
        ...st,
        parens: str
    }
}

let getNotUsedColors = st => {
    let usedColors = Belt_SetString.fromArray(st.colors)
    allColors->Js_array2.filter(c => !(usedColors->Belt_SetString.has(c)))
}

let addTypeColor = st => {
    {
        ...st,
        types: st.types->Js_array2.concat([""]),
        colors: st.colors->Js_array2.concat([getNotUsedColors(st)->Belt_Array.get(0)->Belt_Option.getWithDefault("")]),
    }
}

let changeType = (st,idx,newType) => {
    {
        ...st,
        types: st.types->Js_array2.mapi((e,i) => if i == idx {newType} else {e}),
    }
}

let changeColor = (st,idx,newColor) => {
    {
        ...st,
        colors: st.colors->Js_array2.mapi((e,i) => if i == idx {newColor} else {e}),
    }
}

let correctAndValidate = st => {
    let parensArr = st->getParensAsArray
    let (types, colors) = getCorrectedTypesAndColors(st)
    {
        parens: parensArr->Js_array2.joinWith(" "),
        parensIsValid: parensArr->Js_array2.length->mod(_,2) == 0,
        types,
        colors,
    }
}

let isValid = st => {
    st.parensIsValid
}

let eqState = (st1, st2) => {
    getParensAsArray(st1) == getParensAsArray(st2) && 
        st1.types == st2.types && st1.colors == st2.colors
}

let stateLocStorKey = "settings"

@react.component
let make = (~modalRef:modalRef, ~ctx:mmContext, ~initialSettings:settings, ~onChange: settings => unit) => {
    let (prevState, setPrevState) = React.useState(_ => initialSettings)
    let (state, setStatePriv) = React.useState(_ => initialSettings)

    let setState = update => {
        setStatePriv(prev => {
            let new = update(prev)
            new->settingsSaveToLocStor(stateLocStorKey)
            new
        })
    }

    let onParensChange = newParens => {
        setState(st=>{
            let st = st->setParens(newParens)
            if (st->isValid) {
                st
            } else {
                st->correctAndValidate
            }
        })
    }

    let rndFindParensProgress = (pct) => {
        rndProgress(~text=`Searching parentheses`, ~pct)
    }

    let syncParens = () => {
        openModal(modalRef, _ => rndFindParensProgress(0.))->promiseMap(modalId => {
            MM_wrk_FindParens.beginFindParens(
                ~ctx,
                ~onProgress = pct => updateModal(modalRef, modalId, () => rndFindParensProgress(pct)),
                ~onDone = parens => {
                    onParensChange(parens)
                    closeModal(modalRef, modalId)
                }
            )
        })->ignore
    }

    let applyChanges = () => {
        let st = correctAndValidate(state)
        setState(_ => st)
        if (st->isValid) {
            setPrevState(_ => st)
            onChange(st)
        }
    }
    
    let onTypeColorAdd = () => {
        setState(_ => addTypeColor(state))
    }
    let onTypeChange = (idx,newType) => {
        setState(changeType(_,idx,newType))
    }
    let onColorChange = (idx,newColor) => {
        setState(changeColor(_,idx,newColor))
    }

    let discardChanges = () => {
        setState(_ => prevState)
    }

    <Col spacing=3. style=ReactDOM.Style.make(~margin="30px", ())>
        <Row alignItems=#center>
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="400px", ())
                label="Parentheses" 
                value=state.parens 
                onChange=evt2str(onParensChange)
                error={!state.parensIsValid}
                title="Parentheses are used to speed up finding of substitutions"
            />
            <span title="Determine parentheses from the loaded MM file">
                <IconButton onClick={_ => syncParens()}>
                    <MM_Icons.Sync/>
                </IconButton>
            </span>
        </Row>
        // <MM_cmp_colors
        //     types=state.types
        //     colors=state.colors
        //     availableColors=allColors
        //     onAdd=onTypeColorAdd
        //     onTypeChange
        //     onColorChange
        // />
        {
            if (!eqState(prevState, state)) {
                <Row spacing=3. >
                    <Button disabled={!isValid(state)} onClick={_=>applyChanges()} variant=#contained>
                        {React.string("Apply changes")}
                    </Button>
                    <Button onClick={_ => discardChanges()}>
                        {React.string("Discard changes")}
                    </Button>
                </Row>
            } else {
                React.null
            }
        }
    </Col>
}