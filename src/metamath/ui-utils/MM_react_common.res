open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise

@val external navigator: {..} = "navigator"

let copyToClipboard = (text:string) => {
    navigator["clipboard"]["writeText"](. text)
}

let rndProgress = (~text:string, ~pct:option<float>=?, ~onTerminate:option<unit=>unit>=?, ()) => {
    <Paper style=ReactDOM.Style.make(~padding=onTerminate->Belt.Option.map(_=>"5px")->Belt.Option.getWithDefault("10px"), ())>
        <Row alignItems=#center spacing=1.>
            <span style=ReactDOM.Style.make(~paddingLeft="10px", ())>
                {
                    switch pct {
                        | Some(pct) => `${text}: ${(pct *. 100.)->Js.Math.round->Belt.Float.toInt->Belt_Int.toString}%`
                        | None => text
                    }->React.string
                }
            </span>
            {
                switch onTerminate {
                    | None => React.null
                    | Some(onTerminate) => {
                        <IconButton onClick={_ => onTerminate()}>
                            <MM_Icons.CancelOutlined/>
                        </IconButton>
                    }
                }
            }
        </Row>
    </Paper>
}

let rndInfoDialog = (~text:string, ~onOk:unit=>unit) => {
    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.>
            <span>
                {text->React.string}
            </span>
            <Button onClick={_=>onOk()} variant=#contained >
                {React.string("Ok")}
            </Button>
        </Col>
    </Paper>
}

let openInfoDialog = (~modalRef:modalRef, ~text:string, ~onOk:option<unit=>unit>=?, ()) => {
    openModal(modalRef, _ => React.null)->promiseMap(modalId => {
        updateModal(modalRef, modalId, () => {
            rndInfoDialog(~text, ~onOk = () => {
                closeModal(modalRef, modalId)
                onOk->Belt_Option.forEach(clbk => clbk())
            })
        })
    })->ignore
}

let kbrdHnd = (
    ~onCtrlEnter: option<() => unit>=?,
    ~onEnter: option<() => unit>=?,
    ~onEsc: option<() => unit>=?,
    ()
):(ReactEvent.Keyboard.t => unit) => {
    (kbrdEvt:ReactEvent.Keyboard.t) => {
        let isAlt = kbrdEvt->ReactEvent.Keyboard.altKey
        let isCtrl = kbrdEvt->ReactEvent.Keyboard.ctrlKey
        let isShift = kbrdEvt->ReactEvent.Keyboard.shiftKey
        let keyCode = kbrdEvt->ReactEvent.Keyboard.keyCode

        onCtrlEnter->Belt.Option.forEach(onCtrlEnter => {
            if (!isAlt && isCtrl && !isShift && keyCode == 13) {
                onCtrlEnter()
            }
        })
        
        onEnter->Belt.Option.forEach(onEnter => {
            if (!isAlt && !isCtrl && !isShift && keyCode == 13) {
                onEnter()
            }
        })
        
        onEsc->Belt.Option.forEach(onEsc => {
            if (!isAlt && !isCtrl && !isShift && keyCode == 27) {
                onEsc()
            }
        })
    }
}