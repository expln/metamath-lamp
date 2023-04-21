open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_react_common

@module("./FileLoader") external loadFilePriv: (string, (int,int)=>unit, string=>unit, unit=>unit) => unit = "loadFile"

let loadFile = (
    ~url:string,
    ~onProgress:option<(int,int)=>unit>=?,
    ~onReady:string=>unit,
    ~onError:option<unit=>unit>=?,
    ()
) => {
    loadFilePriv(
        url,
        onProgress->Belt_Option.getWithDefault((_,_) => ()),
        onReady,
        onError->Belt_Option.getWithDefault(() => ()),
    )
}

let loadFileWithProgress = (
    ~modalRef:modalRef,
    ~showWarning:bool,
    ~onUrlBecomesTrusted:string=>unit,
    ~url:string,
    ~progressText:string,
    ~onReady:string=>unit,
    ~onError:option<unit=>unit>=?,
    ~errorMsg:option<string>=?,
    ~onTerminated:option<unit=>unit>=?,
    ()
):unit => {

    let isTerminated = ref(false)

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            isTerminated.contents = true
            switch onTerminated {
                | None => ()
                | Some(onTerminated) => onTerminated()
            }
            closeModal(modalRef, modalId)
        }
    }

    let actDownloadFile = () => {
        openModal(modalRef, () => rndProgress(~text=progressText, ~pct=0., ()))->promiseMap(modalId => {
            updateModal( 
                modalRef, modalId, 
                () => rndProgress( ~text=progressText, ~pct=0., ~onTerminate=makeActTerminate(modalId), () )
            )
            loadFile(
                ~url,
                ~onProgress = (loaded,total) => {
                    let pct = loaded->Belt_Int.toFloat /. total->Belt_Int.toFloat
                    updateModal( 
                        modalRef, modalId,
                        () => rndProgress( ~text=progressText, ~pct, ~onTerminate=makeActTerminate(modalId), () )
                    )
                },
                ~onError = () => {
                    closeModal(modalRef, modalId)
                    switch onError {
                        | Some(onError) => onError()
                        | None => ()
                    }
                    switch errorMsg {
                        | Some(errorMsg) => {
                            openModal(modalRef, _ => React.null)->promiseMap(modalId => {
                                updateModal(modalRef, modalId, () => {
                                    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                                        <Col spacing=1.>
                                            { React.string(errorMsg) }
                                            <Button onClick={_ => closeModal(modalRef, modalId) } variant=#contained> 
                                                {React.string("Ok")} 
                                            </Button>
                                        </Col>
                                    </Paper>
                                })
                            })->ignore
                        }
                        | None => ()
                    }
                },
                ~onReady = text => {
                    if (!isTerminated.contents) {
                        closeModal(modalRef, modalId)
                        onReady(text)
                    }
                },
                ()
            )
        })->ignore
    }

    let dontAskAgain = ref(false)

    let actShowWarning = () => {
        openModal(modalRef, _ => React.null)->promiseMap(modalId => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        <span style=ReactDOM.Style.make(~fontWeight="bolder", ())>
                            { React.string("Do you confirm downloading data from the below URL?") }
                        </span>
                        <span>
                            { React.string(url) }
                        </span>
                        <FormControlLabel
                            control={
                                <Checkbox
                                    onChange=evt2bool(checked => dontAskAgain.contents = checked)
                                />
                            }
                            label="don't ask for this URL"
                        />
                        <Row>
                            <Button 
                                variant=#contained
                                onClick={_ => {
                                    if (dontAskAgain.contents) {
                                        onUrlBecomesTrusted(url)
                                    }
                                    closeModal(modalRef, modalId)
                                    actDownloadFile()
                                }} 
                            > 
                                {React.string("Confirm")} 
                            </Button>
                            <Button 
                                variant=#outlined
                                onClick={_ => closeModal(modalRef, modalId) } 
                            > 
                                {React.string("Cancel")} 
                            </Button>
                        </Row>
                    </Col>
                </Paper>
            })
        })->ignore
    }

    if (showWarning) {
        actShowWarning()
    } else {
        actDownloadFile()
    }
}