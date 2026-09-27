open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_react_common

@module("./FileLoader") external loadFilePriv: (string, (int,int)=>unit, string=>unit, option<string>=>unit) => unit = "loadFile"

let getBasePath = (path:string):string => {
    let chIdx = ref(path->String.length - 1)
    while (chIdx.contents >= 0 && "/" !== path->String.charAt(chIdx.contents)) {
        chIdx := chIdx.contents - 1
    }
    if (chIdx.contents < 0) {
        path
    } else {
        path->String.substring(~start=0, ~end=chIdx.contents)
    }
}

let loadFile = (
    ~url:string,
    ~onProgress:option<(int,int)=>unit>=?,
    ~onReady:string=>unit,
    ~onError:option<option<string>=>unit>=?
) => {
    loadFilePriv(
        url,
        onProgress->Belt_Option.getWithDefault((_,_) => ()),
        onReady,
        onError->Belt_Option.getWithDefault(_ => ()),
    )
}

let loadFileWithProgress = (
    ~modalRef:modalRef,
    ~showWarning:bool,
    ~markUrlAsTrusted: React.ref<string=>unit>,
    ~url:string,
    ~progressText:string,
    ~onReady:string=>unit,
    ~onError:option<option<string>=>unit>=?,
    ~transformErrorMsg:option<option<string>=>string>=?,
    ~onTerminated:option<unit=>unit>=?
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
        openModal(modalRef, () => rndProgress(~text=progressText, ~pct=0.))->Promise.thenResolve(modalId => {
            updateModal( 
                modalRef, modalId, 
                () => rndProgress( ~text=progressText, ~pct=0., ~onTerminate=makeActTerminate(modalId) )
            )
            loadFile(
                ~url,
                ~onProgress = (loaded,total) => {
                    let pct = loaded->Belt_Int.toFloat /. total->Belt_Int.toFloat
                    updateModal( 
                        modalRef, modalId,
                        () => rndProgress( ~text=progressText, ~pct, ~onTerminate=makeActTerminate(modalId) )
                    )
                },
                ~onError = msg => {
                    closeModal(modalRef, modalId)
                    switch onError {
                        | Some(onError) => onError(msg)
                        | None => ()
                    }
                    switch transformErrorMsg {
                        | Some(transformErrorMsg) => {
                            openModal(modalRef, _ => React.null)->Promise.thenResolve(modalId => {
                                updateModal(modalRef, modalId, () => {
                                    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                                        <Col spacing=1.>
                                            { React.string(transformErrorMsg(msg)) }
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
                }
            )
        })->ignore
    }

    let trustedUrl:ref<option<string>> = ref(None)

    let actShowWarning = () => {
        let rec updateWarningDialogContent = (modalRef:modalRef, modalId:modalId):unit => {
            updateModal(modalRef, modalId, () => {
                <Paper style=ReactDOM.Style.make(~padding="10px", ())>
                    <Col spacing=1.>
                        <span style=ReactDOM.Style.make(~fontWeight="bolder", ())>
                            { React.string("Do you confirm downloading data from the below URL?") }
                        </span>
                        <span>
                            { React.string(url) }
                        </span>
                        <RadioGroup 
                            row=false 
                            value={
                                trustedUrl.contents->Option.map(trustedUrl => trustedUrl == url ? "1" : "2")
                                    ->Option.getOr("0")
                            }
                            onChange=evt2str(newValue => {
                                trustedUrl := switch newValue {
                                    | "0" => None
                                    | "1" => Some(url)
                                    | _ => Some(getBasePath(url) ++ "/*")
                                }
                                updateWarningDialogContent(modalRef, modalId)
                            })
                        >
                            {
                                ["0", "1", "2"]->Array.map(value => {
                                    let label = switch value {
                                        | "0" => "Always ask confirmation for this URL"
                                        | "1" => "Don't ask for this URL " ++ url
                                        | _ => "Don't ask for all URLs starting with " ++ getBasePath(url) ++ "/"
                                    }
                                    <FormControlLabel 
                                        key=value value label control={ <Radio/> } 
                                        style=ReactDOM.Style.make(~marginRight="30px", ())
                                    />
                                })->React.array
                            }
                        </RadioGroup>
                        <Row>
                            <Button 
                                variant=#contained
                                onClick={_ => {
                                    trustedUrl.contents->Option.forEach(markUrlAsTrusted.current)
                                    closeModal(modalRef, modalId)
                                    actDownloadFile()
                                }} 
                            > 
                                {React.string("Confirm")} 
                            </Button>
                            <Button 
                                variant=#outlined
                                onClick={_ => makeActTerminate(modalId)() } 
                            > 
                                {React.string("Cancel")} 
                            </Button>
                        </Row>
                    </Col>
                </Paper>
            })
        }
        openModal(modalRef, _ => React.null)->Promise.thenResolve(modalId => {
            updateWarningDialogContent(modalRef, modalId)
        })->ignore
    }

    if (showWarning) {
        actShowWarning()
    } else {
        actDownloadFile()
    }
}

type fileLoadResult =
    | Ok(string)
    | Error(option<string>)
    | TerminatedByUser

let loadFileWithProgressPromise = (
    ~modalRef:modalRef,
    ~showWarning:bool,
    ~markUrlAsTrusted: React.ref<string=>unit>,
    ~url:string,
    ~progressText:string,
    ~transformErrorMsg:option<option<string>=>string>=?
): promise<fileLoadResult> => {
    Promise.make((resolve,_) => {
        loadFileWithProgress(
            ~modalRef,
            ~showWarning,
            ~markUrlAsTrusted,
            ~url,
            ~progressText,
            ~onReady = loadedText => resolve(Ok(loadedText)),
            ~onError = msg => resolve(Error(msg)),
            ~transformErrorMsg?,
            ~onTerminated = () => resolve(TerminatedByUser)
        )
    })
}