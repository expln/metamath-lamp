let getBasePath: string => string

let loadFile: (
    ~url: string,
    ~onProgress: (int, int) => unit=?,
    ~onReady: string => unit,
    ~onError: option<string> => unit=?,
) => unit

let loadFileWithProgress: (
    ~modalRef:Expln_React_Modal.modalRef,
    ~showWarning:bool,
    ~markUrlAsTrusted: React.ref<string=>unit>,
    ~url:string,
    ~progressText:string,
    ~onReady:string=>unit,
    ~onError:option<string>=>unit =?,
    ~transformErrorMsg:option<string>=>string =?,
    ~onTerminated:unit=>unit=?
) => unit

type fileLoadResult =
    | Ok(string)
    | Error(option<string>)
    | TerminatedByUser

let loadFileWithProgressPromise: (
    ~modalRef:Expln_React_Modal.modalRef,
    ~showWarning:bool,
    ~markUrlAsTrusted: React.ref<string=>unit>,
    ~url:string,
    ~progressText:string,
    ~transformErrorMsg:option<string>=>string =?
) => promise<fileLoadResult>