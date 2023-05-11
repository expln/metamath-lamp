open Expln_React_Modal

let loadFile: (
    ~url: string,
    ~onProgress: (int, int) => unit=?,
    ~onReady: string => unit,
    ~onError: option<string> => unit=?,
    unit,
) => unit

let loadFileWithProgress: (
    ~modalRef:modalRef,
    ~showWarning:bool,
    ~onUrlBecomesTrusted:option<string=>unit>,
    ~url:string,
    ~progressText:string,
    ~onReady:string=>unit,
    ~onError:option<string>=>unit =?,
    ~transformErrorMsg:option<string>=>string =?,
    ~onTerminated:unit=>unit=?,
    ()
) => unit