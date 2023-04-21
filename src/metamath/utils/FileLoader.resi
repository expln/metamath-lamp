open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open MM_react_common

let loadFile: (
    ~url: string,
    ~onProgress: (int, int) => unit=?,
    ~onReady: string => unit,
    ~onError: unit => unit=?,
    unit,
) => unit

let loadFileWithProgress: (
    ~modalRef:modalRef,
    ~showWarning:bool,
    ~onUrlBecomesTrusted:string=>unit,
    ~url:string,
    ~progressText:string,
    ~onReady:string=>unit,
    ~onError:string=>unit=?,
    ~errorMsg:string=?,
    ~onTerminated:unit=>unit=?,
    ()
) => unit