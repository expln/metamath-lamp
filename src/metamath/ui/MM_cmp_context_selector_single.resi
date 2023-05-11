open Expln_React_Modal
open MM_wrk_editor

@react.component
let make: (
    ~modalRef: modalRef,
    ~availableWebSrcs: array<webSource>,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:option<string=>unit>,
    ~srcType: mmFileSourceType,
    ~onSrcTypeChange: mmFileSourceType => unit,
    ~fileSrc: option<mmFileSource>,
    ~onFileChange: (mmFileSource, string) => unit,
    ~parseError: option<string>,
    ~readInstr: readInstr,
    ~onReadInstrChange: readInstr => unit,
    ~label: option<string>,
    ~onLabelChange: option<string> => unit,
    ~allLabels: array<string>,
    ~renderDeleteButton: bool,
    ~onDelete: unit => unit,
) => React.element
