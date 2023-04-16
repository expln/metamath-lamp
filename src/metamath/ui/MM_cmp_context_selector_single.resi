open Expln_React_Modal

type mmFileSourceType = Local | Web
type webSource = {alias: string, url: string}
type mmFileSource = Local({fileName: string}) | Web(webSource)
type readInstr = ReadAll | StopBefore | StopAfter

@react.component
let make: (
    ~modalRef: modalRef,
    ~availableWebSrcs: array<webSource>,
    ~trustedUrls:array<string>,
    ~onUrlBecomesTrusted:string=>unit,
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
