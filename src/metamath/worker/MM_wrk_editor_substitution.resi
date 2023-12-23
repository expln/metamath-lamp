open MM_wrk_editor
open MM_context

let findPossibleSubs: (editorState, expr, expr, bool) => result<array<wrkSubs>,string>
let applySubstitutionForEditor: (editorState, wrkSubs) => editorState
let substitute: (editorState, ~what:string, ~with_:string) => result<editorState,string>