open MM_wrk_editor

type editorHistory

let editorHistMake: (~initState:editorState, ~maxLength:int) => editorHistory
let editorHistAddSnapshot: (editorHistory, editorState) => editorHistory
let editorHistSetMaxLength: (editorHistory, int) => editorHistory
let editorHistIsEmpty: editorHistory => bool
let editorHistLength: editorHistory => int
let editorHistGetSnapshotPreview: (editorHistory, int, editorState) => result<editorState,string>
let restoreEditorStateFromSnapshot: (editorState, editorHistory, int) => result<editorState,string>
// let editorHistClear: editorHistory => editorHistory
let editorHistToString: editorHistory => string
let editorHistFromString: string => result<editorHistory,string>

let editorHistToStringExtended: editorHistory => string


// --------- Tests for private types and functions ---------------------------------------

let mm_editor_history__test_findDiff: () => unit
let mm_editor_history__test_applyDiff: () => unit