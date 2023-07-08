open Expln_React_common
open Expln_utils_promise

type modalId

type modalRef
let useModalRef: unit => modalRef

let openModal: (modalRef, unit => reElem) => promise<modalId>
let openModalFullScreen: (modalRef, unit => reElem) => promise<modalId>
let updateModal: (modalRef, modalId, unit => reElem) => unit
let closeModal: (modalRef, modalId) => unit

@react.component
let make: (~modalRef:modalRef) => React.element
