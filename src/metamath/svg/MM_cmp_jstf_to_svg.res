open Expln_2d
open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open MM_provers
open Local_storage_utils

@react.component
let make = (
    ~ctx:mmContext,
    ~args:array<expr>,
    ~label:string,
    ~asrt:expr,
    ~symColors1:option<Belt_HashMapString.t<string>>=?,
    ~symColors2:option<Belt_HashMapString.t<string>>=?,
    ~essOnly:bool=true,
) => {
    React.null
}