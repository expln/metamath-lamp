open MM_context
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_wrk_editor
open MM_wrk_editor_substitution
open MM_wrk_settings
open MM_wrk_unify
open Expln_utils_promise
open MM_react_common
open MM_statements_dto
open MM_wrk_editor_json
open MM_proof_tree
open MM_provers
open Local_storage_utils
open Common
open MM_wrk_pre_ctx_data
open MM_editor_history
open MM_proof_tree_dto

type tabProps = {
    id: Expln_React_UseTabs.tabId,
    label: string,
}

@react.component
let make = (
    ~modalRef:modalRef, 
    ~tabs: array<tabProps>
) => {
    let rndTabControls = (tab:tabProps) => {
        <Row key=tab.id>
            <Paper style=ReactDOM.Style.make(~padding="5px", ())>
                {tab.label->React.string}
            </Paper>
        </Row>
    }
    <Col spacing=1. style=ReactDOM.Style.make(~margin="10px", ())>
        {
            tabs->Array.map(rndTabControls)->React.array
        }
    </Col>
}