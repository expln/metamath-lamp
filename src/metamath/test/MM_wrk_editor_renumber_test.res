open Expln_test
open MM_parser
open MM_context
open MM_wrk_editor
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open MM_wrk_pre_ctx_data

open MM_wrk_editor_test // for createEditorState, etc.

let demo0 = "./src/metamath/test/resources/demo0._mm"
describe("renumbering", _ => {
    it("accurately renumbers simple proof", _ => {
        //given
        // it("sets expr and jstf for each provable when there are no errors")
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"71", cont:strToCont("|- 0 + 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"72", cont:strToCont("|- t term", ()),
            jstfText: "71 hyp1 : mp"
        })

        //when
        let st = prepareEditorForUnification(st)
        switch renumberSteps(st) {
        | Error(msg) => assertEqMsg(1, 0, msg) // TODO
        | Ok(st) => {
            //then
            assertEqMsg(st.stmts[2].id, pr1Id, "old 71 is the third")
            assertEqMsg(st.stmts[2].label, "1", "Statement is 1")
            assertEqMsg(st.stmts[3].label, "2", "Statement is 2")
            assertEqMsg(st.stmts[3].jstfText, "1 hyp1:mp", "Correct jstfText")
        }}
        assertEq(1, 1)
    })
})
