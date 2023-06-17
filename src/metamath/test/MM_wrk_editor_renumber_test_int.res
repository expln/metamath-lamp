open Expln_test
open MM_int_test_utils
open MM_int_test_editor_methods

describe("MM_wrk_editor integration tests: step renumbering", _ => {
    it("simple case", _ => {
        //given

        //setting current test directory to ./src/metamath/test/resources/int-test-data/renumbering
        //some of the following functions will use paths relative to this directory
        setTestDataDir("renumbering")

        //loading context from "simple_case._mm" and creating initial state from "simple_case_init_state.json"
        let st = createEditorState( 
            ~mmFilePath="./src/metamath/test/resources/int-test-data/renumbering/simple_case._mm", 
            ~editorState="simple_case_init_state",
            ()
        )

        //comparing actual initial state with expected one to make sure the test preconditions are valid
        //the actual state is taken from "st" variable
        //the expected state is read from "simple_case_given.txt" file
        //if this assertion fails, 
        //  it will be easy to compare "simple_case_given.txt" and "simple_case_given.txt.actual" files 
        //  to understand what is the difference
        assertEditorState(st, "simple_case_given")

        //when/then
        switch MM_wrk_editor.renumberSteps(st) {
            | Error(msg) => failMsg(msg)
            | Ok(st) => assertEditorState(st, "simple_case_then")
        }
    })

    it_skip("bad prefix", _ => {
        //given
        setTestDataDir("renumbering")
        let st = createEditorState( 
            ~mmFilePath="./src/metamath/test/resources/int-test-data/renumbering/bad_prefix_case._mm", 
            ~editorState="bad_prefix_init_state",
            ()
        )
        assertEditorState(st, "bad_prefix_given")

        //when/then
        switch MM_wrk_editor.renumberSteps(st) {
            | Error(msg) => failMsg(msg)
            | Ok(st) => assertEditorState(st, "bad_prefix_then")
        }
    })
})