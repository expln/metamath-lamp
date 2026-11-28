open Expln_test
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_int_test_editor_methods

let demo0Path = "./src/metamath/test/resources/demo0._mm"

/*
* Searches assertions in the context of the editor and returns the labels of the found assertions.
* */
let searchLabels = (
    st:editorState,
    ~label:string="",
    ~strictLabel:option<string>=?,
    ~pattern:string="",
):array<string> => {
    doSearchAssertions(
        ~allFramesInDeclarationOrder=st.preCtxData.frms->MM_substitution.frmsSelect
            ->Expln_utils_common.sortInPlaceWith((a,b) => Belt_Float.fromInt(a.frame.ord - b.frame.ord))
            ->Array.map(frm => frm.frame),
        ~isAxiom=None,
        ~typ=None,
        ~label,
        ~strictLabel?,
        ~pattern,
        ~patternVersion=2,
        ~ctx=st.preCtxData.ctxV.val.min,
        ~isDisc=None,
        ~isDepr=None,
        ~isTranDepr=None,
    )->Array.map(((frame,_)) => frame.label)
}

describe("MM_wrk_search_asrt integration tests: search by strictLabel", _ => {
    let st = createEditorState(~mmFilePath=demo0Path)

    it("returns only assertions with the given labels, in the order of declaration", _ => {
        //when
        let labels = st->searchLabels(~strictLabel="th2  a1\tmp")

        //then
        assertEq(labels, ["a1", "mp", "th2"])
    })

    it("doesn't match parts of labels", _ => {
        //when
        let strictLabels = st->searchLabels(~strictLabel="th")
        let labels = st->searchLabels(~label="th")

        //then
        assertEq(strictLabels, [])
        assertEq(labels, ["th1", "th2"])
    })

    it("is case-sensitive", _ => {
        //when
        let strictLabels = st->searchLabels(~strictLabel="A1 MP")
        let labels = st->searchLabels(~label="A1 MP")

        //then
        assertEq(strictLabels, [])
        assertEq(labels, ["a1", "mp"])
    })

    it("returns only assertions which also match label and pattern", _ => {
        //when
        let byLabel = st->searchLabels(~strictLabel="a1 a2 th1", ~label="a")
        let byPattern = st->searchLabels(~strictLabel="a1 a2 mp", ~pattern="+")

        //then
        assertEq(byLabel, ["a1", "a2"])
        assertEq(byPattern, ["a2"])
    })

    it("doesn't filter assertions when it is blank", _ => {
        //when
        let withBlankStrictLabel = st->searchLabels(~strictLabel="  ")
        let withoutStrictLabel = st->searchLabels(~label="")

        //then
        assertEq(withBlankStrictLabel, withoutStrictLabel)
        assertEq(withoutStrictLabel->Array.includes("th1"), true)
    })
})
