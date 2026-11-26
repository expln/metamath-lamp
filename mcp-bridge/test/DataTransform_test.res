open Test
open DataTransform

let mkStep = (
    ~label:string,
    ~stmt:string,
    ~isHyp:bool=false,
    ~isGoal:bool=false,
    ~isBkm:bool=false,
    ~status:option<string>=?,
    ~jstfText:string="",
):step => {
    {
        label,
        stmt,
        isHyp,
        isGoal,
        isBkm,
        status: Nullable.fromOption(status),
        jstfText: Nullable.make(jstfText),
        stmtErr: Nullable.null,
        syntaxErr: Nullable.null,
        unifErr: Nullable.null,
    }
}

describe("getInsertIdx", () => {
    let steps = [
        mkStep(~label="hyp1", ~stmt="|- ph", ~isHyp=true),
        mkStep(~label="1", ~stmt="|- ( ph -> ps )"),
        mkStep(~label="qed", ~stmt="|- ps", ~isGoal=true),
    ]

    it("returns the index of the step given by beforeLabel", () => {
        //when
        let res = getInsertIdx({steps, beforeLabel: "1", afterLabel: "hyp1"})

        //then
        assertEq(res, {idx: 1})
    })

    it("returns the index before the goal step when no label is given", () => {
        //when
        let res = getInsertIdx({steps: steps})

        //then
        assertEq(res, {idx: 2})
    })

    it("returns an error when the step given by afterLabel doesn't exist", () => {
        //when
        let res = getInsertIdx({steps, afterLabel: "2"})

        //then
        assertEq(res, {err: "No step with label '2' exists."})
    })
})

describe("assertionToText", () => {
    it("prints the label, the axiom mark, disjoints, hypotheses, and the assertion", () => {
        //given
        let asrt = {
            label: "ax-test",
            isAxiom: true,
            disj: [["x", "y"], ["x", "A"]],
            hyps: ["|- ph", "|- ps"],
            asrt: "|- ch",
        }

        //when
        let text = assertionToText(asrt)

        //then
        assertEq(
            text,
            [
                "ax-test (axiom)",
                "  disj: x y , x A",
                "  hyp: |- ph",
                "  hyp: |- ps",
                "  asrt: |- ch",
            ]->Array.join("\n")
        )
    })
})

describe("editorStateToText", () => {
    it("hides proved non-bookmarked steps when onlyRelevantSteps is true", () => {
        //given
        let state = {
            tabId: "3",
            scope: "all assertions",
            scopeErr: Nullable.null,
            descr: Nullable.undefined,
            varsText: Nullable.make(""),
            varsErr: Nullable.null,
            disjText: Nullable.make(""),
            disjErr: Nullable.null,
            selectedSteps: [],
            steps: [
                mkStep(~label="hyp1", ~stmt="|- ph", ~isHyp=true),
                mkStep(~label="1", ~stmt="|- ( ph -> ps )", ~status="v", ~jstfText=": ax-test"),
                mkStep(~label="2", ~stmt="|- ps", ~status="?"),
                mkStep(~label="qed", ~stmt="|- ps", ~isGoal=true, ~status="~", ~jstfText="hyp1 2 : ax-mp"),
            ],
        }

        //when
        let text = editorStateToText({state, onlyRelevantSteps: true, notes: ["Added steps: 2"]})

        //then
        assertEq(
            text,
            [
                "--- editor state begin ---",
                "Editor tab id: 3",
                "Scope: all assertions",
                "Note: Added steps: 2",
                "",
                "Steps (3 of 4 shown, proved non-bookmarked steps are hidden):",
                "H . hyp1 |- ph",
                "p ? 2    |- ps",
                "G ~ qed  |- ps",
                "    jstf: hyp1 2 : ax-mp",
                "--- editor state end ---",
            ]->Array.join("\n")
        )
    })
})
