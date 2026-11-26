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
    ~stmtErr:option<string>=?,
    ~syntaxErr:option<string>=?,
    ~unifErr:option<string>=?,
):step => {
    {
        label,
        stmt,
        isHyp,
        isGoal,
        isBkm,
        status: Nullable.fromOption(status),
        jstfText: Nullable.make(jstfText),
        stmtErr: stmtErr->Option.map(msg => {msg: Nullable.make(msg)})->Nullable.fromOption,
        syntaxErr: Nullable.fromOption(syntaxErr),
        unifErr: Nullable.fromOption(unifErr),
    }
}

let mkState = (
    ~scope:string="all assertions",
    ~scopeErr:option<string>=?,
    ~descr:option<string>=?,
    ~varsText:string="",
    ~varsErr:option<string>=?,
    ~disjText:string="",
    ~disjErr:option<string>=?,
    ~selectedSteps:array<string>=[],
    ~steps:array<step>=[],
):editorState => {
    {
        tabId: "5",
        scope,
        scopeErr: Nullable.fromOption(scopeErr),
        descr: Nullable.fromOption(descr),
        varsText: Nullable.make(varsText),
        varsErr: Nullable.fromOption(varsErr),
        disjText: Nullable.make(disjText),
        disjErr: Nullable.fromOption(disjErr),
        selectedSteps,
        steps,
    }
}

let mkSrc = (
    ~type_:string="web",
    ~url:string="",
    ~fileName:string="",
    ~alias:option<string>=?,
    ~readInstr:string="readAll",
    ~label:option<string>=?,
    ~numOfAsrts:int,
    ~numOfAxioms:int,
    ~files:array<ctxFileInfo>=[],
):ctxSrcInfo => {
    {
        type_,
        url,
        fileName,
        alias: Nullable.fromOption(alias),
        readInstr,
        label: Nullable.fromOption(label),
        numOfAsrts,
        numOfAxioms,
        files,
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

    it("returns the index after the step given by afterLabel", () => {
        //when
        let res = getInsertIdx({steps, afterLabel: "1"})

        //then
        assertEq(res, {idx: 2})
    })

    it("returns the index after the last step when there is no goal step and no label is given", () => {
        //given
        let steps = [
            mkStep(~label="hyp1", ~stmt="|- ph", ~isHyp=true),
            mkStep(~label="1", ~stmt="|- ( ph -> ps )"),
        ]

        //when
        let res = getInsertIdx({steps: steps})

        //then
        assertEq(res, {idx: 2})
    })

    it("returns the index 0 when there are no steps", () => {
        //when
        let res = getInsertIdx({steps: []})

        //then
        assertEq(res, {idx: 0})
    })

    it("returns an error when the step given by beforeLabel doesn't exist, even if afterLabel exists", () => {
        //when
        let res = getInsertIdx({steps, beforeLabel: "2", afterLabel: "1"})

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

    it("prints only the label and the assertion for a theorem without disjoints and hypotheses", () => {
        //given
        let asrt = {
            label: "idi",
            isAxiom: false,
            disj: [],
            hyps: [],
            asrt: "|- ( ph -> ph )",
        }

        //when
        let text = assertionToText(asrt)

        //then
        assertEq(text, "idi\n  asrt: |- ( ph -> ph )")
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

describe("editorStateToText (more cases)", () => {
    it("prints all steps when onlyRelevantSteps is not set", () => {
        //given
        let state = mkState(~steps=[
            mkStep(~label="1", ~stmt="|- ph", ~status="v", ~jstfText=": ax-test"),
            mkStep(~label="qed", ~stmt="|- ps", ~isGoal=true, ~status="v", ~jstfText="1 : ax-test2"),
        ])

        //when
        let text = editorStateToText({state: state})

        //then
        assertEq(
            text,
            [
                "--- editor state begin ---",
                "Editor tab id: 5",
                "Scope: all assertions",
                "",
                "Steps (2):",
                "p v 1   |- ph",
                "    jstf: : ax-test",
                "G v qed |- ps",
                "    jstf: 1 : ax-test2",
                "--- editor state end ---",
            ]->Array.join("\n")
        )
    })

    it("prints the scope error, notes, description, variables, disjoints, and selected steps", () => {
        //given
        let state = mkState(
            ~scope="assertions before 'ax-mp'",
            ~scopeErr="Cannot find 'ax-mp'.",
            ~descr="  My theorem.\n",
            ~varsText="v1 setvar x1",
            ~varsErr="Unknown type.",
            ~disjErr="Bad disjoint.",
            ~selectedSteps=["1", "2"],
        )

        //when
        let text = editorStateToText({state, notes: ["Note one.", "Note two."]})

        //then
        assertEq(
            text,
            [
                "--- editor state begin ---",
                "Editor tab id: 5",
                "Scope: assertions before 'ax-mp'",
                "Scope error: Cannot find 'ax-mp'.",
                "Note: Note one.",
                "Note: Note two.",
                "",
                "Description:",
                "My theorem.",
                "",
                "Variables:",
                "v1 setvar x1",
                "    error: Unknown type.",
                "",
                "Disjoints:",
                "    error: Bad disjoint.",
                "",
                "Selected steps: 1 2",
                "",
                "Steps (0):",
                "--- editor state end ---",
            ]->Array.join("\n")
        )
    })

    it("prints step errors, aligns multiline errors, and doesn't print justifications of hypotheses", () => {
        //given
        let state = mkState(~steps=[
            mkStep(~label="hyp1", ~stmt="|- ph", ~isHyp=true, ~jstfText=": ax-test"),
            mkStep(~label="1", ~stmt="|- ps"),
            mkStep(
                ~label="2",
                ~stmt="|- ch",
                ~isBkm=true,
                ~status="x",
                ~jstfText="1 : ax-bad",
                ~stmtErr="Unknown label.",
                ~syntaxErr="  ",
                ~unifErr="line one\nline two",
            ),
        ])

        //when
        let text = editorStateToText({state: state})

        //then
        assertEq(
            text,
            [
                "--- editor state begin ---",
                "Editor tab id: 5",
                "Scope: all assertions",
                "",
                "Steps (3):",
                "H . hyp1 |- ph",
                "p . 1    |- ps",
                "P x 2    |- ch",
                "    jstf: 1 : ax-bad",
                "    error: Unknown label.",
                "    error: line one",
                "           line two",
                "--- editor state end ---",
            ]->Array.join("\n")
        )
    })

    it("shows bookmarked, invalid, and erroring steps, and hides proved and unknown status steps", () => {
        //given
        let state = mkState(~steps=[
            mkStep(~label="1", ~stmt="|- ph", ~isBkm=true, ~status="v"),
            mkStep(~label="2", ~stmt="|- ps", ~status="v", ~unifErr="warn"),
            mkStep(~label="3", ~stmt="|- ch", ~status="x"),
            mkStep(~label="4", ~stmt="|- th", ~status="v"),
            mkStep(~label="5", ~stmt="|- ta"),
        ])

        //when
        let text = editorStateToText({state, onlyRelevantSteps: true})

        //then
        assertEq(
            text,
            [
                "--- editor state begin ---",
                "Editor tab id: 5",
                "Scope: all assertions",
                "",
                "Steps (3 of 5 shown, proved non-bookmarked steps are hidden):",
                "P v 1 |- ph",
                "p v 2 |- ps",
                "    error: warn",
                "p x 3 |- ch",
                "--- editor state end ---",
            ]->Array.join("\n")
        )
    })
})

describe("loadedContextInfoToText", () => {
    let noLocalPath = _ => None

    it("returns a hint when no context is loaded", () => {
        //given
        let info = {numOfAsrts: 0, numOfAxioms: 0, srcs: []}

        //when
        let text = loadedContextInfoToText({info, urlToLocalPath: noLocalPath})

        //then
        assertEq(text, "No context is loaded. Use load_context to load one.")
    })

    it("prints a web source with its alias and included files", () => {
        //given
        let info = {
            numOfAsrts: 100,
            numOfAxioms: 12,
            srcs: [
                mkSrc(
                    ~url="https://us.metamath.org/metamath/set.mm",
                    ~alias="set.mm:latest",
                    ~numOfAsrts=100,
                    ~numOfAxioms=12,
                    ~files=[
                        {fileName: "set.mm", numOfAsrts: 90, numOfAxioms: 10},
                        {fileName: "inc.mm", numOfAsrts: 10, numOfAxioms: 2},
                    ],
                ),
            ],
        }

        //when
        let text = loadedContextInfoToText({info, urlToLocalPath: noLocalPath})

        //then
        assertEq(
            text,
            [
                "Loaded: 100 assertions (12 axioms).",
                "Sources, in the order of loading:",
                "1. https://us.metamath.org/metamath/set.mm",
                "   alias: set.mm:latest",
                "   read: the whole file",
                "   loaded: 100 assertions (12 axioms)",
                "   files (the first one is the file itself, others are included via $[ ... $]):",
                "     - set.mm: 90 assertions (10 axioms)",
                "     - inc.mm: 10 assertions (2 axioms)",
            ]->Array.join("\n")
        )
    })

    it("prints a local source, and a web source served by the MCP server as its local path", () => {
        //given
        let servedUrl = "http://127.0.0.1:8765/files/home/user/ext.mm"
        let info = {
            numOfAsrts: 8,
            numOfAxioms: 2,
            srcs: [
                mkSrc(
                    ~type_="local",
                    ~fileName="my.mm",
                    ~readInstr="stopBefore",
                    ~label="thm1",
                    ~numOfAsrts=5,
                    ~numOfAxioms=2,
                ),
                mkSrc(
                    ~url=servedUrl,
                    ~readInstr="stopAfter",
                    ~label="thm2",
                    ~numOfAsrts=3,
                    ~numOfAxioms=0,
                ),
            ],
        }

        //when
        let text = loadedContextInfoToText({
            info,
            urlToLocalPath: url => url == servedUrl ? Some("/home/user/ext.mm") : None,
        })

        //then
        assertEq(
            text,
            [
                "Loaded: 8 assertions (2 axioms).",
                "Sources, in the order of loading:",
                "1. my.mm (a local file selected in metamath-lamp, its full path is unknown)",
                "   read: up to 'thm1', excluding it",
                "   loaded: 5 assertions (2 axioms)",
                `2. /home/user/ext.mm (served by this MCP server as ${servedUrl})`,
                "   read: up to 'thm2', including it",
                "   loaded: 3 assertions (0 axioms)",
            ]->Array.join("\n")
        )
    })
})
