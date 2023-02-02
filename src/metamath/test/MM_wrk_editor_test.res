open Expln_test
open MM_parser
open MM_context
open MM_proof_tree
open MM_provers
open MM_wrk_editor
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open MM_wrk_ctx

let createEditorState = (mmFile) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(mmFileText, ())
    let ctx = loadContext(ast, ())
    let parens = "( ) { } [ ]"
    ctx->moveConstsToBegin(parens)
    let st = {
        settingsV: 1,
        settings: {
            parens,
            asrtsToSkip: "idi",
            typeSettings: [ ],
        },
        typeColors: Belt_HashMapString.make(~hintSize=0),

        preCtxV: 1,
        preCtx: ctx,
        frms: prepareFrmSubsData(ctx),
        parenCnt: parenCntMake(prepareParenInts(ctx, parens), ()),
        preCtxColors: Belt_HashMapString.make(~hintSize=0),

        varsText: "",
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: "",
        disjEditMode: false,
        disjErr: None,
        disj: Belt_MapInt.fromArray([]),

        wrkCtx: None,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],
    }
    recalcAllColors(st)
}

let getVarType = (ctx:mmContext, vName:string) => {
    let varInt = (ctx->ctxSymsToIntsExn([vName]))[0]
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F && hyp.expr[1] == varInt) {
            Some(ctx->ctxIntToSymExn(hyp.expr[0]))
        } else {
            None
        }
    })->Belt_Option.getWithDefault("type-not-found")
}

let demo0 = "./src/metamath/test/resources/demo0.mm"
let findPossibleSubsSimpleCase = "./src/metamath/test/resources/findPossibleSubs-test-data/simple-case.mm"
let findPossibleSubsDisjointsCase = "./src/metamath/test/resources/findPossibleSubs-test-data/disjoints-case.mm"

describe("refreshWrkCtx", _ => {
    it("detects an error in variable declaration", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, "hyp_v1 term v1 \n hyp_v2 term- v2")

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.varsErr->Belt_Option.getWithDefault(""), "The first symbol in a floating expression must be a constant.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional variables are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, "hyp_v1 term v1 \n hyp_v2 wff v2")

        //when
        let st = refreshWrkCtx(st)

        //then
        switch st.wrkCtx {
            | Some(wrkCtx) => {
                assertEqMsg(wrkCtx->isVar("v1"), true, "v1 is var")
                assertEqMsg(getVarType(wrkCtx, "v1"), "term", "v1 is term")
                assertEqMsg(wrkCtx->isVar("v2"), true, "v2 is var")
                assertEqMsg(getVarType(wrkCtx, "v2"), "wff", "v2 is wff")
            }
            | _ => failMsg("A non-empty context was expected")
        }
    })
    
    it("detects an error in disjoints declaration", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeDisjEditMode(st, "t, r \n r, s-")

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.getWithDefault(""), "The symbol 's-' is not a variable but it is used in a disjoint statement.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional disjoints are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeDisjEditMode(st, "t, r \n r, s")

        //when
        let st = refreshWrkCtx(st)

        //then
        switch st.wrkCtx {
            | Some(wrkCtx) => {
                let ti = (wrkCtx->ctxSymsToIntsExn(["t"]))[0]
                let ri = (wrkCtx->ctxSymsToIntsExn(["r"]))[0]
                let si = (wrkCtx->ctxSymsToIntsExn(["s"]))[0]
                assertEqMsg(wrkCtx->isDisj(ti,ri), true, "t and r are disjoint")
                assertEqMsg(wrkCtx->isDisj(ri,ti), true, "r and t are disjoint")
                assertEqMsg(wrkCtx->isDisj(ri,si), true, "r and s are disjoint")
                assertEqMsg(wrkCtx->isDisj(si,ri), true, "s and r are disjoint")
                assertEqMsg(wrkCtx->isDisj(ti,si), false, "t and s are not disjoint")
                assertEqMsg(wrkCtx->isDisj(si,ti), false, "s and t are not disjoint")
            }
            | _ => failMsg("A non-empty context was expected")
        }
    })
    
    it("detects an error in a hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let prId = st.stmts[0].id
        let hypId = st.stmts[1].id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:#p, label:"pr", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:#e, label:"hyp", cont:strToCont("|- 0 + 0.", ())})

        //when
        let st = refreshWrkCtx(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[0].id, hypId, "the hypothesis is the first")
        assertEq(st.stmts[0].stmtErr->Belt_Option.getWithDefault(""), "The symbol '0.' is not declared.")
        assertEqMsg(st.stmts[1].id, prId, "the provable is the second")
        assertEq(st.stmts[1].stmtErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when there are few correct hypotheses", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let hyp1Id = st.stmts[0].id
        let hyp2Id = st.stmts[1].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- t + t", ())})

        //when
        let st = refreshWrkCtx(st)

        //then
        switch st.wrkCtx {
            | Some(wrkCtx) => {
                assertEqMsg(st.stmts[0].id, hyp1Id, "hyp1 is the first")
                assertEq(st.stmts[0].stmtErr->Belt_Option.isNone, true)
                assertEqMsg(st.stmts[1].id, hyp2Id, "hyp2 is the second")
                assertEq(st.stmts[1].stmtErr->Belt_Option.isNone, true)

                assertEqMsg(wrkCtx->isHyp("hyp1"), true, "hyp1 is a hypothesis")
                assertEqMsg(wrkCtx->isHyp("hyp2"), true, "hyp2 is a hypothesis")
            }
            | _ => failMsg("A non-empty context was expected")
        }
    })
})

describe("prepareProvablesForUnification", _ => {
    it("detects an error in a provable expression", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 +- 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ())})
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.getWithDefault(""), "The symbol '+-' is not declared.")
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.isNone, true)
    })

    it("detects a syntax error in a provable's justification", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "Cannot parse justification: 'pr1 hyp1' [1].")
    })

    it("detects a ref error in a provable's justification when asrt label refers to a hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : hyp1"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "The label 'hyp1' doesn't refer to any assertion.")
    })

    it("detects a ref error in a provable's justification when asrt label refers to another provable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : pr1"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "The label 'pr1' doesn't refer to any assertion.")
    })

    it("detects a ref error in a provable's justification when argument label is undefined", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp-- : ax"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "The reference 'hyp--' is not defined.")
    })

    it("detects a label duplication when a provable uses label of a predefined hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"tt", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : mp"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "Cannot reuse label 'tt'.")
    })

    it("detects a label duplication when a provable uses label of a predefined assertion", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"mp", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : mp"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "Cannot reuse label 'mp'.")
    })

    it("detects a label duplication when a provable uses label of a previously defined another provable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : mp"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.getWithDefault(""), "Cannot reuse label 'pr1'.")
    })

    it("sets expr and jstf for each provable when there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let hyp1Id = st.stmts[2].id
        let hyp2Id = st.stmts[3].id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:#e, label:"hyp1", cont:strToCont("|- t + t", ())})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:#e, label:"hyp2", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term", ()),
            jstfText: "pr1 hyp1 : mp"
        })
        let st = refreshWrkCtx(st)

        //when
        let st = prepareProvablesForUnification(st)

        //then
        assertEqMsg(st.stmts[2].id, pr1Id, "pr1 is the third")
        assertEq(st.stmts[2].stmtErr->Belt_Option.isNone, true)
        assertEq(st.stmts[2].expr->Belt_Option.isSome, true)
        assertEq(st.stmts[2].jstf->Belt_Option.isNone, true)

        assertEqMsg(st.stmts[3].id, pr2Id, "pr2 is the fourth")
        assertEq(st.stmts[3].stmtErr->Belt_Option.isNone, true)
        assertEq(st.stmts[3].expr->Belt_Option.isSome, true)
        assertEq(st.stmts[3].jstf, Some({args:["pr1", "hyp1"], asrt:"mp"}))
    })
})

describe("findPossibleSubs", _ => {
    it("finds all possible substitutions", _ => {
        //given
        let st = createEditorState(findPossibleSubsSimpleCase)->prepareEditorForUnification
        let ctx = st.wrkCtx->Belt_Option.getExn

        let t = ctx->ctxSymToIntExn("t")
        let r = ctx->ctxSymToIntExn("r")
        let s = ctx->ctxSymToIntExn("s")

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("t + r"),
            ctx->ctxStrToIntsExn("( t + t ) + ( t + r ) + ( t + s )"),
        )

        //then
        assertEq(
            possibleSubs->Js.Array2.map(wrkSubs => wrkSubs.subs),
            [
                Belt_MapInt.fromArray([
                    (t, ctx->ctxStrToIntsExn("( t + t )")),
                    (r, ctx->ctxStrToIntsExn("( t + r ) + ( t + s )")),
                    (s, ctx->ctxStrToIntsExn("s")),
                ]),
                Belt_MapInt.fromArray([
                    (t, ctx->ctxStrToIntsExn("( t + t ) + ( t + r )")),
                    (r, ctx->ctxStrToIntsExn("( t + s )")),
                    (s, ctx->ctxStrToIntsExn("s")),
                ])
            ]
        )
    })

    it("returns new disjoints if any", _ => {
        //given
        let st = createEditorState(findPossibleSubsDisjointsCase)
        let st = completeDisjEditMode(st, "x, y")
        let st = prepareEditorForUnification(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        let t = ctx->ctxSymToIntExn("t")
        let x = ctx->ctxSymToIntExn("x")
        let y = ctx->ctxSymToIntExn("y")
        let z = ctx->ctxSymToIntExn("z")

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("y"),
            ctx->ctxStrToIntsExn("z"),
        )

        //then
        assertEq(
            possibleSubs->Js.Array2.map(wrkSubs => wrkSubs.subs),
            [
                Belt_MapInt.fromArray([
                    (t, ctx->ctxStrToIntsExn("t")),
                    (x, ctx->ctxStrToIntsExn("x")),
                    (y, ctx->ctxStrToIntsExn("z")),
                    (z, ctx->ctxStrToIntsExn("z")),
                ]),
            ]
        )

        let newDisjStrArr = []
        possibleSubs[0].newDisj->disjForEachArr(arr => {
            newDisjStrArr->Js.Array2.push(ctx->ctxIntsToStrExn(arr))->ignore
        })
        assertEq( newDisjStrArr, ["x z"] )
    })

    it("doesn't return substitutions which don't satisfy disjoints", _ => {
        //given
        let st = createEditorState(findPossibleSubsDisjointsCase)
        let st = prepareEditorForUnification(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        let t = ctx->ctxSymToIntExn("t")
        let x = ctx->ctxSymToIntExn("x")
        let y = ctx->ctxSymToIntExn("y")
        let z = ctx->ctxSymToIntExn("z")

        let stmt1 = "x + y"
        let stmt2 = "z + z"

        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn(stmt1),
            ctx->ctxStrToIntsExn(stmt2),
        )

        assertEq(
            possibleSubs->Js.Array2.map(wrkSubs => wrkSubs.subs),
            [
                Belt_MapInt.fromArray([
                    (t, ctx->ctxStrToIntsExn("t")),
                    (x, ctx->ctxStrToIntsExn("z")),
                    (y, ctx->ctxStrToIntsExn("z")),
                    (z, ctx->ctxStrToIntsExn("z")),
                ]),
            ]
        )

        let st = completeDisjEditMode(st, "x, y")
        let st = prepareEditorForUnification(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn(stmt1),
            ctx->ctxStrToIntsExn(stmt2),
        )

        //then
        assertEq( possibleSubs, [] )
    })

    it("returns substitutions which satisfy disjoints", _ => {
        //given
        let st = createEditorState(findPossibleSubsDisjointsCase)
        let st = prepareEditorForUnification(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("y"),
            ctx->ctxStrToIntsExn("z"),
        )

        //then
        assertEq( possibleSubs->Js_array2.length, 1 )
    })
})

describe("applySubstitutionForEditor", _ => {
    it("applies substitutions correctly", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let st = updateStmt(st, pr1Id, stmt => {...stmt, cont:strToCont("|- t + s", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, cont:strToCont("|- r = 0", ())})
        let st = prepareEditorForUnification(st)
        let ctx = st.wrkCtx->Belt_Option.getExn
        let wrkSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("t = s"),
            ctx->ctxStrToIntsExn("r = ( t + r )"),
        )[0]

        //when
        let st = applySubstitutionForEditor(st, wrkSubs)

        //then
        assertEq(
            st.stmts[0].cont->contToStr,
            "|- r + ( t + r )"
        )
        assertEq(
            st.stmts[1].cont->contToStr,
            "|- r = 0"
        )
    })
})

describe("removeUnusedVars", _ => {
    it("removes unused variables", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st,"v1 term a \n v2 term b \n v3 term c \n v4 term d")
        let st = completeDisjEditMode(st,"a,b,c \n d,c")
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = st.stmts[0].id
        let pr2Id = st.stmts[1].id
        let st = updateStmt(st, pr1Id, stmt => {...stmt, cont:strToCont("|- t + a", ())})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, cont:strToCont("|- r = c", ())})
        let st = prepareEditorForUnification(st)

        //when
        let st = removeUnusedVars(st)

        //then
        assertEq( st.varsText, "v1 term a\nv3 term c" )
        assertEq( st.disjText, "a,c" )
        assertEq( st.stmts[0].cont->contToStr, "|- t + a" )
        assertEq( st.stmts[1].cont->contToStr, "|- r = c" )
    })
})