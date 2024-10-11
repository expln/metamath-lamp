open Expln_test
open MM_parser
open MM_context
open MM_wrk_editor
open MM_wrk_editor_substitution
open MM_wrk_settings
open MM_substitution
open MM_parenCounter
open MM_wrk_pre_ctx_data

let createEditorState = (
    mmFile:string, 
    ~initStmtIsGoal:bool=false, 
    ~defaultStmtLabel:string=""
) => {
    let mmFileText = Expln_utils_files.readStringFromFile(mmFile)
    let (ast, _) = parseMmFile(~mmFileContent=mmFileText, ~skipComments=true, ~skipProofs=true)
    let ctx = loadContext(ast)
    let parens = "( ) { } [ ]"
    let settingsV = 1
    let settings = {
        parens,
        asrtsToSkip: [],
        descrRegexToDisc: "",
        labelRegexToDisc: "",
        descrRegexToDepr: "",
        labelRegexToDepr: "",
        discColor:"",
        deprColor:"",
        tranDeprColor:"",
        editStmtsByLeftClick:true,
        initStmtIsGoal,
        defaultStmtLabel,
        defaultStmtType: "",
        unifMetavarPrefix: "&",
        sortDisjByType: "class wff",
        checkSyntax: true,
        stickGoalToBottom: true,
        autoMergeStmts: false,
        autoUnifyAll: false,
        typeSettings: [ ],
        webSrcSettings: [ ],
        longClickEnabled: true,
        longClickDelayMs: 500,
        hideContextSelector: false,
        showVisByDefault:false,
        editorHistMaxLength:0,
        allowedFrms: {
            inSyntax: {
                useDisc:true,
                useDepr:true,
                useTranDepr:true,
            },
            inEssen: {
                useDisc:true,
                useDepr:true,
                useTranDepr:true,
            },
        },
        useDefaultTransforms:false,
        useCustomTransforms:false,
        customTransforms:"",
        combCntMax:10000,
    }
    let preCtxV = 1
    let preCtx = ctx
    let st = {
        settingsV,
        settings,
        typeColors: Belt_HashMapString.make(~hintSize=0),

        srcs: [],
        preCtxV,
        preCtx,
        frms: prepareFrmSubsData(~ctx),
        parenCnt: parenCntMake(~parenMin=0, ~canBeFirstMin=0, ~canBeFirstMax=0, ~canBeLastMin=0, ~canBeLastMax=0),
        preCtxColors: Belt_HashMapString.make(~hintSize=0),
        allTypes: [],
        syntaxTypes: [],
        parensMap:Belt_HashMapString.make(~hintSize=0),
        typeOrderInDisj:Belt_HashMapInt.make(~hintSize=0),

        descr: "",
        descrEditMode: false,

        varsText: "",
        varsEditMode: false,
        varsErr: None,
        wrkCtxColors: Belt_HashMapString.make(~hintSize=0),

        disjText: "",
        disjEditMode: false,
        disjErr: None,

        wrkCtx: None,

        nextStmtId: 0,
        stmts: [],
        checkedStmtIds: [],

        nextAction: None,
    }
    let st = st->setPreCtxData(
        preCtxDataMake(~settings)->preCtxDataUpdate(
            ~settings,
            ~ctx=([], preCtx)
        )
    )
    st
}

let getVarType = (ctx:mmContext, vName:string) => {
    let varInt = (ctx->ctxSymsToIntsExn([vName]))->Array.getUnsafe(0)
    ctx->forEachHypothesisInDeclarationOrder(hyp => {
        if (hyp.typ == F && hyp.expr->Array.getUnsafe(1) == varInt) {
            Some(ctx->ctxIntToSymExn(hyp.expr->Array.getUnsafe(0)))
        } else {
            None
        }
    })->Belt_Option.getWithDefault("type-not-found")
}

let subsToSortedArr = subs => subs->Belt_MapInt.toArray->Expln_utils_common.sortInPlaceWith(((i1,_),(i2,_)) => Belt_Float.fromInt(i1-i2))
let disjToSortedArr = disj => {
    let res = []
    disj->disjForEach((n,m) => res->Array.push((n,m)))
    {
        open Expln_utils_common
        res->sortInPlaceWith(
        comparatorBy(((l,_)) => l)
            ->comparatorAndThen(
                comparatorBy(((_,r)) => r)
            )
        )
    }
}

let wrkSubsToStr = (ctx:mmContext, wrkSubs:wrkSubs):string => {
    "{newDisj:["
        ++ disjToSortedArr(wrkSubs.newDisj)
            ->Array.map(((l,r)) => `(${ctx->ctxIntToSymExn(l)},${ctx->ctxIntToSymExn(r)})`)
            ->Array.joinUnsafe(",")
        ++ "], subs:["
        ++ subsToSortedArr(wrkSubs.subs)
            ->Array.map(((l,r)) => `(${ctx->ctxIntToSymExn(l)} -> [${r->Array.map(ctxIntToSymExn(ctx, _))->Array.joinUnsafe(",")}])`)
            ->Array.joinUnsafe(",")
        ++ "], err: "
        ++ switch wrkSubs.err {
            | None => "None"
            | Some(CommonVar({var1, var2, commonVar})) =>
                `CommonVar({var1:${ctx->ctxIntToSymExn(var1)}, var2:${ctx->ctxIntToSymExn(var2)}, commonVar:${ctx->ctxIntToSymExn(commonVar)}})`
            | Some(TypeMismatch({var, subsExpr, typeExpr})) =>
                `TypeMismatch({var:${ctx->ctxIntToSymExn(var)}, subsExpr:[${ctx->ctxIntsToStrExn(subsExpr)}], typeExpr:[${ctx->ctxIntsToStrExn(typeExpr)}]})`
        }
        ++ "}"
}

let demo0 = "./src/metamath/test/resources/demo0._mm"
let findPossibleSubsSimpleCase = "./src/metamath/test/resources/findPossibleSubs-test-data/simple-case.mm"
let findPossibleSubsDisjointsCase = "./src/metamath/test/resources/findPossibleSubs-test-data/disjoints-case.mm"
let findPossibleSubsTypeCase = "./src/metamath/test/resources/findPossibleSubs-test-data/type-case.mm"


describe("prepareEditorForUnification", _ => {
    it("detects an error in variable declaration", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, "hyp_v1 term v1 \n hyp_v2 term- v2")

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.getWithDefault(""), "The first symbol in the floating 'term-' must be a constant.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional variables are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st, ".hyp_v1 term v1 \n .hyp_v2 wff v2")

        //when
        let st = verifyEditorState(st)

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
        let st = completeDisjEditMode(st, "t r \n r s-")

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.getWithDefault(""), "The symbol 's-' is not a variable but it is used in a disjoint statement.")
        assertEq(st.wrkCtx->Belt_Option.isNone, true)
    })
    
    it("creates wrkCtx when only additional disjoints are defined and there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeDisjEditMode(st, "t r \n r s")
        let (st,s1) = st->addNewStmt
        let st = st->completeContEditMode(s1, "t r s")

        //when
        let st = verifyEditorState(st)

        //then
        switch st.wrkCtx {
            | Some(wrkCtx) => {
                let ti = (wrkCtx->ctxSymsToIntsExn(["t"]))->Array.getUnsafe(0)
                let ri = (wrkCtx->ctxSymsToIntsExn(["r"]))->Array.getUnsafe(0)
                let si = (wrkCtx->ctxSymsToIntsExn(["s"]))->Array.getUnsafe(0)
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
        let prId = (st.stmts->Array.getUnsafe(0)).id
        let hypId = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:P, label:"pr", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:E, label:"hyp", cont:strToCont("|- 0 + 0.")})

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isSome, true)
        assertEqMsg((st.stmts->Array.getUnsafe(0)).id, hypId, "the hypothesis is the first")
        assertEq(
            (st.stmts->Array.getUnsafe(0)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "The symbol '0.' is not declared."
        )
        assertEqMsg((st.stmts->Array.getUnsafe(1)).id, prId, "the provable is the second")
        assertEq((st.stmts->Array.getUnsafe(1)).stmtErr->Belt_Option.isNone, true)
    })
    
    it("detects an error when a hypothesis is empty", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let prId = (st.stmts->Array.getUnsafe(0)).id
        let hypId = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:P, label:"pr", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:E, label:"hyp", cont:strToCont(""),contEditMode:false})

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isSome, true)
        assertEqMsg((st.stmts->Array.getUnsafe(0)).id, hypId, "the hypothesis is the first")
        assertEq(
            (st.stmts->Array.getUnsafe(0)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "Any statement must begin with a constant."
        )
        assertEqMsg((st.stmts->Array.getUnsafe(1)).id, prId, "the provable is the second")
        assertEq((st.stmts->Array.getUnsafe(1)).stmtErr->Belt_Option.isNone, true)
    })
    
    it("detects an error when a hypothesis begins with a variable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let prId = (st.stmts->Array.getUnsafe(0)).id
        let hypId = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:P, label:"pr", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:E, label:"hyp", cont:strToCont("t = t"),
            contEditMode:false
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isSome, true)
        assertEqMsg((st.stmts->Array.getUnsafe(0)).id, hypId, "the hypothesis is the first")
        Console.log2(`st`, st)
        assertEq(
            (st.stmts->Array.getUnsafe(0)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "Any statement must begin with a constant."
        )
        assertEqMsg((st.stmts->Array.getUnsafe(1)).id, prId, "the provable is the second")
        assertEq((st.stmts->Array.getUnsafe(1)).stmtErr->Belt_Option.isNone, true)
    })

    it("detects an error in a provable expression", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 +- 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term")})

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq(
            (st.stmts->Array.getUnsafe(2)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "The symbol '+-' is not declared."
        )
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq((st.stmts->Array.getUnsafe(3)).stmtErr->Belt_Option.isNone, true)
    })
    
    it("detects an error when a provable expression is empty", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let prId = (st.stmts->Array.getUnsafe(0)).id
        let hypId = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:P, label:"pr", cont:strToCont(""),
            contEditMode:false
        })
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:E, label:"hyp", cont:strToCont("|- t + t")})

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isSome, true)
        assertEqMsg((st.stmts->Array.getUnsafe(0)).id, hypId, "the hypothesis is the first")
        assertEq((st.stmts->Array.getUnsafe(0)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(1)).id, prId, "the provable is the second")
        assertEq(
            (st.stmts->Array.getUnsafe(1)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "Any statement must begin with a constant."
        )
    })
    
    it("detects an error when a provable expression begins with a variable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let prId = (st.stmts->Array.getUnsafe(0)).id
        let hypId = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, prId, stmt => {...stmt, typ:P, label:"pr", cont:strToCont("t = t"),
            contEditMode:false
        })
        let st = updateStmt(st, hypId, stmt => {...stmt, typ:E, label:"hyp", cont:strToCont("|- t + t")})

        //when
        let st = verifyEditorState(st)

        //then
        assertEq(st.varsErr->Belt_Option.isNone, true)
        assertEq(st.disjErr->Belt_Option.isNone, true)
        assertEq(st.wrkCtx->Belt_Option.isSome, true)
        assertEqMsg((st.stmts->Array.getUnsafe(0)).id, hypId, "the hypothesis is the first")
        assertEq((st.stmts->Array.getUnsafe(0)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(1)).id, prId, "the provable is the second")
        assertEq(
            (st.stmts->Array.getUnsafe(1)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "Any statement must begin with a constant."
        )
    })

    it("detects a syntax error in a provable's justification", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "Cannot parse justification: 'pr1 hyp1'. A justification must contain exactly one colon symbol."
        )
    })

    it("detects a ref error in a provable's justification when asrt label refers to a hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : hyp1"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "The label 'hyp1' doesn't refer to any assertion."
        )
    })

    it("detects a ref error in a provable's justification when asrt label refers to another provable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : pr1"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "The label 'pr1' doesn't refer to any assertion."
        )
    })

    it("detects a ref error in a provable's justification when argument label is undefined", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp-- : ax"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "The label 'hyp--' is not defined."
        )
    })

    it("detects a label duplication when a provable uses label of a predefined hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"tt", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : mp"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "[4] Cannot reuse label 'tt'."
        )
    })

    it("detects a label duplication when a provable uses label of a previously defined hypothesis", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"hyp2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : mp"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "[4] Cannot reuse label 'hyp2'."
        )
    })

    it("detects a label duplication when a provable uses label of a previously defined another provable", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : mp"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq(
            (st.stmts->Array.getUnsafe(3)).stmtErr->Belt.Option.map(err => err.msg)->Belt_Option.getWithDefault(""),
            "[4] Cannot reuse label 'pr1'."
        )
    })

    it("sets expr and jstf for each provable when there are no errors", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let hyp1Id = (st.stmts->Array.getUnsafe(2)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(3)).id
        let st = updateStmt(st, hyp1Id, stmt => {...stmt, typ:E, label:"hyp1", cont:strToCont("|- t + t")})
        let st = updateStmt(st, hyp2Id, stmt => {...stmt, typ:E, label:"hyp2", cont:strToCont("|- 0 + 0")})
        let st = updateStmt(st, pr1Id, stmt => {...stmt, label:"pr1", cont:strToCont("|- 0 + 0 + 0")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, label:"pr2", cont:strToCont("|- t term"),
            jstfText: "pr1 hyp1 : mp"
        })

        //when
        let st = verifyEditorState(st)

        //then
        assertEqMsg((st.stmts->Array.getUnsafe(2)).id, pr1Id, "pr1 is the third")
        assertEq((st.stmts->Array.getUnsafe(2)).stmtErr->Belt_Option.isNone, true)
        assertEq((st.stmts->Array.getUnsafe(2)).expr->Belt_Option.isSome, true)
        assertEq((st.stmts->Array.getUnsafe(2)).jstf->Belt_Option.isNone, true)

        assertEqMsg((st.stmts->Array.getUnsafe(3)).id, pr2Id, "pr2 is the fourth")
        assertEq((st.stmts->Array.getUnsafe(3)).stmtErr->Belt_Option.isNone, true)
        assertEq((st.stmts->Array.getUnsafe(3)).expr->Belt_Option.isSome, true)
        assertEq((st.stmts->Array.getUnsafe(3)).jstf, Some({args:["pr1", "hyp1"], label:"mp"}))
    })
})

describe("findPossibleSubs", _ => {
    it("finds all possible substitutions", _ => {
        //given
        let st = createEditorState(findPossibleSubsSimpleCase)->verifyEditorState
        let ctx = st.wrkCtx->Belt_Option.getExn

        let t = ctx->ctxSymToIntExn("t")
        let r = ctx->ctxSymToIntExn("r")
        let s = ctx->ctxSymToIntExn("s")

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("t + r"),
            ctx->ctxStrToIntsExn("( t + t ) + ( t + r ) + ( t + s )"),
            true,
        )->Belt.Result.getExn

        //then
        assertEq(
            possibleSubs->Array.map(wrkSubs => wrkSubs.subs),
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
        let (st,s1) = st->addNewStmt
        let st = st->completeContEditMode(s1, "x y")
        let st = completeDisjEditMode(st, "x y")
        let st = verifyEditorState(st)
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
            true,
        )->Belt.Result.getExn

        //then
        assertEq(possibleSubs->Array.length, 1)
        let expectedDisj = disjMake()
        expectedDisj->disjAddPair(x,z)
        assertEq(
            ctx->wrkSubsToStr(possibleSubs->Array.getUnsafe(0)),
            ctx->wrkSubsToStr(
                {
                    newDisj: expectedDisj,
                    subs: Belt_MapInt.fromArray([
                        (t,[t]),
                        (x,[x]),
                        (y,[z]),
                        (z,[z]),
                    ]),
                    err: None
                }
            ),
        )
    })

    it("doesn't return substitutions which don't satisfy disjoints", _ => {
        //given
        let st = createEditorState(findPossibleSubsDisjointsCase)
        let st = verifyEditorState(st)
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
            true,
        )->Belt.Result.getExn

        assertEq(possibleSubs->Array.length, 1)
        assertEqMsg(
            ctx->wrkSubsToStr(possibleSubs->Array.getUnsafe(0)),
            ctx->wrkSubsToStr(
                {
                    newDisj: disjMake(),
                    subs: Belt_MapInt.fromArray([
                        (t,[t]),
                        (x,[z]),
                        (y,[z]),
                        (z,[z]),
                    ]),
                    err: None
                }
            ),
            "assert-1"
        )

        let (st,s1) = st->addNewStmt
        let st = st->completeContEditMode(s1, "x y")
        let st = completeDisjEditMode(st, "x y")
        let st = verifyEditorState(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn(stmt1),
            ctx->ctxStrToIntsExn(stmt2),
            true,
        )->Belt.Result.getExn

        //then
        assertEq(possibleSubs->Array.length, 1)
        assertEqMsg(
            ctx->wrkSubsToStr(possibleSubs->Array.getUnsafe(0)),
            ctx->wrkSubsToStr(
                {
                    newDisj: disjMake(),
                    subs: Belt_MapInt.fromArray([
                        (t,[t]),
                        (x,[z]),
                        (y,[z]),
                        (z,[z]),
                    ]),
                    err: Some(CommonVar({var1:x, var2:y, commonVar:z}))
                }
            ),
            "assert-2"
        )
    })

    it("doesn't return substitutions which don't satisfy types", _ => {
        //given
        let st = createEditorState(findPossibleSubsTypeCase)
        let st = verifyEditorState(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        let t = ctx->ctxSymToIntExn("t")
        let x = ctx->ctxSymToIntExn("x")
        let y = ctx->ctxSymToIntExn("y")
        let z = ctx->ctxSymToIntExn("z")

        let stmt1 = "x + y"
        let stmt2 = "y + z + t"

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn(stmt1),
            ctx->ctxStrToIntsExn(stmt2),
            true,
        )->Belt.Result.getExn

        //then
        assertEq(possibleSubs->Array.length, 2)
        assertEq(
            ctx->wrkSubsToStr(possibleSubs->Array.getUnsafe(0)),
            ctx->wrkSubsToStr(
                {
                    newDisj: disjMake(),
                    subs: Belt_MapInt.fromArray([
                        (t,[t]),
                        (x,[y]),
                        (y,ctx->ctxStrToIntsExn("z + t")),
                        (z,[z]),
                    ]),
                    err: Some(TypeMismatch({
                        var:y, 
                        subsExpr:ctx->ctxStrToIntsExn("z + t"), 
                        typeExpr:ctx->ctxStrToIntsExn("term z + t")
                    }))
                }
            ),
        )
        assertEq(
            ctx->wrkSubsToStr(possibleSubs->Array.getUnsafe(1)),
            ctx->wrkSubsToStr(
                {
                    newDisj: disjMake(),
                    subs: Belt_MapInt.fromArray([
                        (t,[t]),
                        (x,ctx->ctxStrToIntsExn("y + z")),
                        (y,[t]),
                        (z,[z]),
                    ]),
                    err: Some(TypeMismatch({
                        var:x, 
                        subsExpr:ctx->ctxStrToIntsExn("y + z"), 
                        typeExpr:ctx->ctxStrToIntsExn("term y + z")
                    }))
                }
            ),
        )
    })

    it("returns substitutions which satisfy disjoints", _ => {
        //given
        let st = createEditorState(findPossibleSubsDisjointsCase)
        let st = verifyEditorState(st)
        let ctx = st.wrkCtx->Belt_Option.getExn

        //when
        let possibleSubs = findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("y"),
            ctx->ctxStrToIntsExn("z"),
            true,
        )->Belt.Result.getExn

        //then
        assertEq( possibleSubs->Array.length, 1 )
    })
})

describe("applySubstitutionForEditor", _ => {
    it("applies substitutions correctly", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, pr1Id, stmt => {...stmt, cont:strToCont("|- t + s")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, cont:strToCont("|- r = 0")})
        let st = verifyEditorState(st)
        let ctx = st.wrkCtx->Belt_Option.getExn
        let wrkSubs = (findPossibleSubs(
            st, 
            ctx->ctxStrToIntsExn("t = s"),
            ctx->ctxStrToIntsExn("r = ( t + r )"),
            true,
        )->Belt.Result.getExn)->Array.getUnsafe(0)

        //when
        let st = applySubstitutionForEditor(st, wrkSubs)

        //then
        assertEq(
            (st.stmts->Array.getUnsafe(0)).cont->contToStr,
            "|- r + ( t + r )"
        )
        assertEq(
            (st.stmts->Array.getUnsafe(1)).cont->contToStr,
            "|- r = 0"
        )
    })
})

describe("removeUnusedVars", _ => {
    it("removes unused variables", _ => {
        //given
        let st = createEditorState(demo0)
        let st = completeVarsEditMode(st,"v1 term a \n v2 term b \n v3 term c \n v4 term d")
        let st = completeDisjEditMode(st,"a b c \n d c")
        let (st, _) = addNewStmt(st)
        let (st, _) = addNewStmt(st)
        let pr1Id = (st.stmts->Array.getUnsafe(0)).id
        let pr2Id = (st.stmts->Array.getUnsafe(1)).id
        let st = updateStmt(st, pr1Id, stmt => {...stmt, cont:strToCont("|- t + a")})
        let st = updateStmt(st, pr2Id, stmt => {...stmt, cont:strToCont("|- r = c")})
        let st = verifyEditorState(st)

        //when
        let st = removeUnusedVars(st)

        //then
        assertEq( st.varsText, "v1 term a\nv3 term c" )
        assertEq( st.disjText, "a c" )
        assertEq( (st.stmts->Array.getUnsafe(0)).cont->contToStr, "|- t + a" )
        assertEq( (st.stmts->Array.getUnsafe(1)).cont->contToStr, "|- r = c" )
    })
})

describe("automatic convertion E<->P depending on jstfText", _ => {
    
    it("+s1 -> s1.typ=P, s1.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "isGoal")
    })
    
    it("+s1, s1.jstf=hYp -> s1.typ=E, s1.isGoal=F", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s1, "hYp")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2 -> s1.typ=E, s1.isGoal=F, s2.typ=P, s2.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hYp")

        //when
        let (st, s2) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, true, "isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, +s3 -> s1.typ=E, s1.isGoal=F, s2.typ=E, s2.isGoal=F, s3.typ=P, s3.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "HYP")

        //when
        let (st, s3) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, E , "typ")
        assertEq( editorGetStmtByIdExn(st,s2).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s3).typ, P , "typ")
        assertEqMsg( editorGetStmtByIdExn(st,s3).isGoal, true, "isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, +s3, s2.jstf=_ -> s1.typ=E, s1.isGoal=F, s2.typ=P, s2.isGoal=F, s3.typ=P, s3.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "hyp")
        let (st, s3) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s2, "")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s3).typ, P , "typ")
        assertEqMsg( editorGetStmtByIdExn(st,s3).isGoal, true, "isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, +s3, s2.jstf=hhyp -> s1.typ=E, s1.isGoal=F, s2{typ=P,jstf=hhyp}, s2.isGoal=F, s3.typ=P, s3.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "hyp")
        let (st, s3) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s2, "hhyp")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "s1.typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "s2.isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s2).jstfText, "hhyp", "s2.jstfText")

        assertEqMsg( editorGetStmtByIdExn(st,s3).typ, P , "s3.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s3).isGoal, true, "s3.isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, +s3, s2.jstf=_, s2.jstf=hhyp -> s1.typ=E, s1.isGoal=F, s2.typ=P, s2.isGoal=F, s2.jstf=hhyp, s3.typ=P, s3.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "hyp")
        let (st, s3) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "")

        //when
        let st = st->completeJstfEditMode(s2, "hhyp")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "s1.typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "s2.isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s2).jstfText, "hhyp", "s2.jstfText")

        assertEqMsg( editorGetStmtByIdExn(st,s3).typ, P , "s3.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s3).isGoal, true, "s3.isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, s2.jstf=_ -> s1.typ=E, s1.isGoal=F, s2.typ=P, s2.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "hyp")

        //when
        let st = st->completeJstfEditMode(s2, "")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "s1.typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, true, "s2.isGoal")
    })
    
    it("+s1, s1.jstf=hyp, +s2, s2.jstf=hyp, s2.jstf=_, s1.jstf=_ -> s1.typ=P, s1.isGoal=F, s2.typ=P, s2.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let st = st->completeJstfEditMode(s1, "hyp")
        let (st, s2) = addNewStmt(st)
        let st = st->completeJstfEditMode(s2, "hyp")
        let st = st->completeJstfEditMode(s2, "")

        //when
        let st = st->completeJstfEditMode(s1, "")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, true, "s2.isGoal")
    })
    
    it("+s1, s1.jstf=abc, s1.jstf=_ -> s1.typ=P, s1.isGoal=T", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s1, "")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "s1.isGoal")
    })
    
    it("+s1, +s2 -> s1.typ=P, s1.isGoal=T, s2.typ=P, s2.isGoal=F", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)

        //when
        let (st, s2) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "s2.isGoal")
    })
    
    it("+s1, +s2, s1.jstf=hyp -> s1.typ=E, s1.isGoal=F, s2.typ=P, s2.isGoal=F", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let (st, s2) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s1, "hyp")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, E , "s1.typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, "")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "s2.isGoal")
    })
    
    it("+s1, +s2, s1.jstf=':ax-mp' -> s1.typ=P, s1.isGoal=T, s2.typ=P, s2.isGoal=F", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, s1) = addNewStmt(st)
        let (st, s2) = addNewStmt(st)

        //when
        let st = st->completeJstfEditMode(s1, ":ax-mp")

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEq( editorGetStmtByIdExn(st,s1).jstfText, ":ax-mp")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "s1.isGoal")

        assertEqMsg( editorGetStmtByIdExn(st,s2).typ, P , "s2.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s2).isGoal, false, "s2.isGoal")
    })
})

describe("defaults for G steps", _ => {
    it("the very first step is not marked G when the setting is false", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")
    })

    it("the very first step is marked G when the setting is true", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "s1.isGoal")
    })

    it("the very first step is not labeled qed when the setting is empty", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false, ~defaultStmtLabel="")

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "1", "s1.label")
    })

    it("the very first step is labeled qed when the setting is _qed_", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false, ~defaultStmtLabel=" qed ")

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "qed", "s1.label")
    })
    
    it("if there is a hyp step then the newly added step is not marked G when the setting is false", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false)
        let (st, h1) = addNewStmt(st)
        let st = st->completeTypEditMode(h1,E,false)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.isGoal")
    })

    it("if there is a hyp step then the newly added step is marked G when the setting is true", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, h1) = addNewStmt(st)
        let st = st->completeTypEditMode(h1,E,false)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true, "s1.isGoal")
    })

    it("if there is a hyp step then the newly added step is not labeled qed when the setting is empty", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false, ~defaultStmtLabel="  ")
        let (st, h1) = addNewStmt(st)
        let st = st->completeTypEditMode(h1,E,false)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "2", "s1.label")
    })

    it("if there is a hyp step then the newly added step is labeled qed when the setting is qed", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=false, ~defaultStmtLabel="qed")
        let (st, h1) = addNewStmt(st)
        let st = st->completeTypEditMode(h1,E,false)
        let st = st->completeLabelEditMode(h1,"hyp.1")

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "qed", "s1.label")
    })

    it("if there is a hyp step labeled qed then the newly added step is labeled qed1 when the setting is qed", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true, ~defaultStmtLabel="qed")
        let (st, h1) = addNewStmt(st)
        let st = st->completeTypEditMode(h1,E,false)
        assertEqMsg( editorGetStmtByIdExn(st,h1).typ, E, "h1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,h1).isGoal, false, "h1.isGoal")

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, true , "s1.isGoal")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "qed1", "s1.label")
    })

    it("if there is another P step then the newly added step is not marked G when the setting is true", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true)
        let (st, p1) = addNewStmt(st)
        let st = st->completeLabelEditMode(p1,"p1")
        assertEq( editorGetStmtByIdExn(st,p1).typ, P)
        assertEq( editorGetStmtByIdExn(st,p1).isGoal, true)

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).isGoal, false, "s1.label")
    })

    it("if there is another P step then the newly added step is not labeled qed when the setting is qed", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true, ~defaultStmtLabel="qed")
        let (st, p1) = addNewStmt(st)
        assertEq( editorGetStmtByIdExn(st,p1).typ, P)
        assertEq( editorGetStmtByIdExn(st,p1).label, "qed")
        let st = st->completeLabelEditMode(p1,"p1")
        assertEq( editorGetStmtByIdExn(st,p1).label, "p1")

        //when
        let (st, s1) = addNewStmt(st)

        //then
        assertEqMsg( editorGetStmtByIdExn(st,s1).typ, P , "s1.typ")
        assertEqMsg( editorGetStmtByIdExn(st,s1).label, "1", "s1.label")
    })

    it("duplication of G step produces P step", _ => {
        //given
        let st = createEditorState(demo0, ~initStmtIsGoal=true, ~defaultStmtLabel="qed")
        let (st, s1) = addNewStmt(st)
        assertEq( (st.stmts->Array.getUnsafe(0)).typ, P)
        assertEq( (st.stmts->Array.getUnsafe(0)).isGoal, true)
        assertEq( (st.stmts->Array.getUnsafe(0)).label, "qed")

        //when
        let st = st->toggleStmtChecked(s1)
        let st = st->duplicateCheckedStmt(false)
        let st = st->verifyEditorState

        //then
        assertEqMsg( (st.stmts->Array.getUnsafe(0)).typ, P , "st.stmts[0].typ")
        assertEqMsg( (st.stmts->Array.getUnsafe(0)).isGoal, false , "st.stmts[0].isGoal")
        assertEqMsg( (st.stmts->Array.getUnsafe(0)).label, "1" , "st.stmts[0].label")
        assertEqMsg( (st.stmts->Array.getUnsafe(1)).typ, P , "st.stmts[1].typ")
        assertEqMsg( (st.stmts->Array.getUnsafe(1)).isGoal, true , "st.stmts[1].isGoal")
        assertEqMsg( (st.stmts->Array.getUnsafe(1)).label, "qed" , "st.stmts[1].label")
    })
})

describe("deleteUnrelatedSteps", _ => {
    it("deletes all unrelated steps except the goal step and hypotheses", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, idQed1) = addNewStmt(st)
        let (st, id1) = addNewStmt(st)
        let (st, id2) = addNewStmt(st)
        let (st, id3) = addNewStmt(st)
        let (st, id4) = addNewStmt(st)
        let (st, id5) = addNewStmt(st)
        let (st, id6) = addNewStmt(st)
        let (st, id7) = addNewStmt(st)
        let (st, idQed) = addNewStmt(st)
        let (st, id8) = addNewStmt(st)
        let st = updateStmt(st, idQed1, stmt => { ...stmt, 
            label:"qed.1", typ:E, isGoal:false, cont:strToCont("|- ( &W2 -> &W1 )")
        })
        let st = updateStmt(st, id1, stmt => { ...stmt, 
            label:"1", typ:P, isGoal:false, jstfText: "", cont:strToCont("|- &W2")
        })
        let st = updateStmt(st, id2, stmt => { ...stmt, 
            label:"2", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( t + 0 ) = t")
        })
        let st = updateStmt(st, id3, stmt => { ...stmt, 
            label:"3", typ:P, isGoal:false, jstfText: "1 qed.1 : mp", cont:strToCont("|- &W1")
        })
        let st = updateStmt(st, id4, stmt => { ...stmt, 
            label:"4", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( ( t + 0 ) = t -> ( ( t + 0 ) = t -> t = t ) )")
        })
        let st = updateStmt(st, id5, stmt => { ...stmt, 
            label:"5", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( &T4 + 0 ) = &T4")
        })
        let st = updateStmt(st, id6, stmt => { ...stmt, 
            label:"6", typ:P, isGoal:false, jstfText: "2 4 : mp", cont:strToCont("|- ( ( t + 0 ) = t -> t = t )")
        })
        let st = updateStmt(st, id7, stmt => { ...stmt, 
            label:"7", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( &T2 = &T3 -> ( &T2 = &T1 -> &T3 = &T1 ) )")
        })
        let st = updateStmt(st, idQed, stmt => { ...stmt, 
            label:"qed", typ:P, isGoal:false, jstfText: "2 6 : mp", cont:strToCont("|- t = t")
        })
        let st = updateStmt(st, id8, stmt => { ...stmt, 
            label:"8", typ:P, isGoal:true, jstfText: ": a2", cont:strToCont("|- ( &T6 + 0 ) = &T6")
        })
        let st = completeVarsEditMode(st, "var1 term &T1\n var2 term &T2\n var3 term &T3\n var4 term &T4\n" 
                ++ " var5 wff &W1\n var6 wff &W2\n var8 term &T6\n")
        assertEqMsg(
            st.stmts->Array.map(stmt => stmt.id),
            [ idQed1, id1, id2, id3, id4, id5, id6, id7, idQed, id8, ],
            "before"
        )

        //when
        switch deleteUnrelatedSteps(st, ~stepIdsToKeep=[idQed], ~deleteHyps=false) {
            | Error(msg) => failMsg("Could not remove unrelated steps because of the error: " ++ msg)
            | Ok(state) => {
                //then
                assertEqMsg(
                    state.stmts->Array.map(stmt => stmt.id),
                    [ idQed1, id2, id4, id6, idQed, id8, ],
                    "after"
                )
            }
        }
    })

    it("deletes unrelated hypotheses", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, idQed1) = addNewStmt(st)
        let (st, id1) = addNewStmt(st)
        let (st, id2) = addNewStmt(st)
        let (st, id3) = addNewStmt(st)
        let (st, id4) = addNewStmt(st)
        let (st, id5) = addNewStmt(st)
        let (st, id6) = addNewStmt(st)
        let (st, id7) = addNewStmt(st)
        let (st, idQed) = addNewStmt(st)
        let (st, id8) = addNewStmt(st)
        let st = updateStmt(st, idQed1, stmt => { ...stmt, 
            label:"qed.1", typ:E, isGoal:false, cont:strToCont("|- ( &W2 -> &W1 )")
        })
        let st = updateStmt(st, id1, stmt => { ...stmt, 
            label:"1", typ:P, isGoal:false, jstfText: "", cont:strToCont("|- &W2")
        })
        let st = updateStmt(st, id2, stmt => { ...stmt, 
            label:"2", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( t + 0 ) = t")
        })
        let st = updateStmt(st, id3, stmt => { ...stmt, 
            label:"3", typ:P, isGoal:false, jstfText: "1 qed.1 : mp", cont:strToCont("|- &W1")
        })
        let st = updateStmt(st, id4, stmt => { ...stmt, 
            label:"4", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( ( t + 0 ) = t -> ( ( t + 0 ) = t -> t = t ) )")
        })
        let st = updateStmt(st, id5, stmt => { ...stmt, 
            label:"5", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( &T4 + 0 ) = &T4")
        })
        let st = updateStmt(st, id6, stmt => { ...stmt, 
            label:"6", typ:P, isGoal:false, jstfText: "2 4 : mp", cont:strToCont("|- ( ( t + 0 ) = t -> t = t )")
        })
        let st = updateStmt(st, id7, stmt => { ...stmt, 
            label:"7", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( &T2 = &T3 -> ( &T2 = &T1 -> &T3 = &T1 ) )")
        })
        let st = updateStmt(st, idQed, stmt => { ...stmt, 
            label:"qed", typ:P, isGoal:false, jstfText: "2 6 : mp", cont:strToCont("|- t = t")
        })
        let st = updateStmt(st, id8, stmt => { ...stmt, 
            label:"8", typ:P, isGoal:true, jstfText: ": a2", cont:strToCont("|- ( &T6 + 0 ) = &T6")
        })
        let st = completeVarsEditMode(st, "var1 term &T1\n var2 term &T2\n var3 term &T3\n var4 term &T4\n" 
                ++ " var5 wff &W1\n var6 wff &W2\n var8 term &T6\n")
        assertEqMsg(
            st.stmts->Array.map(stmt => stmt.id),
            [ idQed1, id1, id2, id3, id4, id5, id6, id7, idQed, id8, ],
            "before"
        )

        //when
        switch deleteUnrelatedSteps(st, ~stepIdsToKeep=[idQed], ~deleteHyps=true) {
            | Error(msg) => failMsg("Could not remove unrelated steps because of the error: " ++ msg)
            | Ok(state) => {
                //then
                assertEqMsg(
                    state.stmts->Array.map(stmt => stmt.id),
                    [ id2, id4, id6, idQed, id8, ],
                    "after"
                )
            }
        }
    })

    it("doesn't delete bookmarked steps", _ => {
        //given
        let st = createEditorState(demo0)
        let (st, idQed1) = addNewStmt(st)
        let (st, id1) = addNewStmt(st)
        let (st, id2) = addNewStmt(st)
        let (st, id3) = addNewStmt(st)
        let (st, id4) = addNewStmt(st)
        let (st, id5) = addNewStmt(st)
        let (st, id6) = addNewStmt(st)
        let (st, id7) = addNewStmt(st)
        let (st, idQed) = addNewStmt(st)
        let (st, id8) = addNewStmt(st)
        let st = updateStmt(st, idQed1, stmt => { ...stmt, 
            label:"qed.1", typ:E, isGoal:false, cont:strToCont("|- ( &W2 -> &W1 )")
        })
        let st = updateStmt(st, id1, stmt => { ...stmt, 
            label:"1", typ:P, isGoal:false, jstfText: "", cont:strToCont("|- &W2")
        })
        let st = updateStmt(st, id2, stmt => { ...stmt, 
            label:"2", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( t + 0 ) = t")
        })
        let st = updateStmt(st, id3, stmt => { ...stmt, 
            label:"3", typ:P, isGoal:false, jstfText: "1 qed.1 : mp", cont:strToCont("|- &W1")
        })
        let st = updateStmt(st, id4, stmt => { ...stmt, 
            label:"4", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( ( t + 0 ) = t -> ( ( t + 0 ) = t -> t = t ) )")
        })
        let st = updateStmt(st, id5, stmt => { ...stmt, 
            label:"5", typ:P, isGoal:false, jstfText: ": a2", cont:strToCont("|- ( &T4 + 0 ) = &T4")
        })
        let st = updateStmt(st, id6, stmt => { ...stmt, 
            label:"6", typ:P, isGoal:false, jstfText: "2 4 : mp", cont:strToCont("|- ( ( t + 0 ) = t -> t = t )")
        })
        let st = updateStmt(st, id7, stmt => { ...stmt, 
            label:"7", typ:P, isGoal:false, jstfText: ": a1", 
            cont:strToCont("|- ( &T2 = &T3 -> ( &T2 = &T1 -> &T3 = &T1 ) )")
        })
        let st = updateStmt(st, idQed, stmt => { ...stmt, 
            label:"qed", typ:P, isGoal:false, jstfText: "2 6 : mp", cont:strToCont("|- t = t")
        })
        let st = updateStmt(st, id8, stmt => { ...stmt, 
            label:"8", typ:P, isGoal:false, isBkm:true, jstfText: ": a2", cont:strToCont("|- ( &T6 + 0 ) = &T6")
        })
        let st = completeVarsEditMode(st, "var1 term &T1\n var2 term &T2\n var3 term &T3\n var4 term &T4\n" 
                ++ " var5 wff &W1\n var6 wff &W2\n var8 term &T6\n")
        assertEqMsg(
            st.stmts->Array.map(stmt => stmt.id),
            [ idQed1, id1, id2, id3, id4, id5, id6, id7, idQed, id8, ],
            "before"
        )

        //when
        switch deleteUnrelatedSteps(st, ~stepIdsToKeep=[idQed], ~deleteHyps=false) {
            | Error(msg) => failMsg("Could not remove unrelated steps because of the error: " ++ msg)
            | Ok(state) => {
                //then
                assertEqMsg(
                    state.stmts->Array.map(stmt => stmt.id),
                    [ idQed1, id2, id4, id6, idQed, id8, ],
                    "after"
                )
            }
        }
    })
})