open Expln_test
open MM_int_test_utils
open MM_int_test_editor_methods

module Ed = MM_int_test_editor_methods

describe("MM_wrk_editor integration tests: proofs", _ => {
    it("prove reccot", _ => {
        setTestDataDir("prove-reccot")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="reccsc", ~debug)

        let (st, trgtStmtId) = st->addStmt(
            ~label="reccot", 
            ~stmt="|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( 1 / ( cot ` A ) ) )"
        )
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch( ~filterLabel="tanval", ~chooseLabel="tanval" )
        assertEditorState(st, "step2")

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A", ~useMatching=true)
        let tanvalId = st->getStmtId(~contains="-> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) )")
        assertEditorState(st, "step3")

        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->addStmtsBySearch( ~filterLabel="cotval", ~chooseLabel="cotval" )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A", ~useMatching=true)
        let cotvalId = st->getStmtId(~contains="-> ( cot ` A ) = ( ( cos ` A ) / ( sin ` A ) )")
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let st = st->addStmtsBySearch(~filterPattern="class =/= 0 /\\ class =/= 0 -> 1 /", ~chooseLabel="recdiv" )
        let st = st->unifyAll
        assertEditorState(st, "step6")

        let (st, tanvalCopy1Id) = st->duplicateStmt(tanvalId)
        let st = st->updateStmt(tanvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0"
        )
        let st = st->unifyAll
        assertEditorState(st, "step7")

        let (st, cotvalCopy1Id) = st->duplicateStmt(cotvalId)
        let st = st->updateStmt(cotvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0"
        )
        let st = st->unifyAll
        assertEditorState(st, "step8")

        let (st, cotvalCopy2Id) = st->duplicateStmt(cotvalCopy1Id)
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( cot ` A )",
            ~contReplaceWith="( 1 / ( cot ` A ) )"
        )
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( ( cos ` A ) / ( sin ` A ) )",
            ~contReplaceWith="( 1 / ( ( cos ` A ) / ( sin ` A ) ) )"
        )
        let st = st->unifyAll
        assertEditorState(st, "step9")

        let st = st->applySubstitution(
            ~replaceWhat="1 / ( class1 / class2 )", 
            ~replaceWith="1 / ( ( cos ` A ) / ( sin ` A ) )",
            ~useMatching=true
        )
        let st = st->unifyAll
        assertEditorState(st, "step10")

        let (st, _) = st->addStmt(
            ~before=(st.stmts->Array.getUnsafe(0)).id,
            ~stmt="|- ( A e. CC -> ( cos ` A ) e. CC )"
        )
        let st = st->unifyAll
        assertEditorState(st, "step11")

        let st = st->addStmtsBySearch(~filterLabel="sincl", ~chooseLabel="sincl" )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A", ~useMatching=true)
        let st = st->unifyAll
        assertEditorState(st, "step12")

        let result1Id = st->getStmtId(~contains="1 / ( ( cos ` A ) / ( sin ` A ) ) ) = ( ( sin ` A ) / ( cos ` A ) ) )")
        let (st, result2Id) = st->duplicateStmt(result1Id)
        let st = st->updateStmt(result2Id, 
            ~contReplaceWhat="( cos ` A ) e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( cos ` A ) =/= 0"
        )

        let (st, result3Id) = st->duplicateStmt(result2Id)
        let st = st->updateStmt(result3Id, 
            ~contReplaceWhat="( sin ` A ) e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0"
        )
        let st = st->unifyAll
        assertEditorState(st, "step13")

        let (st, result4Id) = st->duplicateStmt(result3Id)
        let st = st->updateStmt(result4Id, 
            ~contReplaceWhat="( A e. CC /\\ ( cos ` A ) =/= 0 ) /\\ ( A e. CC /\\ ( sin ` A ) =/= 0 )",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0"
        )
        let st = st->unifyAll
        assertEditorState(st, "step14")
        assertProof(st, trgtStmtId, "proof1-no-hyps")

        let hyp1Id = (st.stmts->Array.getUnsafe(0)).id
        let hyp2Id = (st.stmts->Array.getUnsafe(1)).id

        let st = st->updateStmt(hyp1Id, ~typ=E, ~label=_=>"hyp1" )
        let st = st->updateStmt(hyp2Id, ~typ=E, ~label=_=>"hyp2" )
        let st = st->unifyAll
        assertEditorState(st, "step15")
        assertProof(st, trgtStmtId, "proof2-hyps")
        
        let st = st->updateStmt(hyp1Id, ~typ=P )
        let st = st->updateStmt(hyp2Id, ~typ=P )
        let st = st->removeAllJstf
        assertEditorState(st, "step16-no-jstf-before-unify-all")

        let st = st->unifyAll
        assertEditorState(st, "step17-after-unify-all")
        assertProof(st, trgtStmtId, "proof3-no-hyps")
    })

    it("prove nfv", _ => {
        setTestDataDir("prove-nfv")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="nfv", ~debug)

        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> ph )" )
        let (st, _) = st->addStmt( ~stmt="|- ( ph -> A. x ph )" )
        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> A. x ph )" )
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- F/ x ph" )
        let st = st->unifyAll
        assertEditorState(st, "step1")
        
        let st = st->addDisj("x ph")
        let st = st->unifyAll
        assertEditorState(st, "step2")
        assertProof(st, trgtStmtId, "proof1")

    })

    it("filtering bottom-up proof results in two ways for xmulasslem", _ => {
        setTestDataDir("bottom-up-xmulasslem")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" )

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="xmulasslem")
        let resultsWhenLabelParamIsNotSpecified = stmts1->getSingleStmtsDto->newStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "xmulasslem")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="xmulasslem", ~chooseLabel="xmulasslem")
        let resultsWhenLabelParamIsSpecified = stmts2->getSingleStmtsDto->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof results in two ways for fvmpt", _ => {
        setTestDataDir("bottom-up-fvmpt")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" )

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="fvmpt")
        let resultsWhenLabelParamIsNotSpecified = stmts1->getSingleStmtsDto->newStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "fvmpt")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="fvmpt", ~chooseLabel="fvmpt")
        let resultsWhenLabelParamIsSpecified = stmts2->getSingleStmtsDto->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof has same results for search depth 3 and 4", _ => {
        setTestDataDir("bottom-up-ifbieq2d")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( x = A -> if ( x = 0 , 0 , if ( x < 0 , -u 1 , 1 ) ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" )
        let st = st->addDisj("x A")

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=3, ~allowNewVars=false, ~chooseLabel="ifbieq2d")
        let resultsWhenDepth3 = stmts1->getSingleStmtsDto->newStmtsDtoToStr

        assertTextEqFile(resultsWhenDepth3, "ifbieq2d")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=4, ~allowNewVars=false, ~chooseLabel="ifbieq2d")
        let resultsWhenDepth4 = stmts2->getSingleStmtsDto->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenDepth3, "resultsWhenDepth3", 
            resultsWhenDepth4, "resultsWhenDepth4"
        )
    })

    it("prove sgnval", _ => {
        setTestDataDir("prove-sgnval")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestrict=Less, ~chooseLabel="fvmpt")
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch(
            ~addBefore=st->getStmtId(~contains="|- sgn ="),
            ~filterLabel="sgn",
            ~chooseLabel="df-sgn"
        )
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->applySubstitution(
            ~replaceWhat="|- sgn = ( setvar1 e. RR* |-> class1 )",
            ~replaceWith="|- sgn = ( setvar2 e. RR* |-> if ( setvar2 = 0 , 0 , if ( setvar2 < 0 , -u 1 , 1 ) ) )",
            ~useMatching=true
        )
        assertEditorState(st, "step3")

        let st = st->Ed.mergeStmt(
            st->Ed.getStmtId(~predicate=stmt=>stmt.jstfText == ": df-sgn")
        )
        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->Ed.applySubstitution( ~replaceWhat="setvar2", ~replaceWith="x", ~useMatching=true)
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->Ed.getStmtId(~contains="|- ( x = A -> if"), 
            ~args0=NoneStmts,
            ~allowNewVars=false, 
            ~chooseLabel="ifbieq2d"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step6")
        assertProof(st, trgtStmtId, "sgnval-proof")

    })

    it("prove sgnval with hyps", _ => {
        setTestDataDir("prove-sgnval-with-hyps")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestrict=Less, ~chooseLabel="fvmpt")
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let notProvedStmtId = st->getStmtId(~label="1")

        let st = st.stmts->Array.reduce(
            st,
            (st,stmt) => {
                if (trgtStmtId == stmt.id || notProvedStmtId == stmt.id) {
                    st
                } else {
                    st->updateStmt(stmt.id, ~typ=E, ~label = oldLabel => "hyp-" ++ oldLabel)
                }
            }
        )
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestrict=Less,
            ~chooseResult=stmts=>stmts.newVars->Array.length==0
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step3")

        let state2Text = readTestFileToString("step2")
        let state3Text = readTestFileToString("step3")

        assertEq(state2Text, state3Text)

    })

    it("prove bottom-up does not produce duplicated results", _ => {
        setTestDataDir("prove-bottom-up-no-duplicates")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug)

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )" 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestrict=Less, ~chooseLabel="fvmpt")
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let (_, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestrict=Less,
            ~chooseResult = dto => dto.newVars->Array.length != 0
        )
        assertStmtsDto(stmts->getSingleStmtsDto, "stmtsDto")
    })

    it("prove sgn0e0", _ => {
        setTestDataDir("prove-sgn0e0")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="sgnval", ~debug)

        let (st, trgtStmtId) = st->addStmt( ~label="sgn0e0", ~stmt="|- ( sgn ` 0 ) = 0" )

        let st = st->addStmtsBySearch( ~filterLabel="sgn", ~chooseLabel="sgnval" )
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->applySubstitution( ~replaceWhat="class1", ~replaceWith="0", ~useMatching=true)
        assertEditorState(st, "step2")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=trgtStmtId, 
            ~maxSearchDepth=6,
            ~allowNewVars=false, 
            ~args1=AllStmts,
            ~chooseLabel="ax-mp"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step3")
        assertProof(st, trgtStmtId, "sgn0e0-proof")

    })

    it("prove 95p1e96", _ => {
        setTestDataDir("prove-95p1e96")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug)

        let (st, trgtStmtId) = st->addStmt( 
            ~label="95p1e96",
            ~stmt="|- ( ; 9 5 + 1 ) = ; 9 6" 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId, 
            ~maxSearchDepth=1, ~lengthRestrict=No, ~allowNewVars=true, ~chooseLabel="decsuc")
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="5", ~useMatching=true)
        let st = st->unifyAll
        assertEditorState(st, "step2")

        assertProof(st, trgtStmtId, "95p1e96-proof")
    })

    it("set jstf for root stmts when applying bottom-up results", _ => {
        setTestDataDir("set-jstf-for-root-stmts")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="sgnval", ~debug)

        let (st, trgtStmtId) = st->addStmt( ~label="sgn0e0", ~stmt="|- ( sgn ` 0 ) = 0" )
        let st = st->addStmtsBySearch( ~filterLabel="sgn", ~chooseLabel="sgnval" )
        let st = st->applySubstitution( ~replaceWhat="class1", ~replaceWith="0", ~useMatching=true)
        let (st, _) = st->addStmt( ~before=st->Ed.getStmtId(~contains="|- ( 0 e. RR*"),
            ~stmt="|- ( ( sgn ` 0 ) = if ( 0 = 0 , 0 , if ( 0 < 0 , -u 1 , 1 ) ) <-> ( sgn ` 0 ) = 0 )" )
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=trgtStmtId,
            ~asrtLabel="mp2",
            ~maxSearchDepth=4,
            ~allowNewVars=false,
            ~chooseLabel="mp2"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step2")
        assertProof(st, trgtStmtId, "proof")
    })

    it("bottom-up prover should be able to restore missing disjoints", _ => {
        setTestDataDir("restore-missing-disjoints")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->deleteStmts([st->getStmtId(~label="description")])
        assertEditorState(st, "step1.0")

        let st = st->updateStmt(st->getStmtId(~label="stmt3-brab2a.21"), ~jstf="")
        assertEditorState(st, "step1.1")

        let st = st->updateStmt(st->getStmtId(~label="stmt14"), ~jstf="")
        assertEditorState(st, "step1.2")

        let st = st->updateStmt(st->getStmtId(~label="stmt12"), ~jstf="")
        assertEditorState(st, "step1.3")

        let st = st->unifyAll
        assertEditorState(st, "step3")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt14"),
            ~asrtLabel="ralbidv",
            ~maxSearchDepth=4,
            ~chooseLabel="ralbidv"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step4")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt12"),
            ~asrtLabel="2ralbidv",
            ~maxSearchDepth=4,
            ~chooseLabel="2ralbidv"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt1"),
            ~args0=SomeStmts([st->getStmtId(~label="stmt8")]),
            ~maxSearchDepth=4,
            ~allowNewVars=false,
            ~chooseLabel="mp1i"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step6-all-proved")

        let st = st->setDisj("")
        let st = st->unifyAll
        assertEditorState(st, "step7")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt2"),
            ~args1=AllStmts,
            ~asrtLabel="spcev",
            ~maxSearchDepth=40,
            ~lengthRestrict=No,
            ~allowNewStmts=false,
            ~chooseLabel="spcev"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step8")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt3"),
            ~args1=AllStmts,
            ~asrtLabel="brab2a",
            ~maxSearchDepth=40,
            ~lengthRestrict=No,
            ~allowNewStmts=false,
            ~chooseLabel="brab2a"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step9")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="2"),
            ~args1=AllStmts,
            ~asrtLabel="tgjustf",
            ~maxSearchDepth=40,
            ~lengthRestrict=No,
            ~allowNewStmts=false,
            ~chooseLabel="tgjustf"
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step10-disj-restored")

    })
    
    it("bottom-up prover does not add unnecessary disjoints", _ => {
        setTestDataDir("no-unnecessary-disjoints")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->unifyAll
        assertEditorState(st, "step0")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="vtocl2d2"),
            ~maxSearchDepth=4,
            ~allowNewStmts=false
        )
        let st = st->addNewStmts(stmts->getSingleStmtsDto)
        let st = st->unifyAll
        assertEditorState(st, "step1")
    })

    it("unify-all identifies some types of unification errors", _ => {
        setTestDataDir("identify-errors")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->updateStmt(st->getStmtId(~label="stmt11"), ~jstf=": elex")
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->updateStmt(st->getStmtId(~label="stmt11"), ~jstf=": ereq1")
        let st = st->updateStmt(st->getStmtId(~label="stmt3-brab2a.11"), ~jstf="stmt4 stmt5 : brab2a")
        let st = st->unifyAll
        assertEditorState(st, "step3")

        let st = st->updateStmt(st->getStmtId(~label="stmt3-brab2a.11"), ~jstf="stmt4 stmt5 : eqeq12d")
        let st = st->updateStmt(st->getStmtId(~label="stmt9"), 
            ~jstf="stmt9-opabex2.11 stmt9-opabex2.31 stmt9-opabex2.11 stmt9-opabex2.41: opabex2")
        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->updateStmt(st->getStmtId(~label="stmt9"), 
            ~jstf="stmt9-opabex2.11 stmt9-opabex2.11 stmt9-opabex2.31 stmt9-opabex2.41: opabex2")
        let st = st->setDisj("x y v u A\nx y v u F r")
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let st = st->setDisj("x y v u A r\nx y v u F r")
        let st = st->unifyAll
        assertEditorState(st, "step6")

    })

    it("unify-all identifies syntax errors", _ => {
        setTestDataDir("syntax-errors")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->updateStmt(st->getStmtId(~label="stmt9-opabex2.11"), ~content="|- ( A e. _V -> psi A e. _V )")
        let st = st->updateStmt(st->getStmtId(~label="stmt4"), ~content="|- ( ( u = x /\ v = y ) -> ( F ` u ) = ( F ` x ) ) ]")
        let st = st->updateStmt(st->getStmtId(~label="stmt6.1"), ~content="|- ( u = x -> ( psi A ` u ) = ( psi A ` x ) )")
        let st = st->unifyAll
        assertEditorState(st, "step2")

    })

    it("updateEditorStateWithPostupdateActions removes redundant variables from disjoints", _ => {
        setTestDataDir("no-redundant-vars-in-disj")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->setVars(".var1 class width")
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->applySubstitution(~replaceWhat="A", ~replaceWith="width", ~useMatching=true)
        let st = st->unifyAll
        assertEditorState(st, "step3")
    })

    it("unify all finds required justification if more than one correct justification exist", _ => {
        setTestDataDir("two-valid-proofs")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug)

        let (st, stmtId) = st->addStmt( ~jstf=": wi", ~stmt="wff ( ( ph -> ps ) -> ( ph -> ch ) )" )
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->updateStmt(stmtId, ~jstf=": bj-0" )
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->updateStmt(stmtId, ~jstf=": wi" )
        let st = st->unifyAll
        assertEditorState(st, "step3")

        let st = st->updateStmt(stmtId, ~jstf=": bj-0" )
        let st = st->unifyAll
        assertEditorState(st, "step4")
    })

    it("actExportProof does not export redundant elements", _ => {
        setTestDataDir("no-redundant-elems-in-export")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state")
        let st = st->unifyAll
        assertEditorState(st, "step1")
        assertProof(st, st->getStmtId(~label="stmt5"), "proof-no-redundant-vars")
        assertProof(st, st->getStmtId(~label="3"), "proof-no-redundant-disj")
        assertProof(st, st->getStmtId(~label="stmt11"), "proof-no-redundant-hyps")
    })

    it("actExportProof exports implicit local vars", _ => {
        setTestDataDir("implicit-loc-vars-in-export")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state", 
            ~stopBefore="cvjust")
        let st = st->unifyAll
        assertEditorState(st, "step1")
        assertProof(st, st->getStmtId(~label="cvjust"), "cvjust")
    })

    it("findPossibleSubs is able to find substitutions by unification", _ => {
        setTestDataDir("findPossibleSubs")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="bj-0", ~debug, ~editorState="editor-initial-state")
        assertEditorState(st, "step1")
        let ctx = st.wrkCtx->Belt_Option.getExn

        let unifSubsToMap = (wrkSubs:MM_wrk_editor.wrkSubs):Belt_MapString.t<string> => {
            wrkSubs.subs->Belt_MapInt.toArray
                ->Array.map(((e,expr)) => (ctx->MM_context.ctxIntToSymExn(e), ctx->MM_context.ctxIntsToStrExn(expr)))
                ->Belt_MapString.fromArray
        }

        let assertUnifSubs = (
            ~testCaseName:string,
            ~actualUnifSubs:result<array<MM_wrk_editor.wrkSubs>,string>, 
            ~expectedUnifSubs:Belt_MapString.t<string>
        ):unit => {
            switch actualUnifSubs {
                | Error(msg) => failMsg(`assertUnifSubs failed for '${testCaseName}': ${msg}`)
                | Ok([wrkSubs]) => {
                    let actualSubsMap = unifSubsToMap(wrkSubs)
                    expectedUnifSubs->Belt_MapString.forEach((expectedVar, expectedSubExpr) => {
                        switch actualSubsMap->Belt_MapString.get(expectedVar) {
                            | None => failMsg(`assertUnifSubs failed for '${testCaseName}': actualSubsMap doesn't contain a substitution for ${expectedVar}`)
                            | Some(actualSubExpr) => {
                                assertEqMsg(
                                    expectedVar ++ " \u2192 " ++ actualSubExpr,
                                    expectedVar ++ " \u2192 " ++ expectedSubExpr,
                                    `assertUnifSubs failed for '${testCaseName}'`
                                )
                            }
                        }
                    })
                }
                | Ok(_) => failMsg(`assertUnifSubs failed for '${testCaseName}': actualUnifSubs.length != 1`)
            }
        }
        
        let testUnifSubs = (
            ~expr1:string,
            ~expr2:string,
            ~expected:array<(string,string)>
        ):unit => {
            assertUnifSubs(
                ~testCaseName = expr1 ++ " <<<~>>> " ++ expr2,
                ~actualUnifSubs = st->MM_wrk_editor_substitution.findPossibleSubs(
                    ctx->MM_context.ctxStrToIntsExn(expr1), 
                    ctx->MM_context.ctxStrToIntsExn(expr2), 
                    false
                ), 
                ~expectedUnifSubs=expected->Belt_MapString.fromArray
            )
        }

        testUnifSubs( ~expr1="&W1", ~expr2="ph", ~expected = [ ("&W1", "ph") ] )
        testUnifSubs( ~expr1="|- &W1", ~expr2="|- ph", ~expected = [ ("&W1", "ph") ] )
        testUnifSubs( ~expr1="&C1", ~expr2="A", ~expected = [ ("&C1", "A") ] )
        testUnifSubs( ~expr1="class &C1", ~expr2="class A", ~expected = [ ("&C1", "A") ] )
        testUnifSubs( ~expr1="&S1", ~expr2="x", ~expected = [ ("&S1", "x") ] )
        testUnifSubs( ~expr1="setvar &S1", ~expr2="setvar x", ~expected = [ ("&S1", "x") ] )
        testUnifSubs( ~expr1="&C1", ~expr2="x", ~expected = [ ("&C1", "x") ] )
        testUnifSubs( ~expr1="class &C1", ~expr2="setvar x", ~expected = [ ("&C1", "x") ] )
        testUnifSubs( ~expr1="&C1", ~expr2="&S1", ~expected = [ ("&C1", "&S1") ] )
        testUnifSubs( ~expr1="class &C1", ~expr2="setvar &S1", ~expected = [ ("&C1", "&S1") ] )

        testUnifSubs( 
            ~expr1="( ( &W3 -> ( &W4 -> &W2 ) ) -> ( ( &W3 -> &W4 ) -> ( &W3 -> &W2 ) ) )", 
            ~expr2="( &W1                       -> ( ( ph  -> ps  ) -> ( ph  -> ch  ) ) )", 
            ~expected = [ 
                ("&W3", "ph"),
                ("&W4", "ps"),
                ("&W2", "ch"),
                ("&W1", "( ph -> ( ps -> ch ) )"),
            ] 
        )

        testUnifSubs( 
            ~expr1="|- ( ( &W3 -> ( &W4 -> &W2 ) ) -> ( ( &W3 -> &W4 ) -> ( &W3 -> &W2 ) ) )", 
            ~expr2="|- ( &W1                       -> ( ( ph  -> ps  ) -> ( ph  -> ch  ) ) )", 
            ~expected = [ 
                ("&W3", "ph"),
                ("&W4", "ps"),
                ("&W2", "ch"),
                ("&W1", "( ph -> ( ps -> ch ) )"),
            ] 
        )
    })

    it("autoMergeDuplicatedStatements", _ => {
        setTestDataDir("autoMergeDuplicatedStatements")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="bj-0", ~debug, ~editorState="editor-initial-state")
        let st = {...st, settings:{...st.settings, autoMergeStmts:true}}
        let st = st->MM_wrk_editor.verifyEditorState
        assertEditorState(st, "step1")

        let st = st->applySubstitution(
            ~replaceWhat="|- ( ( &W3 -> ( &W4 -> &W2 ) ) -> ( ( &W3 -> &W4 ) -> ( &W3 -> &W2 ) ) )", 
            ~replaceWith="|- ( &W1 -> ( ( ph -> ps ) -> ( ph -> ch ) ) )",
            ~useMatching=false,
        )
        assertEditorState(st, "step2")

        let (st, d1) = st->duplicateStmt(st->getStmtId(~label="qed"))
        assertEditorState(st, "step3")

        let st = st->updateStmt(d1, ~content="|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) ) +")
        assertEditorState(st, "step4")

        let st = st->updateStmt(d1, ~content="|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) )")
        assertEditorState(st, "step5")

        let st = MM_wrk_editor.uncheckAllStmts(st)
        let st = MM_wrk_editor.toggleStmtChecked(st,st->getStmtId(~label="syl.2"))
        let (st, newId) = MM_wrk_editor.addNewStmt(st)
        let st = st->MM_wrk_editor.setStmtCont( newId, "|- ( ps -> ch )"->MM_wrk_editor.strToCont )
        let st = MM_wrk_editor.uncheckAllStmts(st)
        let st = st->MM_wrk_editor.verifyEditorState
        assertEditorState(st, "step6")

        let st = st->updateStmt(newId, ~content="|- ( ps -> ch ) +")
        assertEditorState(st, "step7")

        let st = st->updateStmt(newId, ~content="|- ( ps -> ch )")
        assertEditorState(st, "step8")

        let st = st->deleteStmts([newId])
        let st = MM_wrk_editor.uncheckAllStmts(st)
        let st = MM_wrk_editor.toggleStmtChecked(st,st->getStmtId(~label="qed"))
        let (st, newId2) = MM_wrk_editor.addNewStmt(st)
        let st = st->MM_wrk_editor.setStmtCont( 
            newId2, 
            "|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) )"->MM_wrk_editor.strToCont 
        )
        let st = MM_wrk_editor.uncheckAllStmts(st)
        let st = st->MM_wrk_editor.verifyEditorState
        assertEditorState(st, "step9")

        let st = st->updateStmt(newId2, ~content="|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) ) +")
        assertEditorState(st, "step10")

        let st = st->updateStmt(newId2, ~content="|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) )")
        assertEditorState(st, "step11")
    })

    it("175-Unify-hangs-for-certain-statements", _ => {
        setTestDataDir("175-Unify-hangs-for-certain-statements")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state", ~stopBefore="mathbox")
        let st = st->unifyAll
        assertEditorState(st, "step1")
    })

    it("bottomUpProverFrameParams.frameParams.matches filters by result and hypotheses", _ => {
        setTestDataDir("frameParams-matches")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state", ~stopBefore="mathbox")
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let defaultFrameParams:MM_provers.bottomUpProverFrameParams = {
            minDist: None, maxDist: None,
            matches: None,
            frmsToUse: Some([]),
            args: [
                getStmt(st, ~label="hyp1").expr->Belt.Option.getExn,
                getStmt(st, ~label="hyp2").expr->Belt.Option.getExn,
            ],
            allowNewDisjForExistingVars:false,
            allowNewStmts:true,
            allowNewVars: false,
            lengthRestrict: No,
            maxNumberOfBranches: None,
        }

        let prepareBottomUpProverParams = (
            state:MM_wrk_editor.editorState,
            matcher:MM_cmp_api.apiApplyAsrtResultMatcher,
        ):MM_provers.bottomUpProverParams => {
            {
                maxSearchDepth:3,
                frameParams: [
                    {
                        ...defaultFrameParams,
                        frmsToUse: Some(["qcn"]),
                        matches: None,
                    },
                    {
                        ...defaultFrameParams,
                        frmsToUse: Some(["syl"]),
                        matches: MM_cmp_api.optArrayToMatchers(~state, ~matches=Some([matcher]))->Belt_Result.getExn,
                    }
                ],
            }
        }

        let assertResults = (actual:array<MM_statements_dto.stmtsDto>, expectedFileName:string) => {
            assertTextEqFile(
                actual->Array.map(newStmtsDtoToStr)
                ->Array.joinUnsafe("\n\n----------------------------------\n"), 
                expectedFileName
            )
        }

        let (st, foundProofsResultOnly) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="qed"), 
            ~bottomUpProverParams=prepareBottomUpProverParams( st,
                {hyps:[], res:Some("|- ( ph -> X e. A )")}
            )
        )
        assertResults(foundProofsResultOnly, "foundProofsResultOnly")

        let (st, foundProofsResultAndHyps) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="qed"), 
            ~bottomUpProverParams=prepareBottomUpProverParams( st,
                {
                    hyps:[
                        {label:None, idx:Some(0), pat:"|- ( ph -> X e. A )"},
                        {label:None, idx:Some(1), pat:"|- ( X e. A -> X e. B )"},
                    ],
                    res:Some("|- ( ph -> X e. B )")
                }
            )
        )
        assertResults(foundProofsResultAndHyps, "foundProofsResultAndHyps")

        let (st, foundProofsHypsOnly) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="qed"), 
            ~bottomUpProverParams=prepareBottomUpProverParams( st,
                {
                    hyps:[
                        {label:None, idx:Some(0), pat:"|- ( ph -> X e. A )"},
                        {label:None, idx:Some(1), pat:"|- ( X e. A -> X e. B )"},
                    ],
                    res:None
                }
            )
        )
        assertResults(foundProofsHypsOnly, "foundProofsHypsOnly")

        let (st, foundProofsHyp0Only) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="qed"), 
            ~bottomUpProverParams=prepareBottomUpProverParams( st,
                {
                    hyps:[
                        {label:None, idx:Some(0), pat:"|- ( ph -> X e. A )"},
                    ],
                    res:None
                }
            )
        )
        assertResults(foundProofsHyp0Only, "foundProofsHyp0Only")

        let (_, foundProofsHyp1Only) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="qed"), 
            ~bottomUpProverParams=prepareBottomUpProverParams( st,
                {
                    hyps:[
                        {label:None, idx:Some(1), pat:"|- ( X e. A -> X e. B )"},
                    ],
                    res:None
                }
            )
        )
        assertResults(foundProofsHyp1Only, "foundProofsHyp1Only")

    })

    // it("bottom-up prover should not find missing disjoints if allowNewDisjForExistingVars==false", _ => {
    // })
})

// describe("prepare set.mm without proofs", _ => {
//     it("prepare set.mm without proofs", _ => {
//         generateReducedMmFile(
//             ~pathToFullMmFile="/books/metamath/set.mm",
//             ~pathToSaveTo="./src/metamath/test/resources/set-no-proofs._mm",
//             ~skipComments=true,
//             ~skipProofs=true,
//             ()
//         )
//     })
// })
