open Expln_test
open MM_int_test_utils
open MM_int_test_editor_methods

module Ed = MM_int_test_editor_methods

describe("MM_wrk_editor integration tests", _ => {
    it("prove reccot", _ => {
        setTestDataDir("prove-reccot")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="reccsc", ~debug, ())

        let (st, trgtStmtId) = st->addStmt(
            ~label="reccot", 
            ~stmt="|- ( ( A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0 ) -> ( tan ` A ) = ( 1 / ( cot ` A ) ) )",
            ()
        )
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch( ~filterLabel="tanval", ~chooseLabel="tanval", () )
        assertEditorState(st, "step2")

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let tanvalId = st->getStmtId(~contains="-> ( tan ` A ) = ( ( sin ` A ) / ( cos ` A ) )", ())
        assertEditorState(st, "step3")

        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->addStmtsBySearch( ~filterLabel="cotval", ~chooseLabel="cotval", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let cotvalId = st->getStmtId(~contains="-> ( cot ` A ) = ( ( cos ` A ) / ( sin ` A ) )", ())
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let st = st->addStmtsBySearch(~filterPattern="class =/= 0 /\\ class =/= 0 -> 1 /", ~chooseLabel="recdiv", () )
        let st = st->unifyAll
        assertEditorState(st, "step6")

        let (st, tanvalCopy1Id) = st->duplicateStmt(tanvalId)
        let st = st->updateStmt(tanvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step7")

        let (st, cotvalCopy1Id) = st->duplicateStmt(cotvalId)
        let st = st->updateStmt(cotvalCopy1Id, 
            ~contReplaceWhat="A e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step8")

        let (st, cotvalCopy2Id) = st->duplicateStmt(cotvalCopy1Id)
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( cot ` A )",
            ~contReplaceWith="( 1 / ( cot ` A ) )",
            ()
        )
        let st = st->updateStmt(cotvalCopy2Id, 
            ~contReplaceWhat="( ( cos ` A ) / ( sin ` A ) )",
            ~contReplaceWith="( 1 / ( ( cos ` A ) / ( sin ` A ) ) )",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step9")

        let st = st->applySubstitution(
            ~replaceWhat="1 / ( class1 / class2 )", 
            ~replaceWith="1 / ( ( cos ` A ) / ( sin ` A ) )"
        )
        let st = st->unifyAll
        assertEditorState(st, "step10")

        let (st, _) = st->addStmt(
            ~before=st.stmts[0].id,
            ~stmt="|- ( A e. CC -> ( cos ` A ) e. CC )",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step11")

        let st = st->addStmtsBySearch(~filterLabel="sincl", ~chooseLabel="sincl", () )
        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="A")
        let st = st->unifyAll
        assertEditorState(st, "step12")

        let result1Id = st->getStmtId(~contains="1 / ( ( cos ` A ) / ( sin ` A ) ) ) = ( ( sin ` A ) / ( cos ` A ) ) )", ())
        let (st, result2Id) = st->duplicateStmt(result1Id)
        let st = st->updateStmt(result2Id, 
            ~contReplaceWhat="( cos ` A ) e. CC /\\ ( cos ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( cos ` A ) =/= 0",
            ()
        )

        let (st, result3Id) = st->duplicateStmt(result2Id)
        let st = st->updateStmt(result3Id, 
            ~contReplaceWhat="( sin ` A ) e. CC /\\ ( sin ` A ) =/= 0",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step13")

        let (st, result4Id) = st->duplicateStmt(result3Id)
        let st = st->updateStmt(result4Id, 
            ~contReplaceWhat="( A e. CC /\\ ( cos ` A ) =/= 0 ) /\\ ( A e. CC /\\ ( sin ` A ) =/= 0 )",
            ~contReplaceWith="A e. CC /\\ ( sin ` A ) =/= 0 /\\ ( cos ` A ) =/= 0",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step14")
        assertProof(st, trgtStmtId, "proof1-no-hyps")

        let hyp1Id = st.stmts[0].id
        let hyp2Id = st.stmts[1].id

        let st = st->updateStmt(hyp1Id, ~typ=E, ~label=_=>"hyp1", () )
        let st = st->updateStmt(hyp2Id, ~typ=E, ~label=_=>"hyp2", () )
        let st = st->unifyAll
        assertEditorState(st, "step15")
        assertProof(st, trgtStmtId, "proof2-hyps")
        
        let st = st->updateStmt(hyp1Id, ~typ=P, () )
        let st = st->updateStmt(hyp2Id, ~typ=P, () )
        let st = st->removeAllJstf
        assertEditorState(st, "step16-no-jstf-before-unify-all")

        let st = st->unifyAll
        assertEditorState(st, "step17-after-unify-all")
        assertProof(st, trgtStmtId, "proof3-no-hyps")
    })

    it("prove nfv", _ => {
        setTestDataDir("prove-nfv")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopBefore="nfv", ~debug, ())

        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( ph -> A. x ph )", () )
        let (st, _) = st->addStmt( ~stmt="|- ( E. x ph -> A. x ph )", () )
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- F/ x ph", () )
        let st = st->unifyAll
        assertEditorState(st, "step1")
        
        let st = st->addDisj("x,ph")
        let st = st->unifyAll
        assertEditorState(st, "step2")
        assertProof(st, trgtStmtId, "proof1")

    })

    it("filtering bottom-up proof results in two ways for xmulasslem", _ => {
        setTestDataDir("bottom-up-xmulasslem")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="xmulasslem", ())
        let resultsWhenLabelParamIsNotSpecified = stmts1->newStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "xmulasslem")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="xmulasslem", ~chooseLabel="xmulasslem", ())
        let resultsWhenLabelParamIsSpecified = stmts2->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof results in two ways for fvmpt", _ => {
        setTestDataDir("bottom-up-fvmpt")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~chooseLabel="fvmpt", ())
        let resultsWhenLabelParamIsNotSpecified = stmts1->newStmtsDtoToStr

        assertTextEqFile(resultsWhenLabelParamIsNotSpecified, "fvmpt")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=1, ~asrtLabel="fvmpt", ~chooseLabel="fvmpt", ())
        let resultsWhenLabelParamIsSpecified = stmts2->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenLabelParamIsNotSpecified, "resultsWhenLabelParamIsNotSpecified", 
            resultsWhenLabelParamIsSpecified, "resultsWhenLabelParamIsSpecified"
        )
    })

    it("filtering bottom-up proof has same results for search depth 3 and 4", _ => {
        setTestDataDir("bottom-up-ifbieq2d")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let st = st->addDisj("x,A")
        let (st, trgtStmtId) = st->addStmt( ~stmt="|- ( x = A -> if ( x = 0 , 0 , if ( x < 0 , -u 1 , 1 ) ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", () )

        let (st, stmts1) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=3, ~allowNewVars=false, ~chooseLabel="ifbieq2d", ())
        let resultsWhenDepth3 = stmts1->newStmtsDtoToStr

        assertTextEqFile(resultsWhenDepth3, "ifbieq2d")

        let (_, stmts2) = st->unifyBottomUp(~stmtId=trgtStmtId, ~maxSearchDepth=4, ~allowNewVars=false, ~chooseLabel="ifbieq2d", ())
        let resultsWhenDepth4 = stmts2->newStmtsDtoToStr

        assertTextsEq(
            resultsWhenDepth3, "resultsWhenDepth3", 
            resultsWhenDepth4, "resultsWhenDepth4"
        )
    })

    it("prove sgnval", _ => {
        setTestDataDir("prove-sgnval")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", 
            () 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, ~chooseLabel="fvmpt", ())
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->addStmtsBySearch(
            ~addBefore=st->getStmtId(~contains="|- sgn =", ()),
            ~filterLabel="sgn",
            ~chooseLabel="df-sgn",
            ()
        )
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->applySubstitution(
            ~replaceWhat="|- sgn = ( setvar1 e. RR* |-> class1 )",
            ~replaceWith="|- sgn = ( setvar2 e. RR* |-> if ( setvar2 = 0 , 0 , if ( setvar2 < 0 , -u 1 , 1 ) ) )",
        )
        assertEditorState(st, "step3")

        let st = st->Ed.mergeStmt(
            st->Ed.getStmtId(~predicate=stmt=>stmt.jstfText == ": df-sgn", ())
        )
        let st = st->unifyAll
        assertEditorState(st, "step4")

        let st = st->Ed.applySubstitution( ~replaceWhat="setvar2", ~replaceWith="x" )
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->Ed.getStmtId(~contains="|- ( x = A -> if", ()), 
            ~args0=None,
            ~allowNewVars=false, 
            ~chooseLabel="ifbieq2d",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step6")
        assertProof(st, trgtStmtId, "sgnval-proof")

    })

    it("prove sgnval with hyps", _ => {
        setTestDataDir("prove-sgnval-with-hyps")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", 
            () 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, ~chooseLabel="fvmpt", ())
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let notProvedStmtId = st->getStmtId(~label="1", ())

        let st = st.stmts->Js_array2.reduce(
            (st,stmt) => {
                if (trgtStmtId == stmt.id || notProvedStmtId == stmt.id) {
                    st
                } else {
                    st->updateStmt(stmt.id, ~typ=E, ~label = oldLabel => "hyp-" ++ oldLabel, ())
                }
            },
            st
        )
        let st = st->unifyAll
        assertEditorState(st, "step2")

        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, 
            ~chooseResult=stmts=>stmts.newVars->Js_array2.length==0, 
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step3")

        let state2Text = readEditorStateToString("step2")
        let state3Text = readEditorStateToString("step3")

        assertEq(state2Text, state3Text)

    })

    it("prove bottom-up does not produce duplicated results", _ => {
        setTestDataDir("prove-bottom-up-no-duplicates")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="df-sgn", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( 
            ~label="sgnval",
            ~stmt="|- ( A e. RR* -> ( sgn ` A ) = if ( A = 0 , 0 , if ( A < 0 , -u 1 , 1 ) ) )", 
            () 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, ~chooseLabel="fvmpt", ())
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId,
            ~asrtLabel="fvmpt", ~maxSearchDepth=4, ~lengthRestriction=Less, 
            ~chooseResult = dto => dto.newVars->Js_array2.length != 0,
            ()
        )
        assertStmtsDto(stmts, "stmtsDto")
    })

    it("prove sgn0e0", _ => {
        setTestDataDir("prove-sgn0e0")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="sgnval", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( ~label="sgn0e0", ~stmt="|- ( sgn ` 0 ) = 0", () )

        let st = st->addStmtsBySearch( ~filterLabel="sgn", ~chooseLabel="sgnval", () )
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->applySubstitution( ~replaceWhat="class1", ~replaceWith="0", )
        assertEditorState(st, "step2")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=trgtStmtId, 
            ~maxSearchDepth=6,
            ~allowNewVars=false, 
            ~args1=All,
            ~chooseLabel="ax-mp",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step3")
        assertProof(st, trgtStmtId, "sgn0e0-proof")

    })

    it("prove 95p1e96", _ => {
        setTestDataDir("prove-95p1e96")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ())

        let (st, trgtStmtId) = st->addStmt( 
            ~label="95p1e96",
            ~stmt="|- ( ; 9 5 + 1 ) = ; 9 6", 
            () 
        )
        let (st, stmts) = st->unifyBottomUp(~stmtId=trgtStmtId, 
            ~maxSearchDepth=1, ~lengthRestriction=No, ~allowNewVars=true, ~chooseLabel="decsuc", ())
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let st = st->applySubstitution(~replaceWhat="class1", ~replaceWith="5")
        let st = st->unifyAll
        assertEditorState(st, "step2")

        assertProof(st, trgtStmtId, "95p1e96-proof")
    })

    it("set jstf for root stmts when applying bottom-up results", _ => {
        setTestDataDir("set-jstf-for-root-stmts")
        let st = createEditorState(~mmFilePath=setMmPath, ~stopAfter="sgnval", ~debug, ())

        let (st, trgtStmtId) = st->addStmt( ~label="sgn0e0", ~stmt="|- ( sgn ` 0 ) = 0", () )
        let st = st->addStmtsBySearch( ~filterLabel="sgn", ~chooseLabel="sgnval", () )
        let st = st->applySubstitution( ~replaceWhat="class1", ~replaceWith="0", )
        let (st, _) = st->addStmt( ~before=st->Ed.getStmtId(~contains="|- ( 0 e. RR*", ()),
            ~stmt="|- ( ( sgn ` 0 ) = if ( 0 = 0 , 0 , if ( 0 < 0 , -u 1 , 1 ) ) <-> ( sgn ` 0 ) = 0 )", () )
        let st = st->unifyAll
        assertEditorState(st, "step1")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=trgtStmtId,
            ~asrtLabel="mp2",
            ~maxSearchDepth=4,
            ~allowNewVars=false,
            ~chooseLabel="mp2",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step2")
        assertProof(st, trgtStmtId, "proof")
    })

    it("bottom-up prover should be able to restore missing disjoints", _ => {
        setTestDataDir("restore-missing-disjoints")
        let st = createEditorState(~mmFilePath=setMmPath, ~debug, ~editorState="editor-initial-state", 
            ~asrtsToSkipFilePath, ())
        let st = st->deleteStmts([st->getStmtId(~label="description", ())])
        assertEditorState(st, "step1")

        let st = st->unifyAll
        assertEditorState(st, "step2")

        let st = st->updateStmt(st->getStmtId(~label="stmt3-brab2a.21", ()), ~jstf="", ())
        let st = st->unifyAll
        assertEditorState(st, "step3")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt14", ()),
            ~asrtLabel="ralbidv",
            ~maxSearchDepth=4,
            ~chooseLabel="ralbidv",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step4")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt12", ()),
            ~asrtLabel="2ralbidv",
            ~maxSearchDepth=4,
            ~chooseLabel="2ralbidv",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step5")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt1", ()),
            ~args0=Some([st->getStmtId(~label="stmt8", ())]),
            ~maxSearchDepth=4,
            ~allowNewVars=false,
            ~chooseLabel="mp1i",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step6-all-proved")

        let st = st->removeDisj("y,k,A,u,r,v,F")
        let st = st->removeDisj("k,r,F")
        let st = st->removeDisj("A,k,r,F")
        let st = st->removeDisj("u,k,A,r,F")
        let st = st->removeDisj("F,r")
        let st = st->removeDisj("v,k,A,u,r,F")
        let st = st->removeDisj("x,y,k,A,u,r,v,F")
        let st = st->removeDisj("r,u,v,x,y,A")
        let st = st->removeDisj("r,u,v,x,y,F")
        let st = st->unifyAll
        assertEditorState(st, "step7")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt2", ()),
            ~args1=All,
            ~asrtLabel="spcev",
            ~maxSearchDepth=40,
            ~lengthRestriction=No,
            ~allowNewStmts=false,
            ~chooseLabel="spcev",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step8")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="stmt3", ()),
            ~args1=All,
            ~asrtLabel="brab2a",
            ~maxSearchDepth=40,
            ~lengthRestriction=No,
            ~allowNewStmts=false,
            ~chooseLabel="brab2a",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step9")

        let (st, stmts) = st->unifyBottomUp(
            ~stmtId=st->getStmtId(~label="2", ()),
            ~args1=All,
            ~asrtLabel="tgjustf",
            ~maxSearchDepth=40,
            ~lengthRestriction=No,
            ~allowNewStmts=false,
            ~chooseLabel="tgjustf",
            ()
        )
        let st = st->addNewStmts(stmts, ())
        let st = st->unifyAll
        assertEditorState(st, "step10-disj-restored")

    })

    // it("bottom-up prover should not find missing disjoints if allowNewDisjForExistingVars==false", _ => {
    // })
})

// describe("prepare set.mm without proofs", _ => {
//     it("prepare set.mm without proofs", _ => {
//         generateReducedMmFile(
//             ~pathToFullMmFile="C:/Users/Igor/igye/books/metamath/set.mm",
//             ~pathToSaveTo="./src/metamath/test/resources/set-no-proofs.mm",
//             ~skipComments=true,
//             ~skipProofs=true,
//             ()
//         )
//     })
// })
