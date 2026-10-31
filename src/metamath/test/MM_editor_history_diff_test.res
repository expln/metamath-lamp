open Expln_test
open MM_wrk_editor
open! MM_editor_snapshot_diff

let a:editorSnapshot = {
    {
        descr: "descr",
        varsText: "varsText",
        disjText: "disjText",
        stmts: [
            { id: "1", label: "label1", typ: E, isGoal: false, isBkm: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
            { id: "2", label: "label2", typ: P, isGoal: false, isBkm: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
            { id: "3", label: "label3", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
        ],
    }
}

let moveStmtTest = (a:editorSnapshot, stmtId:stmtId, dIdx:int):editorSnapshot => {
    let newStmts = a.stmts->Array.copy
    let idx = newStmts->Array.findIndex(stmt => stmt.id == stmtId)
    let tmp = newStmts->Array.getUnsafe(idx)
    newStmts[idx] = newStmts->Array.getUnsafe(idx+dIdx)
    newStmts[idx+dIdx] = tmp
    {
        ...a,
        stmts:newStmts
    }
}

let moveStmtUp = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => moveStmtTest(a, stmtId, -1)
let moveStmtDown = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => moveStmtTest(a, stmtId, 1)

let testApplyDiff = (
    ~initState:editorSnapshot,
    ~changes:editorSnapshot=>editorSnapshot,
    ~expectedEndState:editorSnapshot
):unit => {
    //given
    let diff = initState->findDiff(initState->changes)
    assertEq(diff->Array.length > 0, true)

    //when
    let actualEndState = initState->applyDiff(diff)

    //then
    assertEq(actualEndState, expectedEndState)
}

describe("findDiff", _ => {
    it("finds diffs", _ => {
        assertEq( findDiff(a, {...a, descr: a.descr}), [] )

        assertEq( findDiff(a, {...a, descr: "descr-new"}), [Descr("descr-new")] )
        assertEq( findDiff(a, {...a, varsText: "varsText-new"}), [Vars("varsText-new")] )
        assertEq( findDiff(a, {...a, disjText: "disjText-new"}), [Disj("disjText-new")] )

        assertEq( 
            findDiff(a, a->updateStmt("1", stmt => {...stmt, label:"label-new"})), 
            [StmtLabel({stmtId: "1", label: "label-new"})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, typ:E})), 
            [StmtTyp({stmtId: "2", typ: E, isGoal: false})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, isGoal:true})), 
            [StmtTyp({stmtId: "2", typ: P, isGoal: true})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})), 
            [StmtTyp({stmtId: "2", typ: E, isGoal: true})] 
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, isBkm:true})), 
            [StmtBkm({stmtIds: ["2"], isBkm: true})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("3", stmt => {...stmt, jstfText: "jstfText-new"})), 
            [StmtJstf({stmtId: "3", jstfText: "jstfText-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("1", stmt => {...stmt, cont: "cont-new"})), 
            [StmtCont({stmtId: "1", cont: "cont-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, proofStatus: None})), 
            [StmtStatus({stmtIds: ["2"], proofStatus: None})]
        )

        assertEq( 
            findDiff(
                {...a, stmts:[]}, 
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
            ), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 1, stmt: { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 2, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 3, stmt: { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )

        assertEq( findDiff(a, a->removeStmt("1")), [StmtRemove({stmtIds: ["1"]})] )
        assertEq( findDiff(a, a->removeStmt("2")), [StmtRemove({stmtIds: ["2"]})] )
        assertEq( findDiff(a, a->removeStmt("3")), [StmtRemove({stmtIds: ["3"]})] )
        assertEq( 
            findDiff(
                {...a, stmts:[]}->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
                {...a, stmts:[]},
            ), 
            [StmtRemove({stmtIds: ["4"]})]
        )

        assertEq( findDiff(a, a->moveStmtDown("1")), [StmtMove({stmtId: "2", idx: 0})] )
        assertEq( findDiff(a, a->moveStmtDown("2")), [StmtMove({stmtId: "3", idx: 1})] )

        assertEq( 
            findDiff(a, a->removeStmt("1")->removeStmt("2")), 
            [
                StmtRemove({stmtIds: ["1","2"]}),
            ] 
        )

        assertEq( 
            findDiff(
                a, 
                a
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtAdd({idx: 2, stmt: { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) }}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtDown("1") ), 
            [
                StmtMove({stmtId: "2", idx: 0}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtDown("1")->moveStmtDown("1") ), 
            [
                StmtMove({stmtId: "2", idx: 0}),
                StmtMove({stmtId: "3", idx: 1}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtUp("3") ), 
            [
                StmtMove({stmtId: "3", idx: 1}),
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmtUp("3")->moveStmtUp("3") ), 
            [
                StmtMove({stmtId: "3", idx: 0}),
            ] 
        )

        assertEq( 
            findDiff(
                a
                    ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:None}),
                a
                    ->updateStmt("1", stmt => {...stmt, proofStatus:Some(Ready)})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:Some(Waiting)})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:Some(JstfIsIncorrect)}),
            ), 
            [
                StmtStatus({stmtIds: ["1"], proofStatus: Some(Ready)}),
                StmtStatus({stmtIds: ["2"], proofStatus: Some(Waiting)}),
                StmtStatus({stmtIds: ["3"], proofStatus: Some(JstfIsIncorrect)}),
            ] 
        )

        assertEq( 
            findDiff(
                a,
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT", isBkm:true}),
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtLabel({stmtId: "1", label: "ABC"}),
                StmtTyp({stmtId: "2", typ:E, isGoal:true}),
                StmtJstf({stmtId: "3", jstfText:"BBB"}),
                StmtCont({stmtId: "3", cont:"TTTTT"}),
                StmtBkm({stmtIds: ["3"], isBkm:true}),
                StmtStatus({stmtIds: ["1"], proofStatus: Some(NoJstf)}),
                Descr("descr-new"),
                Vars("varsText-new"),
                Disj("disjText-new"),
            ] 
        )

        assertEq( 
            findDiff(
                a,
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf), isBkm:true})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) }),
            ), 
            [
                StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }}),
                StmtAdd({idx: 4, stmt: { id: "5", label: "label5", typ: E, isGoal: true, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) }}),
                StmtLabel({stmtId: "1", label: "ABC"}),
                StmtTyp({stmtId: "2", typ:E, isGoal:true}),
                StmtJstf({stmtId: "3", jstfText:"BBB"}),
                StmtCont({stmtId: "3", cont:"TTTTT"}),
                StmtBkm({stmtIds: ["1"], isBkm: true}),
                StmtStatus({stmtIds: ["1"], proofStatus: Some(NoJstf)}),
                Descr("descr-new"),
                Vars("varsText-new"),
                Disj("disjText-new"),
            ], 
        )
    })
})

describe("applyDiff", _ => {
    it("applies diffs", _ => {
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, descr: "descr-new"},
            ~expectedEndState = {...a, descr: "descr-new"}
        )
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, varsText: "varsText-new"},
            ~expectedEndState = {...a, varsText: "varsText-new"}
        )
        testApplyDiff( ~initState=a,
            ~changes = sn => {...sn, disjText: "disjText-new"},
            ~expectedEndState = {...a, disjText: "disjText-new"}
        )

        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "1", stmt => {...stmt, label:"label-new"}),
            ~expectedEndState = a->updateStmt("1", stmt => {...stmt, label:"label-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, typ:E}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, typ:E})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, isGoal:true}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, isGoal:true})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, typ:E, isGoal:true}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "3", stmt => {...stmt, isBkm:true}),
            ~expectedEndState = a->updateStmt("3", stmt => {...stmt, isBkm:true})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "3", stmt => {...stmt, jstfText: "jstfText-new"}),
            ~expectedEndState = a->updateStmt("3", stmt => {...stmt, jstfText: "jstfText-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "1", stmt => {...stmt, cont: "cont-new"}),
            ~expectedEndState = a->updateStmt("1", stmt => {...stmt, cont: "cont-new"})
        )
        testApplyDiff( ~initState=a,
            ~changes = updateStmt(_, "2", stmt => {...stmt, proofStatus: None}),
            ~expectedEndState = a->updateStmt("2", stmt => {...stmt, proofStatus: None})
        )

        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }),
            ~expectedEndState = a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
        )
        testApplyDiff( ~initState=a,
            ~changes = addStmt(_, 3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None }),
            ~expectedEndState = a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: None })
        )

        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "1"),
            ~expectedEndState = a->removeStmt("1")
        )
        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "2"),
            ~expectedEndState = a->removeStmt("2")
        )
        testApplyDiff( ~initState=a,
            ~changes = removeStmt(_, "3"),
            ~expectedEndState = a->removeStmt("3")
        )

        testApplyDiff( ~initState=a,
            ~changes = moveStmtDown(_, "1"),
            ~expectedEndState = a->moveStmtDown("1")
        )
        testApplyDiff( ~initState=a,
            ~changes = moveStmtDown(_, "2"),
            ~expectedEndState = a->moveStmtDown("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->removeStmt("1")->removeStmt("2"),
            ~expectedEndState = a->removeStmt("1")->removeStmt("2")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                sn
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            },
            ~expectedEndState = a
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, isBkm: true, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => sn->moveStmtDown("1")->moveStmtDown("1"),
            ~expectedEndState = a->moveStmtDown("1")->moveStmtDown("1")
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                sn
                    ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                    ->updateStmt("3", stmt => {...stmt, proofStatus:None})
            },
            ~expectedEndState = a
                ->updateStmt("1", stmt => {...stmt, proofStatus:None})
                ->updateStmt("2", stmt => {...stmt, proofStatus:None})
                ->updateStmt("3", stmt => {...stmt, proofStatus:None})
        )

        testApplyDiff( ~initState=a,
            ~changes = sn => {
                {...sn, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
        )

        testApplyDiff( ~initState=a,
            ~changes = _ => {
                {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                    ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                    ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                    ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
            },
            ~expectedEndState = {...a, descr: "descr-new", varsText: "varsText-new", disjText: "disjText-new", }
                ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                ->updateStmt("1", stmt => {...stmt, label:"ABC", proofStatus:Some(NoJstf)})
                ->updateStmt("2", stmt => {...stmt, typ:E, isGoal:true})
                ->updateStmt("3", stmt => {...stmt, jstfText:"BBB", cont:"TTTTT"})
                ->addStmt(4, { id: "5", label: "label5", typ: E, isGoal: true, isBkm: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Ready) })
        )
    })
})
