open Expln_test
open MM_editor_snapshot
open MM_wrk_editor

describe("findDiff", _ => {
    let a = {
        {
            descr: "descr",
            varsText: "varsText",
            disjText: "disjText",
            stmts: [
                { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
                { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
            ],
        }
    }

    let updateStmt = (a:editorSnapshot, stmtId:stmtId, update:stmtSnapshot=>stmtSnapshot):editorSnapshot => {
        {
            ...a,
            stmts:a.stmts->Js_array2.map(stmt => if (stmt.id == stmtId) {stmt->update} else {stmt})
        }
    }

    let addStmt = (a:editorSnapshot, atIdx:int, stmt:stmtSnapshot):editorSnapshot => {
        let newStmts = a.stmts->Js_array2.copy
        newStmts->Js.Array2.spliceInPlace(~pos=atIdx, ~remove=0, ~add=[stmt])->ignore
        {
            ...a,
            stmts:newStmts
        }
    }

    let removeStmt = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => {
        {
            ...a,
            stmts:a.stmts->Js.Array2.filter(stmt => stmt.id != stmtId)
        }
    }

    let moveStmt = (a:editorSnapshot, stmtId:stmtId):editorSnapshot => {
        let newStmts = a.stmts->Js_array2.copy
        let idx = newStmts->Js.Array2.findIndex(stmt => stmt.id == stmtId)
        let tmp = newStmts[idx]
        newStmts[idx] = newStmts[idx+1]
        newStmts[idx+1] = tmp
        {
            ...a,
            stmts:newStmts
        }
    }

    it("finds diffs", _ => {
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
            findDiff(a, a->updateStmt("3", stmt => {...stmt, jstfText: "jstfText-new"})), 
            [StmtJstf({stmtId: "3", jstfText: "jstfText-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("1", stmt => {...stmt, cont: "cont-new"})), 
            [StmtCont({stmtId: "1", cont: "cont-new"})]
        )
        assertEq( 
            findDiff(a, a->updateStmt("2", stmt => {...stmt, proofStatus: None})), 
            [StmtStatus({stmtId: "2", proofStatus: None})]
        )

        assertEq( 
            findDiff(a, a->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 0, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(1, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })), 
            [StmtAdd({idx: 1, stmt: { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(2, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 2, stmt: { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )
        assertEq( 
            findDiff(a, a->addStmt(3, { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None })), 
            [StmtAdd({idx: 3, stmt: { id: "4", label: "label4", typ: P, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: None }})]
        )

        assertEq( findDiff(a, a->removeStmt("1")), [StmtRemove({stmtId: "1"})] )
        assertEq( findDiff(a, a->removeStmt("2")), [StmtRemove({stmtId: "2"})] )
        assertEq( findDiff(a, a->removeStmt("3")), [StmtRemove({stmtId: "3"})] )

        assertEq( findDiff(a, a->moveStmt("1")), [StmtMove({stmtId: "1", idx: 1})] )
        assertEq( findDiff(a, a->moveStmt("2")), [StmtMove({stmtId: "2", idx: 2})] )

        assertEq( 
            findDiff(a, a->removeStmt("1")->removeStmt("2")), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                        ],
                    }
                )
            ] 
        )

        assertEq( 
            findDiff(
                a, 
                a
                    ->addStmt(0, { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) })
                    ->addStmt(2, { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) })
            ), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "4", label: "label4", typ: E, isGoal: true, jstfText: "jstfText4", cont: "cont4", proofStatus: Some(Ready) },
                            { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
                            { id: "5", label: "label5", typ: P, isGoal: false, jstfText: "jstfText5", cont: "cont5", proofStatus: Some(Waiting) },
                            { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                        ],
                    }
                )
            ] 
        )

        assertEq( 
            findDiff( a, a->moveStmt("1")->moveStmt("1") ), 
            [
                Snapshot(
                    {
                        descr: "descr",
                        varsText: "varsText",
                        disjText: "disjText",
                        stmts: [
                            { id: "2", label: "label2", typ: P, isGoal: false, jstfText: "jstfText2", cont: "cont2", proofStatus: Some(Ready) },
                            { id: "3", label: "label3", typ: P, isGoal: true, jstfText: "jstfText3", cont: "cont3", proofStatus: Some(Waiting) },
                            { id: "1", label: "label1", typ: E, isGoal: false, jstfText: "jstfText1", cont: "cont1", proofStatus: None },
                        ],
                    }
                )
            ] 
        )
    })
})