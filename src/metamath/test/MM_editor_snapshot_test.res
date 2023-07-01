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

    let updateStmt = (a:editorSnapshot,stmtId:stmtId, update:stmtSnapshot=>stmtSnapshot):editorSnapshot => {
        {
            ...a,
            stmts:a.stmts->Js_array2.map(stmt => if (stmt.id == stmtId) {stmt->update} else {stmt})
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
    })
})