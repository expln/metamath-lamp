open Expln_test
open MM_wrk_pre_ctx_data
open MM_parser
open MM_wrk_LoadCtx

describe("createMmScopesForFrame", _ => {

    it("1 src, read all", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })

    it("1 src, stop after, case 1", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopAfter",
                label: "E",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })

    it("1 src, stop after, case 2", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopAfter",
                label: "D",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })

    it("1 src, stop before, case 1", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopBefore",
                label: "F",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })

    it("1 src, stop before, case 2", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopBefore",
                label: "E",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })
    
    it("2 srcs, frm is in src1, ReadAll", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let ast2 = { begin: 0, end: 0, stmt: Comment({text:"2"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast2),
                allLabels: ["X", "Y"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="C")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 2 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("C") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })
    
    it("2 srcs, frm is in src1, StopBefore", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let ast2 = { begin: 0, end: 0, stmt: Comment({text:"2"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopBefore",
                label: "E",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast2),
                allLabels: ["X", "Y", "Z"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="C")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 2 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("C") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })
    
    it("2 srcs, frm is in src1, StopAfter, the last requested", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let ast2 = { begin: 0, end: 0, stmt: Comment({text:"2"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopAfter",
                label: "D",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast2),
                allLabels: ["X", "Y", "Z"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="D")

        //then
        assertEq( mmScopes->Array.length, 1 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 3 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, false )
    })
    
    it("2 srcs, frm is in src2, no intersection", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let ast2 = { begin: 0, end: 0, stmt: Comment({text:"2"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopAfter",
                label: "D",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast2),
                allLabels: ["X", "Y", "Z"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="Y")

        //then
        assertEq( mmScopes->Array.length, 2 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 4 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, true )

        assertEq( (mmScopes->Array.getUnsafe(1)).ast === ast2, true )
        assertEq( (mmScopes->Array.getUnsafe(1)).expectedNumOfAssertions, 1 )
        assertEq( (mmScopes->Array.getUnsafe(1)).stopBefore, Some("Y") )
        assertEq( (mmScopes->Array.getUnsafe(1)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(1)).resetNestingLevel, false )
    })
    
    it("2 srcs, frm is in src2, intersection", _ => {
        //given
        let ast1 = { begin: 0, end: 0, stmt: Comment({text:"1"}) }
        let ast2 = { begin: 0, end: 0, stmt: Comment({text:"2"}) }
        let srcs = [
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "StopAfter",
                label: "D",
                ast: Some(ast1),
                allLabels: ["A", "B", "C", "D", "E", "F"],
                resetNestingLevel:true,
            },
            {
                typ: "----",
                fileName: "======",
                url: "........",
                readInstr: "ReadAll",
                label: "",
                ast: Some(ast2),
                allLabels: ["E", "F"],
                resetNestingLevel:true,
            },
        ]

        //when
        let mmScopes = createMmScopesForFrame(~srcs, ~label="E")

        //then
        assertEq( mmScopes->Array.length, 2 )

        assertEq( (mmScopes->Array.getUnsafe(0)).ast === ast1, true )
        assertEq( (mmScopes->Array.getUnsafe(0)).expectedNumOfAssertions, 4 )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopBefore, None )
        assertEq( (mmScopes->Array.getUnsafe(0)).stopAfter, Some("D") )
        assertEq( (mmScopes->Array.getUnsafe(0)).resetNestingLevel, true )

        assertEq( (mmScopes->Array.getUnsafe(1)).ast === ast2, true )
        assertEq( (mmScopes->Array.getUnsafe(1)).expectedNumOfAssertions, 0 )
        assertEq( (mmScopes->Array.getUnsafe(1)).stopBefore, Some("E") )
        assertEq( (mmScopes->Array.getUnsafe(1)).stopAfter, None )
        assertEq( (mmScopes->Array.getUnsafe(1)).resetNestingLevel, false )
    })
})
