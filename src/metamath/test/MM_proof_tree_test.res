open Expln_test
open MM_proof_tree
open MM_context
open MM_parenCounter

let createSrc = (hypTypes:array<hypothesisType>, args:array<expr>, label:string):exprSrc => {
    let proofTree = ptMake(
        ~proofCtx = createContext(),
        ~frms = MM_substitution.frmsEmpty(),
        ~parenCnt = parenCntMake(~parenMin=0, ~canBeFirstMin=0, ~canBeFirstMax=0, ~canBeLastMin=0, ~canBeLastMax=0),
    )
    Assertion({
        args: args->Array.map(ptGetNode(proofTree, _)),
        frame: {
            ord:0,
            isAxiom:false,
            disj: Belt.Map.Int.empty,
            hyps: hypTypes->Array.map(hypTyp => { typ: hypTyp, label: "", expr: [] }),
            asrt: [],
            label,
            frameVarToSymb: [],
            varTypes: [],
            varHyps: [],
            numOfVars: 0,
            numOfArgs: 0,
            descr: None,
            descrNorm: None,
            proof: None,
            isDisc:false,
            isDepr:false,
            isTranDepr:false,
            dbg: None,
        },
    })
}

describe("jstfEqSrc", _ => {
    it("everything matches, same number of args => true", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6,7], [8,9], ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E,E],[[1,2,3], [4,5,6,7], [8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })

    it("number of E hyps is less => false", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6,7], [8,9], ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,E,E],[[1,2,3], [4,5,6,7], [8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("labels are same and 0 args => true", _ => {
        //given
        let jstfArgs = []
        let jstfLabel = "ABC"
        let src = createSrc([],[],"ABC")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })

    it("labels are not equal => false", _ => {
        //given
        let jstfArgs = []
        let jstfLabel = "ABC"
        let src = createSrc([],[],"ABC-")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("jstf has more args then number of hyps in the src frame => false", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6,7], [8,9], ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E],[[1,2,3], [4,5,6,7]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("jstf has at least 1 arg but src frame has only floating hyps => false", _ => {
        //given
        let jstfArgs = [ [1,2,3] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,F,F],[[1,2,3], [4,5,6,7], [8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is less than number of jstf args, and the last is F hyp => false", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,E,F],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is less than number of jstf args, and the last is E hyp => false", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,F,E],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is also greater than number of jstf args, and the last is F hyp => false", _ => {
        //given
        let jstfArgs = [ [1,2,3] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E,F],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is also greater than number of jstf args, and the last is E hyp => false", _ => {
        //given
        let jstfArgs = [ [1,2,3] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,F,E],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is equal to number of jstf args, and the last is F hyp => true", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E,F],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })

    it("number of hyps in src frame is greater than number of jstf args, but number of E hyps is equal to number of jstf args, and the last is E hyp => true", _ => {
        //given
        let jstfArgs = [ [1,2,3], [7,8,9] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,F,E],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })

    it("one of E hyps doesn't match the corresponding jstf arg => false", _ => {
        //given
        let jstfArgs = [ [1,2,3], [7,8,90] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,F,E],[[1,2,3], [4,5,6], [7,8,9]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , false )
    })

    it("all F hyps are located at the end => true", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6], [7,8,9] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E,E,F,F,F],[[1,2,3], [4,5,6], [7,8,9], [1], [2], [3]],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })

    it("all F hyps are located at the beginning => true", _ => {
        //given
        let jstfArgs = [ [1,2,3], [4,5,6], [7,8,9] ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,F,F,E,E,E],[[1], [2], [3], [1,2,3], [4,5,6], [7,8,9], ],"asrt-label")

        //when/then
        assertEq( jstfEqSrc(jstfArgs, jstfLabel, src) , true )
    })
})