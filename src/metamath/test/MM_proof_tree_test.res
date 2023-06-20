open Expln_test
open MM_proof_tree
open MM_context
open MM_parenCounter

let createSrc = (hypTypes:array<hypothesisType>, args:array<expr>, label:string):exprSrc => {
    let proofTree = ptMake(
        ~frms = Belt_MapString.empty,
        ~hyps = Belt_MapString.empty,
        ~ctxMaxVar = 0,
        ~disj = disjMake(),
        ~parenCnt = parenCntMake([], ()),
        ~exprToStr = None,
    )
    Assertion({
        args: args->Js_array2.map(proofTree->ptGetNode),
        frame: {
            isAxiom:false,
            disj: Belt.Map.Int.empty,
            hyps: hypTypes->Js_array2.map(hypTyp => { typ: hypTyp, label: "", expr: [] }),
            asrt: [],
            label,
            frameVarToSymb: [],
            varTypes: [],
            numOfVars: 0,
            numOfArgs: 0,
            descr: None,
            proof: None,
            dbg: None,
        },
    })
}

describe("jstfEqSrc", _ => {
    it("everything matches, same number of args => true", _ => {
        //given
        let jstfArgs = [
            [1,2,3],
            [4,5,6,7],
            [8,9],
        ]
        let jstfLabel = "asrt-label"
        let src = createSrc([E,E,E],[[1,2,3], [4,5,6,7], [8,9]],"asrt-label")

        //when
        let actual = jstfEqSrc(jstfArgs, jstfLabel, src)

        //then
        assertEq( actual , true )
    })

    it("number of E hyps is less => false", _ => {
        //given
        let jstfArgs = [
            [1,2,3],
            [4,5,6,7],
            [8,9],
        ]
        let jstfLabel = "asrt-label"
        let src = createSrc([F,E,E],[[1,2,3], [4,5,6,7], [8,9]],"asrt-label")

        //when
        let actual = jstfEqSrc(jstfArgs, jstfLabel, src)

        //then
        assertEq( actual , false )
    })
})