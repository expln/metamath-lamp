open Expln_test
open MM_int_test_utils
open MM_int_test_editor_methods
open MM_wrk_frag_transform
open MM_context
open Common

let rec extractResult = (reactElemDto:{..}):option<string> => {
    switch reactElemDto["cmp"]->Js.Nullable.toOption {
        | Some("ApplyButtons") => reactElemDto["result"]->Js.Nullable.toOption
        | _ => {
            switch reactElemDto["children"]->Js.Nullable.toOption {
                | Some(children) => {
                    children->Js_array2.reduce(
                        (res,child) => {
                            switch res {
                                | Some(_) => res
                                | None => extractResult(child)
                            }
                        },
                        None
                    )
                }
                | None => None
            }
        }
    }
}

let testTransform = (
    ~editorState:MM_wrk_editor.editorState,
    ~selectedFragment:string,
    ~transformName:string,
    ~transformState:fragmentTransformState,
    ~expectedResult:string,
):unit => {
    let wrkCtx = editorState.wrkCtx->Belt.Option.getExn
    let syntaxTree = MM_wrk_editor.textToSyntaxTree(
        ~wrkCtx,
        ~syms=[selectedFragment->getSpaceSeparatedValuesAsArray],
        ~syntaxTypes=[wrkCtx->ctxSymToIntExn("wff")],
        ~frms=editorState.frms,
        ~frameRestrict=editorState.settings.allowedFrms.inSyntax,
        ~parenCnt=editorState.parenCnt,
        ~lastSyntaxType=None,
        ~onLastSyntaxTypeChange=_=>(),
    )
    let syntaxTreeNode = switch syntaxTree {
        | Ok([Ok(syntaxTreeNode)]) => syntaxTreeNode
        | Error(msg) => Js.Exn.raiseError(`[error-1] ${msg}`)
        | Ok([Error(msg)]) => Js.Exn.raiseError(`[error-2] ${msg}`)
        | _ => Js.Exn.raiseError(`[error-3]`)
    }
    let selection = syntaxTreeToSelection(Subtree(syntaxTreeNode))
    let param = {"selection":selection}
    let allTransforms = arrStrToFragTransforms([MM_frag_transform_default_script.fragmentTransformsDefaultScript])->Belt_Result.getExn
    let filteredTransforms = allTransforms->Js.Array2.filter(tr => tr.displayName(param) == transformName)
    assertEqMsg(filteredTransforms->Js.Array2.length, 1, "filteredTransforms->Js.Array2.length")
    let transform = filteredTransforms[0]
    assertEqMsg(transform.canApply(param), true, "transform.canApply(param)")

    let elemDto = transform.renderDialog( { "selection":selection, "state":transformState, "setState":_=>() } )
    assertEq(
        elemDto->reactElemDtoToObj->extractResult->Belt.Option.map(str => {
            str->getSpaceSeparatedValuesAsArray->Js.Array2.joinWith(" ")
        }), 
        Some(expectedResult)
    )
}

describe("MM_wrk_editor integration tests: MM_wrk_frag_transform", _ => {
    it("Insert: X => ( X + A )", _ => {
        //given
        setTestDataDir("MM_wrk_frag_transform")
        let st = createEditorState( ~mmFilePath=setMmPath, ~stopBefore="mathbox", ~debug, () )

        testTransform(
            ~editorState = st,
            ~selectedFragment = "x = y",
            ~transformName = "Insert: X => ( X + A )",
            ~transformState = objToFragmentTransformState({"text": "=> ph", "right": true, "paren": "( )"}),
            ~expectedResult = "( x = y => ph )",
        )
    })
})