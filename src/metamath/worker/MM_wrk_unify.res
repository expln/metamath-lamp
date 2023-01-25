open MM_context
open Expln_utils_promise
open MM_wrk_ctx
open MM_proof_tree
open MM_proof_tree_dto
open MM_provers

let procName = "MM_wrk_unify"

type request = 
    | Unify({
        stmts: array<rootStmt>, 
        framesToSkip:array<string>, 
        bottomUpProverParams:option<bottomUpProverParams>,
    })

type response =
    | OnProgress(float)
    | Result(proofTreeDto)

let unify = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~framesToSkip:array<string>,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~stmts: array<rootStmt>,
    ~bottomUpProverParams: option<bottomUpProverParams>,
    ~onProgress:float=>unit,
): promise<proofTreeDto> => {
    promise(resolve => {
        beginWorkerInteractionUsingCtx(
            ~preCtxVer,
            ~preCtx,
            ~parenStr,
            ~varsText,
            ~disjText,
            ~hyps,
            ~procName,
            ~initialRequest = Unify({stmts:stmts, framesToSkip, bottomUpProverParams}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | Result(proofTree) => {
                        endWorkerInteraction()
                        resolve(proofTree)
                    }
                }
            },
            ~enableTrace=false,
            ()
        )
    })
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | Unify({stmts, bottomUpProverParams, framesToSkip}) => {
            let proofTree = unifyAll(
                ~parenCnt = getWrkParenCntExn(),
                ~frms = getWrkFrmsExn(),
                ~ctx = getWrkCtxExn(),
                ~stmts,
                ~framesToSkip,
                ~bottomUpProverParams?,
                ~onProgress = pct => sendToClient(OnProgress(pct)),
                ~debug=false,
                ()
            )
            sendToClient(Result(proofTree->proofTreeToDto(stmts->Js_array2.map(stmt=>stmt.expr))))
        }
    }
}