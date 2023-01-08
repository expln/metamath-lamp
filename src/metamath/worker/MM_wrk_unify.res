open MM_context
open Expln_utils_promise
open MM_wrk_ctx
open MM_proof_tree

let procName = "MM_wrk_unify"

type request = 
    | Unify({stmts: array<rootStmt>, bottomUp:bool})

type response =
    | OnProgress(float)
    | Result(proofTreeDto)

let unify = (
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~stmts: array<rootStmt>,
    ~bottomUp: bool,
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
            ~initialRequest = Unify({stmts:stmts, bottomUp}),
            ~onResponse = (~resp, ~sendToWorker, ~endWorkerInteraction) => {
                switch resp {
                    | OnProgress(pct) => onProgress(pct)
                    | Result(proofTree) => {
                        endWorkerInteraction()
                        Js.Console.log2("proofTree", proofTree)
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
        | Unify({stmts, bottomUp}) => {
            let proofTree = proofTreeProve(
                ~parenCnt = getWrkParenCntExn(),
                ~frms = getWrkFrmsExn(),
                ~ctx = getWrkCtxExn(),
                ~stmts,
                ~bottomUp,
                ~syntaxProof=false,
                ~onProgress = pct => sendToClient(OnProgress(pct)),
                ~debug=true,
                ()
            )
            sendToClient(Result({
                newVars: proofTree.newVars->Belt_MutableSet.toArray,
                disj: proofTree.disj,
                nodes: proofTree.rootNodes->Belt_MutableMap.valuesToArray
            }))
        }
    }
}