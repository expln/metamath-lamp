open MM_context
open MM_wrk_ctx_proc
open MM_progress_tracker
open MM_wrk_settings
open Expln_utils_common

type rec syntaxTreeNode = {
    id: int,
    typ:int,
    label:string,
    children:array<childNode>,
    height:int,
}
and childNode =
    | Subtree(syntaxTreeNode)
    | Symbol({id:int, symInt:int, sym:string, color:option<string>, isVar:bool})

let procName = "MM_wrk_syntax_tree"

type request = 
    | BuildSyntaxTreesForAllAsrts

type response =
    | OnProgress(float)
    | Result(array<(string,syntaxTreeNode)>)

let reqToStr = req => {
    switch req {
        | BuildSyntaxTreesForAllAsrts => `BuildSyntaxTreesForAllAsrts`
    }
}

let respToStr = resp => {
    switch resp {
        | OnProgress(pct) => `OnProgress(pct=${pct->Belt_Float.toString})`
        | Result(_) => `Result`
    }
}

// let buildSyntaxTreesForAllAssertions = (
//     ~settings:settingsV,
//     ~preCtxVer: int,
//     ~preCtx: mmContext,
//     ~isAxiom:option<bool>,
//     ~typ:option<int>, 
//     ~label:string, 
//     ~searchPattern:array<stmtPattern>,
//     ~isDisc:option<bool>,
//     ~isDepr:option<bool>,
//     ~isTranDepr:option<bool>,
//     ~returnLabelsOnly:bool,
//     ~onProgress:float=>unit,
// ): promise<(array<stmtsDto>,array<string>)> => {
//     promise(resolve => {
//         beginWorkerInteractionUsingCtx(
//             ~settingsVer,
//             ~settings,
//             ~preCtxVer,
//             ~preCtx,
//             ~varsText="",
//             ~disjText="",
//             ~procName,
//             ~initialRequest = FindAssertions({
//                 isAxiom, typ, label, searchPattern, isDisc, isDepr, isTranDepr, returnLabelsOnly,
//             }),
//             ~onResponse = (~resp, ~sendToWorker as _, ~endWorkerInteraction) => {
//                 switch resp {
//                     | OnProgress(pct) => onProgress(pct)
//                     | SearchResult({found, foundLabels}) => {
//                         endWorkerInteraction()
//                         resolve((found,foundLabels))
//                     }
//                 }
//             },
//             ~enableTrace=false
//         )
//     })
// }

// let doSearchAssertions = (
//     ~allFramesInDeclarationOrder:array<frame>,
//     ~isAxiom:option<bool>,
//     ~typ:option<int>, 
//     ~label:string, 
//     ~searchPattern:array<stmtPattern>,
//     ~isDisc:option<bool>,
//     ~isDepr:option<bool>,
//     ~isTranDepr:option<bool>,
//     ~onProgress:option<float=>unit>=?
// ):array<frame> => {
//     let progressState = progressTrackerMake(~step=0.01, ~onProgress?)
//     let framesProcessed = ref(0.)
//     let numOfFrames = allFramesInDeclarationOrder->Array.length->Belt_Int.toFloat
//     let mapping = Belt_HashMapInt.make(~hintSize=10)

//     let labelTrim = label->String.trim->String.toLowerCase
//     allFramesInDeclarationOrder->Array.filter(frame => {
//         switch onProgress {
//             | None => ()
//             | Some(_) => {
//                 framesProcessed.contents = framesProcessed.contents +. 1.
//                 progressState->progressTrackerSetCurrPct(
//                     framesProcessed.contents /. numOfFrames
//                 )
//             }
//         }
//         isAxiom->Option.mapOr( true, isAxiom => isAxiom == frame.isAxiom ) 
//             && typ->Option.mapOr( true, typ => typ == frame.asrt->Array.getUnsafe(0) )
//             && frame.label->String.toLowerCase->String.includes(labelTrim)
//             && (threeStateBoolMatchesTwoStateBool(isDisc, frame.isDisc))
//             && (threeStateBoolMatchesTwoStateBool(isDepr, frame.isDepr))
//             && (threeStateBoolMatchesTwoStateBool(isTranDepr, frame.isTranDepr))
//             && frameMatchesPattern(~frame, ~searchPattern, ~mapping)
//     })
// }

// let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
//     switch req {
//         | FindAssertions({isAxiom, typ, label, searchPattern, isDisc, isDepr, isTranDepr, returnLabelsOnly}) => {
//             let filteredFrames = doSearchAssertions(
//                 ~allFramesInDeclarationOrder=getAllFramesInDeclarationOrderExn(),
//                 ~isAxiom,
//                 ~typ,
//                 ~label,
//                 ~searchPattern,
//                 ~isDisc,
//                 ~isDepr,
//                 ~isTranDepr,
//                 ~onProgress = pct => sendToClient(OnProgress(pct))
//             )
//             let (results,labels) = if (returnLabelsOnly) {
//                 ([], filteredFrames->Array.map(frame => frame.label))
//             } else {
//                 (filteredFrames->Array.map(frame => frameToStmtsDto(~wrkCtx=getWrkCtxExn(), ~frame)),[])
//             }
//             sendToClient(SearchResult({found:results,foundLabels:labels}))
//         }
//     }
// }