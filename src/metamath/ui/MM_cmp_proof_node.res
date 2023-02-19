open Expln_React_common
open Expln_React_Mui
open MM_proof_tree_dto
open MM_react_common
open MM_context

type state = {
    expandedSrcs: array<int>,
    expandedArgs: Belt_MapInt.t<array<int>>,
}

let makeInitialState = ():state => {
    {
        expandedSrcs: [],
        expandedArgs: Belt_MapInt.empty,
    }
}

let isExpandedSrc = (st,srcIdx) => st.expandedSrcs->Js_array2.includes(srcIdx)

let isExpandedArg = (st,srcIdx,argIdx) => {
    switch st.expandedArgs->Belt_MapInt.get(srcIdx) {
        | None => false
        | Some(argIdxs) => argIdxs->Js_array2.includes(argIdx)
    }
}

let expandCollapseSrc = (st,srcIdx) => {
    if (st.expandedSrcs->Js_array2.includes(srcIdx)) {
        {
            ...st,
            expandedSrcs: st.expandedSrcs->Js.Array2.filter(i => i != srcIdx)
        }
    } else {
        {
            ...st,
            expandedSrcs: st.expandedSrcs->Js.Array2.concat([srcIdx])
        }
    }
}

let expandCollapseArg = (st,srcIdx,argIdx) => {
    switch st.expandedArgs->Belt_MapInt.get(srcIdx) {
        | None => {
            {
                ...st,
                expandedArgs: Belt_MapInt.fromArray([(srcIdx,[argIdx])])
            }
        }
        | Some(argIdxs) => {
            if (argIdxs->Js_array2.includes(argIdx)) {
                {
                    ...st,
                    expandedArgs: st.expandedArgs->Belt_MapInt.set(srcIdx, argIdxs->Js.Array2.filter(i => i != argIdx)) 
                }
            } else {
                {
                    ...st,
                    expandedArgs: st.expandedArgs->Belt_MapInt.set(srcIdx, argIdxs->Js.Array2.concat([argIdx]))
                }
            }
        }
    }
}

@react.component
let rec make = (
    ~tree: proofTreeDto,
    ~nodeIdx: int,
    ~nodeIdxToLabel: int=>string,
    ~exprToReElem: expr=>reElem,
) => {
    let (state, setState) = React.useState(makeInitialState)

    let node = tree.nodes[nodeIdx]

    let rndArgs = (srcIdx,args) => {
        <table>
            <tbody>
                {
                    args->Js_array2.mapi((arg,argIdx) => {
                        <>
                            <tr key={argIdx->Belt_Int.toString}>
                                <td> {React.string(nodeIdxToLabel(arg))} </td>
                                <td> {exprToReElem(tree.nodes[arg].expr)} </td>
                            </tr>
                            {
                                if (isExpandedArg(state,srcIdx,argIdx)) {
                                    <tr key={argIdx->Belt_Int.toString ++ "-exp"}>
                                        <td> React.null </td>
                                        <td>
                                            {React.createElement(make, {
                                                "tree": tree,
                                                "nodeIdx": arg,
                                                "nodeIdxToLabel": nodeIdxToLabel,
                                                "exprToReElem": exprToReElem,
                                            })}
                                        </td>
                                    </tr>
                                } else {
                                    React.null
                                }
                            }
                        </>
                    })->React.array
                }
            </tbody>
        </table>
    }

    let rndSrc = (src,srcIdx) => {
        <table key={srcIdx->Belt_Int.toString}>
            <tbody>
                {
                    switch src {
                        | VarType => {
                            <tr>
                                <td> {React.string("VarType")} </td>
                            </tr>
                        }
                        | Hypothesis({label}) => {
                            <tr>
                                <td> {React.string("Hypothesis: " ++ label)} </td>
                            </tr>
                        }
                        | Assertion({args, label}) => {
                            <>
                                <tr>
                                    <td> {React.string(label ++ " :")} </td>
                                    <td> {React.string(args->Js_array2.map(nodeIdxToLabel)->Js_array2.joinWith(" "))} </td>
                                </tr>
                                {
                                    if (isExpandedSrc(state,srcIdx)) {
                                        <tr key={srcIdx->Belt_Int.toString ++ "-exp"}>
                                            <td> React.null </td>
                                            <td> {rndArgs(srcIdx, args)} </td>
                                        </tr>
                                    } else {
                                        React.null
                                    }
                                }
                            </>
                        }
                    }
                }
            </tbody>
        </table>
    }

    let rndSrcs = () => {
        <table>
            <tbody>
                {
                    switch node.parents {
                        | None => {
                            <tr>
                                <td>
                                    {React.string("Parents are not set.")}
                                </td>
                            </tr>
                        }
                        | Some(parents) => {
                            React.array(parents->Js_array2.mapi((src,srcIdx) => {
                                <tr key={srcIdx->Belt_Int.toString}>
                                {
                                    switch src {
                                        | VarType => {
                                            <td> {React.string("VarType")} </td>
                                        }
                                        | Hypothesis({label}) => {
                                            <td> {React.string("Hypothesis: " ++ label)} </td>
                                        }
                                        | Assertion({args, label}) => {
                                            <>
                                                <td> {React.string(label ++ " :")} </td>
                                                <td> {React.string(args->Js_array2.map(nodeIdxToLabel)->Js_array2.joinWith(" "))} </td>
                                            </>
                                        }
                                    }
                                }
                                </tr>
                            }))
                        }
                    }
                }
            </tbody>
        </table>
    }

    React.string("")
}