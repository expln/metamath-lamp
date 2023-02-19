open Expln_React_common
open Expln_React_Mui
open MM_proof_tree_dto
open MM_react_common
open MM_context

type state = {
    isExpanded: bool,
    expandedSrcs: array<int>,
}

let makeInitialState = ():state => {
    {
        isExpanded: false,
        expandedSrcs: [],
    }
}

let isExpandedSrc = (st,srcIdx) => st.expandedSrcs->Js_array2.includes(srcIdx)

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

@react.component
let rec make = (
    ~tree: proofTreeDto,
    ~nodeIdx: int,
    ~nodeIdxToLabel: int=>string,
    ~exprToReElem: expr=>reElem,
) => {
    let (state, setState) = React.useState(makeInitialState)

    let node = tree.nodes[nodeIdx]

    let rndArgs = (args) => {
        <table>
            <tbody>
                {
                    args->Js_array2.mapi((arg,argIdx) => {
                        <tr key={argIdx->Belt_Int.toString ++ "-exp"}>
                            <td>
                                {React.createElement(make, {
                                    "tree": tree,
                                    "nodeIdx": arg,
                                    "nodeIdxToLabel": nodeIdxToLabel,
                                    "exprToReElem": exprToReElem,
                                })}
                            </td>
                        </tr>
                    })->React.array
                }
            </tbody>
        </table>
    }

    let rndSrc = (src,srcIdx) => {
        let key = srcIdx->Belt_Int.toString
        switch src {
            | VarType => {
                <tr key>
                    <td> {React.string("VarType")} </td>
                    <td> {React.null} </td>
                </tr>
            }
            | Hypothesis({label}) => {
                <tr key>
                    <td> {React.string("Hyp " ++ label)} </td>
                    <td> {React.null} </td>
                </tr>
            }
            | Assertion({args, label}) => {
                <tr key>
                    <td> {React.string(label ++ " :")} </td>
                    <td> 
                        {
                            if (state->isExpandedSrc(srcIdx)) {
                                rndArgs(args)
                            } else {
                                React.string(args->Js_array2.map(nodeIdxToLabel)->Js_array2.joinWith(" "))
                            }
                        } 
                    </td>
                    <td> {React.string(args->Js_array2.map(nodeIdxToLabel)->Js_array2.joinWith(" "))} </td>
                </tr>
            }
        }
    }

    let rndSrcs = () => {
        switch node.parents {
            | None => {
                React.string("Parents are not set.")
            }
            | Some(parents) => {
                <table>
                    <tbody>
                        {
                            parents->Js_array2.mapi((src,srcIdx) => rndSrc(src,srcIdx))->React.array
                        }
                    </tbody>
                </table>
            }
        }
    }

    let rndNode = () => {
        <table>
            <tbody>
                <tr>
                    <td> {React.string(nodeIdxToLabel(nodeIdx))} </td>
                    <td> {exprToReElem(tree.nodes[nodeIdx].expr)} </td>
                </tr>
                {
                    if (state.isExpanded) {
                        <tr>
                            <td> React.null </td>
                            <td>
                                {rndSrcs()}
                            </td>
                        </tr>
                    } else {
                        React.null
                    }
                }
            </tbody>
        </table>
    }

    rndNode()
}