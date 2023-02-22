open Expln_React_common
open Expln_React_Mui
open MM_proof_tree_dto
open MM_react_common
open MM_context
open MM_unification_debug

type state = {
    expanded: bool,
    expandedSrcs: array<int>,
}

let makeInitialState = ():state => {
    {
        expanded: false,
        expandedSrcs: [],
    }
}

let toggleExpanded = st => {
    {
        ...st,
        expanded: !st.expanded
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

module rec ProofNodeDtoCmp: {
    @react.component
    let make: (
        ~tree: proofTreeDto,
        ~nodeIdx: int,
        ~nodeIdxToLabel: int=>string,
        ~exprToReElem: expr=>reElem,
    ) => reElem
} = {
    @react.component
    let make = (
        ~tree: proofTreeDto,
        ~nodeIdx: int,
        ~nodeIdxToLabel: int=>string,
        ~exprToReElem: expr=>reElem,
    ) => {
        let (state, setState) = React.useState(makeInitialState)

        let node = tree.nodes[nodeIdx]

        let actToggleExpanded = () => {
            setState(toggleExpanded)
        }

        let actToggleSrcExpanded = (srcIdx) => {
            setState(expandCollapseSrc(_, srcIdx))
        }

        let rndArgs = (args) => {
            <table>
                <tbody>
                    {
                        args->Js_array2.mapi((arg,argIdx) => {
                            <tr key={argIdx->Belt_Int.toString ++ "-exp"}>
                                <td>
                                    <ProofNodeDtoCmp
                                        tree
                                        nodeIdx=arg
                                        nodeIdxToLabel
                                        exprToReElem
                                    />
                                </td>
                            </tr>
                        })->React.array
                    }
                </tbody>
            </table>
        }

        let rndStatusIconForStmt = (node) => {
            if (node.proof->Belt_Option.isSome) {
                <span
                    title="This is proved"
                    style=ReactDOM.Style.make(~color="green", ~fontWeight="bold", ())
                >
                    {React.string("\u2713")}
                </span>
            } else {
                React.null
            }
        }

        let rndStatusIconForSrc = (args:array<int>, err:option<unifErr>) => {
            switch err {
                | None => {
                    let allArgsAreProved = args->Js_array2.every(arg => tree.nodes[arg].proof->Belt_Option.isSome)
                    if (allArgsAreProved) {
                        <span
                            title="This is a valid proof"
                            style=ReactDOM.Style.make(~color="green", ~fontWeight="bold", ())
                        >
                            {React.string("\u2713")}
                        </span>
                    } else {
                        React.null
                    }
                }
                | Some(_) => {
                    <span
                        title="Click to see error details"
                        style=ReactDOM.Style.make(~color="red", ~fontWeight="bold", ~cursor="pointer", ())
                    >
                        {React.string("\u2717")}
                    </span>
                }
            }
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
                | Assertion({args, label, err}) => {
                    <tr key>
                        <td style=ReactDOM.Style.make(~verticalAlign="top", ())> {rndStatusIconForSrc(args, err)} </td>
                        <td
                            onClick={_=>actToggleSrcExpanded(srcIdx)}
                            style=ReactDOM.Style.make(~cursor="pointer", ~verticalAlign="top", ())
                        >
                            {React.string(label ++ ":")}
                        </td>
                        <td>
                            {
                                if (state->isExpandedSrc(srcIdx)) {
                                    rndArgs(args)
                                } else {
                                    React.string(args->Js_array2.map(nodeIdxToLabel)->Js_array2.joinWith(" "))
                                }
                            } 
                        </td>
                    </tr>
                }
            }
        }

        let rndSrcs = () => {
            switch node.parents {
                | None => {
                    React.string("Sources are not set.")
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
                        <td> {rndStatusIconForStmt(tree.nodes[nodeIdx])} </td>
                        <td
                            style=ReactDOM.Style.make(~cursor="pointer", ())
                            onClick={_=>actToggleExpanded()}
                        > 
                            {React.string(nodeIdxToLabel(nodeIdx) ++ ":")} 
                        </td>
                        <td> {exprToReElem(tree.nodes[nodeIdx].expr)} </td>
                    </tr>
                    {
                        if (state.expanded) {
                            <tr>
                                <td> React.null </td>
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
}

@react.component
let make = (
    ~tree: proofTreeDto,
    ~nodeIdx: int,
    ~nodeIdxToLabel: int=>string,
    ~exprToReElem: expr=>reElem,
) => {
    <ProofNodeDtoCmp
        tree
        nodeIdx
        nodeIdxToLabel
        exprToReElem
    />
}