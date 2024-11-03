open Expln_React_common
open MM_proof_tree_dto
open MM_context
open MM_unification_debug

type state = {
    expanded: bool,
    expandedSrcs: array<int>,
    allSrcsExpanded:bool,
}

let makeInitialState = ():state => {
    {
        expanded: false,
        expandedSrcs: [],
        allSrcsExpanded:false,
    }
}

let toggleExpanded = st => {
    {
        ...st,
        expanded: !st.expanded
    }
}

let toggleAllSrcsExpanded = st => {
    {
        ...st,
        allSrcsExpanded: !st.allSrcsExpanded
    }
}

let isExpandedSrc = (st,srcIdx) => st.expandedSrcs->Array.includes(srcIdx)

let expandCollapseSrc = (st,srcIdx) => {
    if (st.expandedSrcs->Array.includes(srcIdx)) {
        {
            ...st,
            expandedSrcs: st.expandedSrcs->Array.filter(i => i != srcIdx)
        }
    } else {
        {
            ...st,
            expandedSrcs: st.expandedSrcs->Array.concat([srcIdx])
        }
    }
}

let validProofIcon = 
    <span
        title="This is a valid proof"
        style=ReactDOM.Style.make(~color="green", ~fontWeight="bold", ())
    >
        {React.string("\u2713")}
    </span>

type propsInner = {
    tree: proofTreeDto,
    nodeIdx: int,
    isRootStmt: int=>bool,
    nodeIdxToLabel: int=>string,
    exprToStr: expr=>string,
    exprToReElem: expr=>reElem,
    frmExprToStr: (string,expr)=>string,
    getFrmLabelBkgColor: string=>option<string>,
    showUnprovedOnly:bool,
}

let propsInnerAreSame = (a:propsInner,b:propsInner) => {
    a.showUnprovedOnly == b.showUnprovedOnly
}

let rndExpandCollapseIcon = (~expand:bool,~visible:bool) => {
    let char = if (expand) {"\u229E"} else {"\u229F"}
    <span 
        style=ReactDOM.Style.make(
            ~fontSize="13px", 
            ~opacity={visible ? {"1.0"} : {"0.0"}}, 
            ()
        )>
        {React.string(char)}
    </span>
}

module rec ProofNodeDtoCmp: {
    let make: propsInner => reElem
} = {
    type props = propsInner
    let make = React.memoCustomCompareProps( ({
        tree,
        nodeIdx,
        isRootStmt,
        nodeIdxToLabel,
        exprToStr,
        exprToReElem,
        frmExprToStr,
        getFrmLabelBkgColor,
        showUnprovedOnly,
    }:props) => {
        let (state, setState) = React.useState(makeInitialState)

        let node = tree.nodes->Array.getUnsafe(nodeIdx)

        let getParents = () => {
            if (node.parents->Array.length == 0) {
                switch node.proof {
                    | None => []
                    | Some(src) => [src]
                }
            } else {
                node.parents
            }
        }

        let parents = getParents()

        let actToggleExpanded = () => {
            setState(toggleExpanded)
        }

        let actToggleAllSrcsExpanded = () => {
            setState(st => {
                let st = st->toggleAllSrcsExpanded
                if (parents->Array.length > 0) {
                    if (st.allSrcsExpanded) {
                        {
                            ...st,
                            expanded:true,
                            expandedSrcs: Belt.Array.range(0,parents->Array.length-1) 
                        }
                    } else {
                        {
                            ...st,
                            expandedSrcs: []
                        }
                    }
                } else {
                    st
                }
            })
        }

        let actToggleSrcExpanded = (srcIdx) => {
            setState(expandCollapseSrc(_, srcIdx))
        }

        let getColorForLabel = nodeIdx => {
            if(isRootStmt(nodeIdx)) {
                "black"
            } else {
                "lightgrey"
            }
        }

        let rndCollapsedArgs = (args, srcIdx) => {
            <span
                onClick={_=>actToggleSrcExpanded(srcIdx)}
                style=ReactDOM.Style.make(~cursor="pointer", ())
            >
                {React.string("( ")}
                {
                    args->Array.mapWithIndex((arg,i) => {
                        <span
                            key={i->Belt_Int.toString} 
                            style=ReactDOM.Style.make(~color=getColorForLabel(arg), ())
                        >
                            {React.string(nodeIdxToLabel(arg) ++ " ")}
                        </span>
                    })->React.array
                }
                {React.string(" )")}
            </span>
        }

        let rndErr = (err,asrtLabel) => {
            <pre style=ReactDOM.Style.make(~color="red", ~margin="0px", ())>
            { unifErrToStr(err, ~exprToStr, ~frmExprToStr = frmExprToStr(asrtLabel, _))->React.string }
            </pre>
        }

        let isAsrtWithoutHyps = (src) => {
            switch src {
                | VarType | Hypothesis(_) => false
                | Assertion({args}) => args->Array.length == 0
                | AssertionWithErr({args, err}) => {
                    switch err {
                        | UnifErr | DisjCommonVar(_) | Disj(_) | UnprovedFloating(_) => {
                            args->Array.length == 0
                        }
                        | NoUnifForAsrt(_) | NoUnifForArg(_) | NewVarsAreDisabled(_) | TooManyCombinations(_) => false
                    }
                }
            }
        }

        let isNodeProved = (argIdx:int):bool => {
            (tree.nodes->Array.getUnsafe(argIdx)).proof->Option.isSome
        }

        let rndExpandedArgs = (args, srcIdx) => {
            let src = parents->Array.getUnsafe(srcIdx)
            <table>
                <tbody>
                    <tr key="c-args">
                        <td>
                            {rndCollapsedArgs(args, srcIdx)}
                        </td>
                    </tr>
                    {
                        switch src {
                            | AssertionWithErr({label,err}) => {
                                <tr>
                                    <td> {rndErr(err,label)} </td>
                                </tr>
                            }
                            | _ => React.null
                        }
                    }
                    {
                        if (isAsrtWithoutHyps(src)) {
                            <tr key={"-exp"}>
                                <td>
                                    {React.string("This assertion doesn't have hypotheses.")}
                                </td>
                            </tr>
                        } else {
                            args
                                ->Array.filter(arg => !showUnprovedOnly || !isNodeProved(arg))
                                ->Array.mapWithIndex((arg,argIdx) => {
                                    <tr key={argIdx->Belt_Int.toString ++ "-exp"}>
                                        <td>
                                            <ProofNodeDtoCmp
                                                tree
                                                nodeIdx=arg
                                                isRootStmt
                                                nodeIdxToLabel
                                                exprToStr
                                                exprToReElem
                                                frmExprToStr
                                                getFrmLabelBkgColor
                                                showUnprovedOnly
                                            />
                                        </td>
                                    </tr>
                                })->React.array
                        }
                    }
                </tbody>
            </table>
        }

        let rndStatusIconForStmt = (node:proofNodeDto) => {
            <span
                title="This is proved"
                style=ReactDOM.Style.make(
                    ~color="green", 
                    ~fontWeight="bold", 
                    ~visibility=if (node.proof->Belt_Option.isSome) {"visible"} else {"hidden"},
                    ()
                )
            >
                {React.string("\u2713")}
            </span>
        }

        let rndStatusIconForSrc = (src:exprSrcDto) => {
            switch src {
                | VarType | Hypothesis(_) => validProofIcon
                | Assertion({args}) => {
                    let allArgsAreProved = args->Array.every(arg => (tree.nodes->Array.getUnsafe(arg)).proof->Belt_Option.isSome)
                    if (allArgsAreProved) {
                        validProofIcon
                    } else {
                        React.null
                    }
                }
                | AssertionWithErr(_) => {
                    <span
                        style=ReactDOM.Style.make(~color="red", ~fontWeight="bold", ~cursor="pointer", ())
                    >
                        {React.string("\u2717")}
                    </span>
                }
            }
        }

        let rndSrc = (src:exprSrcDto,srcIdx:int) => {
            let key = srcIdx->Belt_Int.toString
            switch src {
                | VarType => {
                    <tr key>
                        <td style=ReactDOM.Style.make(~verticalAlign="top", ())> {rndStatusIconForSrc(src)} </td>
                        <td> {React.string("VarType")} </td>
                        <td> {React.null} </td>
                    </tr>
                }
                | Hypothesis({label}) => {
                    <tr key>
                        <td style=ReactDOM.Style.make(~verticalAlign="top", ())> {rndStatusIconForSrc(src)} </td>
                        <td> {React.string("Hyp " ++ label)} </td>
                        <td> {React.null} </td>
                    </tr>
                }
                | Assertion({args, label}) | AssertionWithErr({args, label}) => {
                    <tr key>
                        <td style=ReactDOM.Style.make(~verticalAlign="top", ())> {rndStatusIconForSrc(src)} </td>
                        <td
                            onClick={_=>actToggleSrcExpanded(srcIdx)}
                            style=ReactDOM.Style.make(~cursor="pointer", ~verticalAlign="top", ())
                        >
                            {
                                rndExpandCollapseIcon(
                                    ~expand=!(state->isExpandedSrc(srcIdx)),
                                    ~visible=parents->Array.length != 0
                                )
                            }
                            <span
                                style=ReactDOM.Style.make(
                                    ~backgroundColor=?getFrmLabelBkgColor(label), 
                                    ~borderRadius="3px",
                                    ()
                                )
                            >
                                <i>{React.string(label)}</i>
                            </span>
                        </td>
                        <td>
                            {
                                if (state->isExpandedSrc(srcIdx)) {
                                    rndExpandedArgs(args, srcIdx)
                                } else {
                                    rndCollapsedArgs(args, srcIdx)
                                }
                            } 
                        </td>
                    </tr>
                }
            }
        }

        let rndSrcs = () => {
            if (parents->Array.length == 0) {
                React.string("Sources are not set.")
            } else {
                <table>
                    <tbody>
                        {
                            parents->Array.mapWithIndex((src,srcIdx) => rndSrc(src,srcIdx))->React.array
                        }
                    </tbody>
                </table>
            }
        }

        let rndNode = () => {
            <table>
                <tbody>
                    <tr>
                        <td> {rndStatusIconForStmt(node)} </td>
                        <td
                            style=ReactDOM.Style.make(
                                ~cursor="pointer", 
                                ~color=getColorForLabel(nodeIdx), ()
                            )
                            onClick={_=>actToggleExpanded()}
                        > 
                            {
                                rndExpandCollapseIcon(
                                    ~expand=!state.expanded,
                                    ~visible=parents->Array.length != 0
                                )
                            }
                            
                            {React.string(nodeIdxToLabel(nodeIdx) ++ ":")}
                        </td>
                        <td> 
                            <span 
                                style=ReactDOM.Style.make( ~cursor="pointer", ~minWidth="500px", ()) 
                                onClick={_=>actToggleExpanded()}
                            >
                                {exprToReElem((tree.nodes->Array.getUnsafe(nodeIdx)).expr)}
                            </span>
                            <span 
                                style=ReactDOM.Style.make( ~cursor="pointer", ~minWidth="500px", ()) 
                                onClick={_=>actToggleAllSrcsExpanded()}
                            >
                                {
                                    rndExpandCollapseIcon(
                                        ~expand=!state.allSrcsExpanded,
                                        ~visible=parents->Array.length != 0
                                    )
                                }
                            </span>
                        </td>
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
    }, propsInnerAreSame)
}

@react.component
let make = (
    ~tree: proofTreeDto,
    ~nodeIdx: int,
    ~isRootStmt: int=>bool,
    ~nodeIdxToLabel: int=>string,
    ~exprToStr: expr=>string,
    ~exprToReElem: expr=>reElem,
    ~frmExprToStr: (string,expr)=>string,
    ~getFrmLabelBkgColor: string=>option<string>,
    ~showUnprovedOnly:bool,
) => {
    <ProofNodeDtoCmp
        tree
        nodeIdx
        isRootStmt
        nodeIdxToLabel
        exprToStr
        exprToReElem
        frmExprToStr
        getFrmLabelBkgColor
        showUnprovedOnly
    />
}