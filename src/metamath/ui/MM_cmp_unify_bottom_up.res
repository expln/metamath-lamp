open Expln_React_common
open Expln_React_Mui
open MM_react_common
open Expln_utils_promise
open MM_asrt_apply
open MM_wrk_ctx
open MM_wrk_editor
open MM_wrk_search_asrt
open MM_context
open MM_substitution
open MM_parser
open Expln_React_Modal
open MM_statements_dto
open MM_provers
open MM_proof_tree
open MM_proof_tree_dto
open MM_wrk_unify

type resultRendered = {
    elem: reElem,
    asrtLabel:string,
    numOfNewVars: int,
    numOfNewUnprovedStmts: int,
}

type state = {
    title: reElem,

    availableLabels: array<string>,
    label:option<string>,
    depth: int,
    depthStr: string,
    lengthRestrict: lengthRestrict,

    results: option<array<newStmtsDto>>,
    resultsRendered: option<array<resultRendered>>,
    resultsSorted: option<array<int>>,
    resultsPerPage:int,
    resultsMaxPage:int,
    resultsPage:int,
    checkedResultIdx: option<int>,
}

let makeInitialState = (~wrkCtx, ~stmts: array<rootStmt>,) => {
    {
        title:
            <span>
                <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                    {"Proving bottom-up: "->React.string}
                </span>
                {
                    React.string(
                        stmts[stmts->Js_array2.length-1].expr->ctxIntsToStrExn(wrkCtx, _)
                    )
                }
            </span>,
        
        availableLabels: wrkCtx->getAllFrames->Belt_MapString.keysToArray,
        label: None,
        depthStr: "4",
        depth: 4,
        lengthRestrict: Less,

        results: None,
        resultsRendered: None,
        resultsSorted: None,
        resultsPerPage: 20,
        resultsMaxPage: 0,
        resultsPage: 0,
        checkedResultIdx: None,
    }
}

let setLabel = (st,label) => {
    {
        ...st,
        label
    }
}

let setDepthStr = (st,depthStr) => {
    {
        ...st,
        depthStr
    }
}

let setLengthRestrict = (st,lengthRestrict) => {
    {
        ...st,
        lengthRestrict
    }
}

let setResults = (st,results) => {
    {
        ...st,
        results
    }
}

let notDigitPattern = %re("/\D/g")

let lengthRestrictToStr = (len:lengthRestrict) => {
    switch len {
        | No => "No"
        | LessEq => "LessEq"
        | Less => "Less"
    }
}
let lengthRestrictFromStr = str => {
    switch str {
        | "No" => No
        | "LessEq" => LessEq
        | "Less" => Less
        | _ => raise(MmException({msg:`Cannot convert '${str}' to lengthRestrict.`}))
    }
}

let srcToNewStmts = (
    ~rootStmts:array<rootStmt>,
    ~src:exprSourceDto, 
    ~tree:proofTreeDto, 
    ~newVarTypes:Belt_HashMapInt.t<int>,
    ~wrkCtx: mmContext,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~getFrame: string=>frame,
):option<newStmtsDto> => {
    switch src {
        | Assertion({args, label}) => {
            let maxCtxVar = wrkCtx->getNumOfVars - 1
            let res = {
                newVars: [],
                newVarTypes: [],
                newDisj: disjMutableMake(),
                newDisjStr: [],
                stmts: [],
            }
            let exprToLabel = rootStmts
                ->Js.Array2.map(stmt=>(stmt.expr,stmt.label))
                ->Belt_HashMap.fromArray(~id=module(ExprHash))
            let varNames = Belt_HashMapInt.make(~hintSize=8)
            let usedVarNames = Belt_HashSetString.make(~hintSize=8)
            let usedLabels = rootStmts->Js.Array2.map(stmt=>stmt.label)->Belt_HashSetString.fromArray

            let intToSym = i => {
                if (i <= maxCtxVar) {
                    switch wrkCtx->ctxIntToSym(i) {
                        | None => raise(MmException({msg:`Cannot determine sym for an existing int in nodeToNewStmts.`}))
                        | Some(sym) => sym
                    }
                } else {
                    switch varNames->Belt_HashMapInt.get(i) {
                        | None => raise(MmException({msg:`Cannot determine name of a new var in nodeToNewStmts.`}))
                        | Some(name) => name
                    }
                }
            }

            let addExprToResult = (~label, ~expr, ~proof) => {
                expr->Js_array2.forEach(ei => {
                    if (ei > maxCtxVar && !(res.newVars->Js_array2.includes(ei))) {
                        switch newVarTypes->Belt_HashMapInt.get(ei) {
                            | None => raise(MmException({msg:`Cannot determine type of a new var in nodeToNewStmts.`}))
                            | Some(typ) => {
                                res.newVars->Js_array2.push(ei)->ignore
                                res.newVarTypes->Js_array2.push(typ)->ignore
                                let newVarName = generateNewVarNames( ~ctx = wrkCtx, ~types = [typ],
                                    ~typeToPrefix, ~usedNames=usedVarNames, ()
                                )[0]
                                usedVarNames->Belt_HashSetString.add(newVarName)
                                varNames->Belt_HashMapInt.set(ei, newVarName)
                            }
                        }
                    }
                })
                let exprStr = expr->Js_array2.map(intToSym)->Js.Array2.joinWith(" ")
                let jstf = switch proof {
                    | Some(Assertion({args, label})) => {
                        let argLabels = []
                        getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                            if (hyp.typ == E) {
                                switch exprToLabel->Belt_HashMap.get(tree.nodes[args[i]].expr) {
                                    | None => raise(MmException({msg:`Cannot get a label for an arg by arg expr.`}))
                                    | Some(argLabel) => argLabels->Js_array2.push(argLabel)->ignore
                                }
                            }
                        })
                        Some({ args:argLabels, label})
                    }
                    | _ => None
                }
                res.stmts->Js_array2.push(
                    {
                        label,
                        expr,
                        exprStr,
                        jstf,
                        isProved: proof->Belt_Option.isSome,
                    }
                )->ignore
            }

            let frame = getFrame(label)
            let eArgs = []
            frame.hyps->Js.Array2.forEachi((hyp,i) => {
                if (hyp.typ == E) {
                    eArgs->Js_array2.push(tree.nodes[args[i]])->ignore
                }
            })
            eArgs->Js.Array2.forEach(node => {
                Expln_utils_data.traverseTree(
                    (),
                    node,
                    (_,node) => {
                        if (exprToLabel->Belt_HashMap.has(node.expr)) {
                            None
                        } else {
                            switch node.proof {
                                | Some(Assertion({args,label})) => {
                                    let children = []
                                    getFrame(label).hyps->Js_array2.forEachi((hyp,i) => {
                                        if (hyp.typ == E) {
                                            let argNode = tree.nodes[args[i]]
                                            if (!(exprToLabel->Belt_HashMap.has(argNode.expr))) {
                                                children->Js_array2.push(argNode)->ignore
                                            }
                                        }
                                    })
                                    Some(children)
                                }
                                | _ => None
                            }
                        }
                    },
                    ~postProcess = (_,node) => {
                        if (!(exprToLabel->Belt_HashMap.has(node.expr))) {
                            let newLabel = generateNewLabels(~ctx = wrkCtx, ~prefix="stmt", ~amount=1, ~usedLabels, ())[0]
                            exprToLabel->Belt_HashMap.set(node.expr, newLabel)
                            usedLabels->Belt_HashSetString.add(newLabel)
                            addExprToResult(~label=newLabel, ~expr = node.expr, ~proof = node.proof)
                        }
                        None
                    },
                    ()
                )->ignore
            })
            let stmtToProve = rootStmts[rootStmts->Js.Array2.length-1]
            addExprToResult(~label=stmtToProve.label, ~expr = stmtToProve.expr, ~proof = Some(src))
            if (!disjIsEmpty(tree.disj)) {
                let numOfNewVars = res.newVars->Js.Array2.length
                for ni in 0 to numOfNewVars-2 {
                    for mi in ni+1 to numOfNewVars-1 {
                        let n = res.newVars[ni]
                        let m = res.newVars[mi]
                        if (!(wrkCtx->isDisj(n,m)) && tree.disj->disjContains(n,m)) {
                            res.newDisj->disjAddPair(n,m)
                            res.newDisjStr->Js.Array2.push(`$d ${intToSym(n)} ${intToSym(m)} $.`)->ignore
                        }
                    }
                }
            }
            Some(res)
        }
        | _ => None
    }
    
}

let newStmtsDtoToResultRendered = (newStmtsDto:newStmtsDto):resultRendered => {
    let elem = 
        <Col>
            {
                newStmtsDto.newDisjStr
                    ->Js.Array2.map(disjStr => {
                        <span key=disjStr>
                            {disjStr->React.string}
                        </span>
                    })
                    ->React.array
            }
            <table>
                <tbody>
                    {
                        newStmtsDto.stmts
                            ->Js.Array2.map(stmt => {
                                <tr key=stmt.exprStr>
                                    <td>
                                        {
                                            switch stmt.jstf {
                                                | None => React.null
                                                | Some({args, label}) => {
                                                    React.string(
                                                        args->Js_array2.joinWith(" ") ++ " : " ++ label
                                                    )
                                                }
                                            }
                                        }
                                    </td>
                                    <td> 
                                        { if (stmt.isProved) { React.string("\u2713") } else { React.null } } 
                                    </td>
                                    <td> 
                                        {React.string(stmt.exprStr)}
                                    </td>
                                </tr>
                            })
                            ->React.array
                    }
                </tbody>
            </table>
        </Col>
    {
        elem,
        asrtLabel:
            newStmtsDto.stmts[newStmtsDto.stmts->Js.Array2.length-1].jstf
                ->Belt_Option.map(jstf => jstf.label)
                ->Belt_Option.getWithDefault(""),
        numOfNewVars: newStmtsDto.newVars->Js.Array2.length,
        numOfNewUnprovedStmts: newStmtsDto.stmts->Js.Array2.reduce(
            (cnt,stmt) => cnt + if (stmt.isProved) {0} else {1},
            0
        ),
    }
}

@react.component
let make = (
    ~modalRef:modalRef,
    ~preCtxVer: int,
    ~preCtx: mmContext,
    ~wrkCtx: mmContext,
    ~framesToSkip:array<string>,
    ~parenStr: string,
    ~varsText: string,
    ~disjText: string,
    ~hyps: array<wrkCtxHyp>,
    ~stmts: array<rootStmt>,
    ~typeToPrefix: Belt_MapString.t<string>,
    ~onCancel:unit=>unit
) => {
    let (state, setState) = React.useState(() => makeInitialState(~wrkCtx, ~stmts))

    Js.Console.log2("state", state)

    let actLabelUpdated = label => {
        setState(setLabel(_,label))
    }

    let actDepthUpdated = depthStr => {
        setState(setDepthStr(_,depthStr->Js.String2.replaceByRe(notDigitPattern, "")))
    }

    let actLengthRestrictUpdated = lengthRestrict => {
        setState(setLengthRestrict(_,lengthRestrict))
    }

    let actSetResults = results => {
        setState(setResults(_, results))
    }

    let rndTitle = () => {
        state.title
    }

    let makeActTerminate = (modalId:modalId):(unit=>unit) => {
        () => {
            MM_wrk_client.terminateWorker()
            closeModal(modalRef, modalId)
        }
    }

    let actOnResultReady = (treeDto) => {
        let newVarTypes = treeDto.newVars->Js_array2.map(([typ, var]) => (var, typ))->Belt_HashMapInt.fromArray
        let stmtToProve = stmts[stmts->Js_array2.length-1]
        let proofNode = switch treeDto.nodes->Js_array2.find(node => node.expr->exprEq(stmtToProve.expr)) {
            | None => raise(MmException({msg:`the proof tree DTO doesn't contain the node to prove.`}))
            | Some(node) => node
        }

        switch proofNode.parents {
            | None => actSetResults(Some([]))
            | Some(parents) => {
                let results = parents
                    ->Js_array2.map(src => srcToNewStmts(
                        ~rootStmts=stmts,
                        ~src, 
                        ~tree = treeDto, 
                        ~newVarTypes,
                        ~wrkCtx,
                        ~typeToPrefix,
                        ~getFrame = label => wrkCtx->getFrame(label)->Belt_Option.getExn,
                    ))
                    ->Js.Array2.filter(Belt_Option.isSome)
                    ->Js.Array2.map(Belt_Option.getExn)
                actSetResults(Some(results))
            }
        }
    }

    let actProve = () => {
        setState(st => {
            let depthStr = st.depthStr->Js_string2.trim
            let st = if (depthStr->Js_string2.length == 0) {
                {...st, depth:1, depthStr:"1"}
            } else {
                let depth = Js.Math.max_int(depthStr->Belt_Int.fromString->Belt_Option.getWithDefault(1), 1)
                let depthStr = depth->Belt_Int.toString
                {...st, depth, depthStr}
            }

            openModal(modalRef, () => rndProgress(~text="Proving bottom-up", ~pct=0., ()))->promiseMap(modalId => {
                updateModal( 
                    modalRef, modalId, () => rndProgress(
                        ~text="Proving bottom-up", ~pct=0., ~onTerminate=makeActTerminate(modalId), ()
                    )
                )
                unify(
                    ~preCtxVer, ~preCtx, ~parenStr, ~varsText, ~disjText, ~hyps, ~stmts, ~framesToSkip,
                    ~bottomUpProverParams=Some({
                        asrtLabel: st.label,
                        maxSearchDepth: st.depth,
                        lengthRestriction: st.lengthRestrict,
                    }),
                    ~onProgress = pct => updateModal( 
                        modalRef, modalId, () => rndProgress(
                            ~text="Proving bottom-up", ~pct, ~onTerminate=makeActTerminate(modalId), ()
                        )
                    )
                )->promiseMap(proofTreeDto => {
                    closeModal(modalRef, modalId)
                    Js.Console.log2("proofTreeDto", proofTreeDto)
                    actOnResultReady(proofTreeDto)
                })
            })->ignore

            st
        })
    }

    let rndLengthRestrictSelector = (value:lengthRestrict) => {
        <FormControl size=#small>
            <InputLabel id="length-restrict-select-label">"Length resctriction"</InputLabel>
            <Select
                sx={"width": 130}
                labelId="length-restrict-select-label"
                value={lengthRestrictToStr(value)}
                label="Length restriction"
                onChange=evt2str(str => actLengthRestrictUpdated(lengthRestrictFromStr(str)))
            >
                <MenuItem value="No">{React.string("No")}</MenuItem>
                <MenuItem value="LessEq">{React.string("LessEq")}</MenuItem>
                <MenuItem value="Less">{React.string("Less")}</MenuItem>
            </Select>
        </FormControl>
    }

    let rndParams = () => {
        <Row>
            <AutocompleteVirtualized value=state.label options=state.availableLabels size=#small width=200
                onChange=actLabelUpdated
            />
            <TextField 
                label="Search depth"
                size=#small
                style=ReactDOM.Style.make(~width="100px", ())
                autoFocus=true
                value=state.depthStr
                onChange=evt2str(actDepthUpdated)
            />
            {rndLengthRestrictSelector(state.lengthRestrict)}
            <Button onClick={_=>actProve()} variant=#contained>
                {React.string("Prove")}
            </Button>
            <Button onClick={_=>onCancel()}> {React.string("Cancel")} </Button>
        </Row>
    }

    <Paper style=ReactDOM.Style.make(~padding="10px", ())>
        <Col spacing=1.5>
            {rndTitle()}
            {rndParams()}
        </Col>
    </Paper>
}