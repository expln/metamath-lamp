open MM_context
open Common

type state = {
    frmCtx:mmContext,
    symColors:Belt_HashMapString.t<string>,
    eHyps:array<expr>,
    asrt:expr,
    symRename:option<Belt_HashMapString.t<string>>,
    descrIsExpanded:bool,
    disj: option<array<array<(string,option<string>)>>>,
}

let createDisjGroups = (
    ~disj:Belt_MapInt.t<Belt_SetInt.t>,
    ~intToSym: int => (string,option<string>),
    ~intToTyp: int => option<int>,
    ~typeOrder:Belt_HashMapInt.t<int>,
):array<array<(string,option<string>)>> => {
    let disjMut = disjMake()
    disj->Belt_MapInt.forEach((n,ms) => {
        ms->Belt_SetInt.forEach(m => {
            disjMut->disjAddPair(n,m)
        })
    })
    let resArr = []
    disjMut->disjForEachArr(
        ~sortByTypeAndName=true,
        ~varIntToVarName = i => {
            let (sym,_) = intToSym(i)
            Some(sym)
        },
        ~varIntToVarType=intToTyp,
        ~typeOrder,
        grp => {
            resArr->Js.Array2.push( grp->Js_array2.map(intToSym) )->ignore
        }
    )
    resArr
}

let makeInitialState = (
    ~preCtx:mmContext, 
    ~frame:frame, 
    ~typeColors:Belt_HashMapString.t<string>,
    ~typeOrderInDisj:Belt_HashMapInt.t<int>,
):state => {
    let frmCtx = createContext(~parent=preCtx, ())
    let symRename = ref(None)
    let frmVarIntToCtxInt = []
    let symColors = Belt_HashMapString.make(~hintSize=frame.numOfVars)

    let createLocalCtxVar = (~frmVarName, ~typInt):int => {
        @warning("-8")
        let [ctxVarName] = generateNewVarNames( 
            ~ctx=frmCtx, 
            ~types=[typInt], 
            ~typeToPrefix=Belt_MapString.empty, 
            ()
        )
        @warning("-8")
        let [ctxVarLabel] = generateNewLabels(
            ~ctx=frmCtx, 
            ~prefix="loc-var-", 
            ~amount=1,
            ()
        )
        frmCtx->applySingleStmt(Var({symbols:[ctxVarName]}), ())
        frmCtx->applySingleStmt(Floating({label:ctxVarLabel, expr:[frmCtx->ctxIntToSymExn(typInt), ctxVarName]}), ())
        switch symRename.contents {
            | None => {
                let map = Belt_HashMapString.make(~hintSize=frame.numOfVars)
                map->Belt_HashMapString.set(ctxVarName, frmVarName)
                symRename := Some(map)
            }
            | Some(symRename) => {
                symRename->Belt_HashMapString.set(ctxVarName, frmVarName)
            }
        }
        frmCtx->ctxSymToIntExn(ctxVarName)
    }

    for frmVarInt in 0 to frame.numOfVars-1 {
        let frmVarName = frame.frameVarToSymb->Array.getUnsafe(frmVarInt)
        let ctxVarName = switch frmCtx->getTokenType(frmVarName) {
            | Some(V) => {
                let ctxVarInt = frmCtx->ctxSymToIntExn(frmVarName)
                let ctxVarTypInt = frmCtx->getTypeOfVarExn(ctxVarInt)
                if (frame.varTypes->Array.getUnsafe(frmVarInt) == ctxVarTypInt) {
                    frmVarIntToCtxInt->Js.Array2.push(ctxVarInt)->ignore
                    frmVarName
                } else {
                    let ctxNewVarInt = createLocalCtxVar(~frmVarName, ~typInt=frame.varTypes->Array.getUnsafe(frmVarInt))
                    frmVarIntToCtxInt->Js.Array2.push(ctxNewVarInt)->ignore
                    frmCtx->ctxIntToSymExn(ctxNewVarInt)
                }
            }
            | _ => {
                let ctxNewVarInt = createLocalCtxVar(~frmVarName, ~typInt=frame.varTypes->Array.getUnsafe(frmVarInt))
                frmVarIntToCtxInt->Js.Array2.push(ctxNewVarInt)->ignore
                frmCtx->ctxIntToSymExn(ctxNewVarInt)
            }
        }
        let frmVarTypSym = frmCtx->ctxIntToSymExn(frame.varTypes->Array.getUnsafe(frmVarInt))
        typeColors->Belt_HashMapString.get(frmVarTypSym)->Belt.Option.forEach(color => {
            symColors->Belt_HashMapString.set(ctxVarName, color)
        })
    }

    let frameIntToCtxInt = i => if (i < 0) {i} else {frmVarIntToCtxInt->Array.getUnsafe(i)}

    let frameExprToCtxExpr = (frmExpr:expr):expr => {
        frmExpr->Js_array2.map(frameIntToCtxInt)
    }

    let disj = if (frame.disj->Belt_MapInt.size > 0) {
        Some(
            createDisjGroups(
                ~disj = frame.disj,
                ~intToSym = i => {
                    let sym = frmCtx->ctxIntToSymExn(frameIntToCtxInt(i))
                    (
                        sym,
                        symColors->Belt_HashMapString.get(sym)
                    )
                },
                ~intToTyp = i => frmCtx->getTypeOfVar(frameIntToCtxInt(i)),
                ~typeOrder = typeOrderInDisj,
            )
        )
    } else {
        None
    }

    {
        frmCtx,
        symColors,
        eHyps:frame.hyps->Js.Array2.filter(hyp => hyp.typ == E)->Js.Array2.map(hyp => hyp.expr->frameExprToCtxExpr),
        asrt:frame.asrt->frameExprToCtxExpr,
        symRename:symRename.contents,
        disj,
        descrIsExpanded:false
    }
}

let toggleDescrIsExpanded = st => {
    {...st, descrIsExpanded: !st.descrIsExpanded}
}

let rndDisjGrp = (grp:array<(string,option<string>)>):React.element => {
    let res = []
    for i in 0 to grp->Js.Array2.length-1 {
        if (i > 0) {
            res->Js.Array2.push(
                <span
                    key={"s-" ++ i->Belt_Int.toString}
                    style=ReactDOM.Style.make(
                        ~color="black",
                        ~fontFamily="monospace",
                        ~fontSize="1.3em",
                        ~fontWeight="normal",
                        ()
                    )
                >
                    {" "->React.string}
                </span>
            )->ignore
        }
        let (sym,colorOpt) = grp->Array.getUnsafe(i)
        res->Js.Array2.push(
            <span
                key={"v-" ++ i->Belt_Int.toString}
                style=ReactDOM.Style.make(
                    ~color=?colorOpt,
                    ~fontFamily="monospace",
                    ~fontSize="1.3em",
                    ~fontWeight="bold",
                    ()
                )
            >
                {sym->React.string}
            </span>
        )->ignore
    }
    res->React.array
}

let disjGrpDelim = nbsp ++ nbsp ++ nbsp ++ nbsp

let rndDisj = (disj:array<array<(string,option<string>)>>):React.element => {
    let disjGrpArr = []
    for i in 0 to disj->Js.Array2.length-1 {
        let grp = disj->Array.getUnsafe(i)
        if (i > 0) {
            disjGrpArr->Js.Array2.push(
                <span 
                    key={"s-" ++ i->Belt_Int.toString} 
                    style=ReactDOM.Style.make(~minWidth="20px", ~display="inline-block", ()) 
                />
            )->ignore
        }
        disjGrpArr->Js.Array2.push(
            <span 
                key={"g-" ++ i->Belt_Int.toString} 
                style=ReactDOM.Style.make(
                    ~border="1px solid grey",
                    ~borderRadius="5px",
                    ~paddingLeft="3px", ~paddingRight="3px",
                    ()
                )
            >
                {rndDisjGrp(grp)}
            </span>
        )->ignore
    }
    disjGrpArr->React.array
}