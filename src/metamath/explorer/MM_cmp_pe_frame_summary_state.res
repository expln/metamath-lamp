open MM_syntax_tree
open MM_wrk_editor
open MM_context
open MM_substitution
open MM_parenCounter
open MM_proof_tree
open MM_proof_tree_dto
open Common
open MM_cmp_user_stmt

type state = {
    frmCtx:mmContext,
    symColors:Belt_HashMapString.t<string>,
    eHyps:array<expr>,
    asrt:expr,
    symRename:option<Belt_HashMapString.t<string>>,
    descrIsExpanded:bool,
    disj: option<array<array<(string,option<string>)>>>,
}

let makeInitialState = (~preCtx:mmContext, ~frame:frame, ~typeColors:Belt_HashMapString.t<string>):state => {
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
        frmCtx->applySingleStmt(Var({symbols:[ctxVarName]}))
        frmCtx->applySingleStmt(Floating({label:ctxVarLabel, expr:[frmCtx->ctxIntToSymExn(typInt), ctxVarName]}))
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
        let frmVarName = frame.frameVarToSymb[frmVarInt]
        let ctxVarName = switch frmCtx->getTokenType(frmVarName) {
            | Some(V) => {
                let ctxVarInt = frmCtx->ctxSymToIntExn(frmVarName)
                let ctxVarTypInt = frmCtx->getTypeOfVarExn(ctxVarInt)
                if (frame.varTypes[frmVarInt] == ctxVarTypInt) {
                    frmVarIntToCtxInt->Js.Array2.push(ctxVarInt)->ignore
                    frmVarName
                } else {
                    let ctxNewVarInt = createLocalCtxVar(~frmVarName, ~typInt=frame.varTypes[frmVarInt])
                    frmVarIntToCtxInt->Js.Array2.push(ctxNewVarInt)->ignore
                    frmCtx->ctxIntToSymExn(ctxNewVarInt)
                }
            }
            | _ => {
                let ctxNewVarInt = createLocalCtxVar(~frmVarName, ~typInt=frame.varTypes[frmVarInt])
                frmVarIntToCtxInt->Js.Array2.push(ctxNewVarInt)->ignore
                frmCtx->ctxIntToSymExn(ctxNewVarInt)
            }
        }
        let frmVarTypSym = frmCtx->ctxIntToSymExn(frame.varTypes[frmVarInt])
        typeColors->Belt_HashMapString.get(frmVarTypSym)->Belt.Option.forEach(color => {
            symColors->Belt_HashMapString.set(ctxVarName, color)
        })
    }

    let frameExprToCtxExpr = (frmExpr:expr):expr => {
        frmExpr->Js_array2.map(i => if (i < 0) {i} else {frmVarIntToCtxInt[i]})
    }

    let disj = if (frame.disj->Belt_MapInt.size > 0) {
        let disjMut = disjMake()
        frame.disj->Belt_MapInt.forEach((n,ms) => {
            ms->Belt_SetInt.forEach(m => {
                disjMut->disjAddPair(n,m)
            })
        })
        let resArr = []
        disjMut->disjForEachArr(grp => {
            resArr->Js.Array2.push(
                grp->frameExprToCtxExpr->Js_array2.map(i => {
                    let sym = frmCtx->ctxIntToSymExn(i)
                    (
                        sym,
                        symColors->Belt_HashMapString.get(sym)
                    )
                })
            )->ignore
        })
        Some(resArr)
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