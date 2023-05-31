open Expln_utils_common

let {log,log2} = module(Js.Console)

type selectExpr<'a,'b> = 'a=>(string,'b)
type selectStage<'a,'b> = {
    selectors: array<selectExpr<'a,'b>>,
    childRef: option<'a=>option<array<'a>>>,
}

let applySingleSelect:('a,selectExpr<'a,'b>) => Js.Dict.t<'b> = 
(obj, sel) => Js_dict.fromArray([sel(obj)])

let mergeRows:(Js_dict.t<'b>,Js_dict.t<'b>) => Js_dict.t<'b> = (r1,r2) => {
    let result = r1->Js_dict.entries->Js_dict.fromArray
    r2->Js_dict.entries->Belt_Array.forEach(((k,v)) => result->Js_dict.set(k,v))
    result
}

let select: (array<'a>, array<selectExpr<'a,'b>>) => array<Js_dict.t<'b>> = 
    (objects,selectors) => {
        objects->Js.Array2.map( o=>
                        selectors->Js.Array2.map(applySingleSelect(o,_))
            ->Js.Array2.reduce(mergeRows, Js.Dict.empty())

        )
    }

let objToTableWithChildren:('a,array<selectStage<'a,'b>>) => array<(Js_dict.t<'b>,option<'a>)> = (json, selectStages) => {
    selectStages->Belt.Array.reduceWithIndex(
        [(Js_dict.empty(),Some(json))],
        (acc, stage, idx) => {
            acc->arrFlatMap( ( (row,jsObjOpt) ) => switch jsObjOpt {
                | None => [(row,jsObjOpt)]
                | Some(jsObj) =>
                    let newRow = select([jsObj], stage.selectors)
                        ->Belt_Array.getExn(0)
                        ->mergeRows(row)
                    switch stage.childRef {
                        | Some(func) => switch func(jsObj) {
                                            | Some([]) | None => [(newRow, None)]
                                            | Some(arr) => arr->Belt_Array.map(ch=>(newRow,Some(ch)))
                                        }
                        | None => 
                            if (idx < Belt_Array.length(selectStages)-1) {
                                exn("Intermediate selector stage must have children ref.")
                            } else {
                                [(newRow, None)]
                            }
                    }
            })
        }
    ) 
}

let objToTable = (json, selectStages) => 
    objToTableWithChildren(json, selectStages) 
    -> Js.Array2.map(( (dictjs,_) ) => dictjs)

type nodeToProcess<'n> = {
    node: 'n,
    nodesToPostProcess: option<array<'n>>
}

let traverseTree = (
    context:'c,
    root:'n,
    getChildren: ('c,'n)=>option<array<'n>>,
    ~preProcess:option<('c,'n)=>option<'r>>=?,
    ~process:option<('c,'n)=>option<'r>>=?,
    ~postProcess:option<('c,'n)=>option<'r>>=?,
    ()
): ('c, option<'r>) => {
    let hasPreProcess = preProcess->Belt_Option.isSome
    let hasProcess = process->Belt_Option.isSome
    let hasPostProcess = postProcess->Belt_Option.isSome
    let nodesToProcess = Belt_MutableStack.make()
    let res = ref(None)
    nodesToProcess->Belt_MutableStack.push({node:root, nodesToPostProcess: if hasPostProcess {Some([root])} else {None}})
    while (!(nodesToProcess->Belt_MutableStack.isEmpty) && res.contents->Belt_Option.isNone) {
        switch nodesToProcess->Belt_MutableStack.pop {
            | None => ()
            | Some(currNode) => {
                if (hasPreProcess) {
                    res.contents = preProcess->Belt_Option.flatMap(f=>f(context, currNode.node))
                }
                if (hasProcess && res.contents->Belt_Option.isNone) {
                    res.contents = process->Belt_Option.flatMap(f=>f(context, currNode.node))
                }
                if (res.contents->Belt_Option.isNone) {
                    switch getChildren(context, currNode.node) {
                        | None | Some([]) => {
                            if (hasPostProcess) {
                                switch currNode.nodesToPostProcess {
                                    | None =>
                                        raise(ExplnUtilsException({
                                            msg:"this case is not possible, because each node has itself in its nodesToPostProcess (1)"
                                        }))
                                    | Some(nodes) => {
                                        let i = ref(nodes->Js_array2.length - 1)
                                        while (i.contents >= 0 && res.contents->Belt_Option.isNone) {
                                            //TODO check how this gets converted to js
                                            res.contents = postProcess->Belt_Option.flatMap(f => f(context, nodes[i.contents]))
                                            i.contents = i.contents - 1
                                        }
                                    }
                                }
                            }
                        }
                        | Some(children) => {
                            let maxChildIdx = children->Js_array2.length - 1
                            for i in maxChildIdx downto 0 {
                                nodesToProcess->Belt_MutableStack.push({
                                    node:children[i],
                                    nodesToPostProcess:
                                        if (hasPostProcess) {
                                            if (i == maxChildIdx) {
                                                switch currNode.nodesToPostProcess {
                                                    | Some(nodes) => {
                                                        nodes->Js_array2.push(children[i])->ignore
                                                        Some(nodes)
                                                    }
                                                    | _ => 
                                                        raise(ExplnUtilsException({
                                                            msg:"this case is not possible, because each node has itself in its nodesToPostProcess (2)"
                                                        }))
                                                }
                                            } else {
                                                Some([children[i]])
                                            }
                                        } else {
                                            None
                                        }
                                })
                            }
                        }
                    }
                }
            }
        }
    }
    (context, res.contents)
}

