open Expln_utils_common

let {log,log2} = module(Console)

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
    ~postProcess:option<('c,'n)=>option<'r>>=?
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
                                        let i = ref(nodes->Array.length - 1)
                                        while (i.contents >= 0 && res.contents->Belt_Option.isNone) {
                                            //TODO check how this gets converted to js
                                            res.contents = postProcess->Belt_Option.flatMap(f => f(context, nodes->Array.getUnsafe(i.contents)))
                                            i.contents = i.contents - 1
                                        }
                                    }
                                }
                            }
                        }
                        | Some(children) => {
                            let maxChildIdx = children->Array.length - 1
                            for i in maxChildIdx downto 0 {
                                nodesToProcess->Belt_MutableStack.push({
                                    node:children->Array.getUnsafe(i),
                                    nodesToPostProcess:
                                        if (hasPostProcess) {
                                            if (i == maxChildIdx) {
                                                switch currNode.nodesToPostProcess {
                                                    | Some(nodes) => {
                                                        nodes->Array.push(children->Array.getUnsafe(i))
                                                        Some(nodes)
                                                    }
                                                    | _ => 
                                                        raise(ExplnUtilsException({
                                                            msg:"this case is not possible, because each node has itself in its nodesToPostProcess (2)"
                                                        }))
                                                }
                                            } else {
                                                Some([children->Array.getUnsafe(i)])
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

