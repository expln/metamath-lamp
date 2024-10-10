type rec xmlNode =
    | Node({ name:string, attrs:Belt_MapString.t<string>, children: array<xmlNode>, })
    | Text(string)

type attr = {
    name: string,
    value: string,
}

type namedNodeMap = {
    length: int,
    item: (. int) => Js.Nullable.t<attr>
}

type rec element = {
    tagName: string,
    attributes: option<namedNodeMap>,
    childNodes: nodeList,
    nodeType: int,
    nodeValue: string,
} and nodeList = {
    length: int,
    item: (. int) => Js.Nullable.t<element>
}

type xmlDocument = { 
    documentElement: element
}

@new external newDomParser: unit => {..} = "DOMParser"

let parseXmlFromStr = (str:string):xmlDocument => {
    newDomParser()["parseFromString"](. str, "text/xml")
}

let elementGetAttrs = (element:element):Belt_MapString.t<string> => {
    let res = []
    switch element.attributes {
        | None => ()
        | Some(attrs) => {
            for i in 0 to attrs.length-1 {
                switch attrs.item(. i)->Js.Nullable.toOption {
                    | None => ()
                    | Some(attr) => {
                        res->Array.push((attr.name,attr.value))
                    }
                }
            }
        }
    }
    res->Belt_MapString.fromArray
}

let nodeTypeElement = 1
let nodeTypeText = 3

let parseStrExn = (str:string):xmlNode => {
    let doc = parseXmlFromStr(str)
    let root = doc.documentElement

    let (_, rootXmlNodeOpt) = Expln_utils_data.traverseTree(
        Belt_MutableStack.make(),
        root,
        (_,node) => {
            if (node.nodeType == nodeTypeElement) {
                let children = []
                let cNodes = node.childNodes
                for i in 0 to cNodes.length-1 {
                    switch cNodes.item(. i)->Js.Nullable.toOption {
                        | None => ()
                        | Some(cNode) => {
                            if (cNode.nodeType == nodeTypeElement || cNode.nodeType == nodeTypeText) {
                                children->Array.push(cNode)
                            }
                        }
                    }
                }
                Some(children)
            } else {
                None
            }
        },
        ~process = (parents,node) => {
            let xmlNode = 
                if (node.nodeType == nodeTypeElement) {
                    Node({
                        name:node.tagName,
                        attrs:node->elementGetAttrs,
                        children: [],
                    })
                } else if (node.nodeType == nodeTypeText) {
                    Text(node.nodeValue)
                } else {
                    Exn.raiseError(`Node type of ${node.nodeType->Belt.Int.toString} is not expected here.`)
                }
            switch parents->Belt_MutableStack.top {
                | None => ()
                | Some(Node({children})) => children->Array.push(xmlNode)
                | Some(Text(_)) => ()
            }
            parents->Belt_MutableStack.push(xmlNode)
            None
        },
        ~postProcess = (parents, _) => {
            if (parents->Belt_MutableStack.size == 1) {
                parents->Belt_MutableStack.pop
            } else {
                (parents->Belt_MutableStack.pop)->ignore
                None
            }
        },
        ()
    )

    switch rootXmlNodeOpt {
        | None => Exn.raiseError("Got None as a result of parsing.")
        | Some(xmlNode) => xmlNode
    }
}



let parseStr = (str:string):result<xmlNode,string> => {
    let exToError = (ex:option<Exn.t>):result<xmlNode,string> => {
        let msg = switch ex {
            | None => "no error message"
            | Some(ex) => {
                switch Exn.message(ex) {
                    | None => "no error message"
                    | Some(msg) => msg
                }
            }
        }
        Error(msg)
    }

    try {
        Ok(parseStrExn(str))
    } catch {
        | Exn.Error(ex) => exToError(Some(ex))
        | reEx => exToError(reEx->Exn.asJsExn)
    }
}