open Expln_React_Mui
open Expln_React_common

let xmlToReactElem = (xml:Xml_parser.xmlNode):reElem => {
    let saveChild = (childrenStack:Belt_MutableStack.t<array<reElem>>, child:reElem):unit => {
        switch childrenStack->Belt_MutableStack.top {
            | None => Js.Exn.raiseError("childrenStack->Belt_MutableStack.top is None")
            | Some(children) => children->Js_array2.push(child)->ignore
        }
    }

    let (_, reElemOpt) = Expln_utils_data.traverseTree(
        Belt_MutableStack.make(),
        xml,
        (_,node) => {
            switch node {
                | Node({children}) => Some(children)
                | Text(_) => None
            }
        },
        ~process = (childrenStack, node) => {
            switch node {
                | Node(_) => childrenStack->Belt_MutableStack.push([])
                | Text(_) => ()
            }
            None
        },
        ~postProcess = (childrenStack,node) => {
            let thisElem = switch node {
                | Text(str) => str->React.string
                | Node({name, attrs, children}) => {
                    let children = switch childrenStack->Belt_MutableStack.pop {
                        | None => []
                        | Some(children) => children
                    }
                    ReactDOMRe.createDOMElementVariadic(name, children)
                }
            }
            if (childrenStack->Belt_MutableStack.size == 0) {
                Some(thisElem)
            } else {
                childrenStack->saveChild(thisElem)
                None
            }
        },
        ()
    )

    switch reElemOpt{
        | None => Js.Exn.raiseError("Got None as a result of convertion from XML to React Elem.")
        | Some(reElem) => reElem
    }
}

// let xmlToReactElem = (xml:Xml_parser.xmlNode, errorTitle:string):reElem => {
//     switch xmlStr->Xml_parser.parseStr {
//         | Error(msg) => {
//             <Col>
//                 <Row>
//                     {React.string(errorTitle)}
//                     <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(msg)}</pre>
//                 </Row>
//                 {React.string("Showing raw content:")}
//                 <pre>{React.string(xmlStr)}</pre>
//             </Col>
//         }
//         | Ok(rootNode) => {
//             let saveChild = (childrenStack:Belt_MutableStack.t<array<reElem>>, child:reElem):unit => {
//                 switch childrenStack->Belt_MutableStack.top {
//                     | None => Js.Exn.raiseError("childrenStack->Belt_MutableStack.top is None")
//                     | Some(children) => children->Js_array2.push(child)->ignore
//                 }
//             }

//             Expln_utils_data.traverseTree(
//                 Belt_MutableStack.make(),
//                 rootNode,
//                 (_,node) => {
//                     switch node {
//                         | Node({children}) => Some(children)
//                         | Text => None
//                     }
//                 },
//                 ~process = (childrenStack, node) => {
//                     switch node {
//                         | Node({children}) => childrenStack->Belt_MutableStack.push([])
//                         | Text => ()
//                     }
                    
//                 },
//                 ~postProcess = (childrenStack,node) => {
//                     switch node {
//                         | Text(str) => childrenStack->saveChild(str->React.string)
//                         | Node({name, attrs, children}) => {
//                             let children = childrenStack->Belt_MutableStack
//                         }
//                     }
//                 }
//             )
//         }
//     }
// }